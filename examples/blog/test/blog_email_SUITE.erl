-module(blog_email_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    builder_normalises_addresses/1,
    mailer_requires_sender_and_recipient/1,
    test_adapter_sends_to_a_process/1,
    logger_adapter_logs_the_email/1,
    comment_email_escapes_html/1,
    export_email_attaches_csv/1,
    smtp_attachment_keeps_its_filename/1
]).

all() ->
    [
        builder_normalises_addresses,
        mailer_requires_sender_and_recipient,
        test_adapter_sends_to_a_process,
        logger_adapter_logs_the_email,
        comment_email_escapes_html,
        export_email_attaches_csv,
        smtp_attachment_keeps_its_filename
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_jobs_fixtures:testing(manual),
    blog_jobs_fixtures:capture_mail(),
    blog_data_fixtures:checkout(),
    Config.

end_per_testcase(_Case, _Config) ->
    blog_jobs_fixtures:testing(disabled),
    blog_data_fixtures:checkin().

builder_normalises_addresses(_Config) ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, {~"Nova Blog", ~"noreply@blog.test"}),
    E2 = hikyaku_email:to(E1, ~"alice@example.com"),
    E3 = hikyaku_email:cc(E2, {~"Bob", ~"bob@example.com"}),
    E4 = hikyaku_email:subject(E3, ~"Welcome!"),
    #hikyaku_email{
        from = #hikyaku_address{name = ~"Nova Blog", address = ~"noreply@blog.test"},
        to = [#hikyaku_address{name = undefined, address = ~"alice@example.com"}],
        cc = [#hikyaku_address{name = ~"Bob"}],
        subject = ~"Welcome!",
        text_body = undefined
    } = E4.

mailer_requires_sender_and_recipient(_Config) ->
    NoSender = hikyaku_email:to(hikyaku_email:new(), ~"alice@example.com"),
    {error, {validation, ~"from is required"}} = blog_mailer:deliver(NoSender),
    NoRecipient = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    {error, {validation, ~"at least one recipient (to, cc, or bcc) is required"}} =
        blog_mailer:deliver(NoRecipient).

test_adapter_sends_to_a_process(_Config) ->
    E0 = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    E1 = hikyaku_email:to(E0, ~"alice@example.com"),
    Email = hikyaku_email:text_body(hikyaku_email:subject(E1, ~"Hi"), ~"Hello Alice"),
    {ok, #{status := sent}} = blog_mailer:deliver(Email),
    Email = blog_jobs_fixtures:receive_email().

logger_adapter_logs_the_email(_Config) ->
    blog_jobs_fixtures:mailer(#{adapter => hikyaku_adapter_logger}),
    E0 = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    Email = hikyaku_email:to(E0, ~"alice@example.com"),
    {ok, #{status := logged}} = blog_mailer:deliver(Email).

comment_email_escapes_html(_Config) ->
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Tom & Jerry"}),
    {ok, Comment} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"<script>alert(1)</script>"}),
    Loaded = blog_repo:preload(blog_comment, Comment, [author, {post, [author]}]),
    #hikyaku_email{subject = Subject, text_body = Text, html_body = Html} =
        blog_emails:comment_notification(Loaded),
    ~"bob commented on \"Tom & Jerry\"" = Subject,
    {_, _} = binary:match(Text, ~"<script>alert(1)</script>"),
    {_, _} = binary:match(Html, ~"&lt;script&gt;alert(1)&lt;/script&gt;"),
    {_, _} = binary:match(Html, ~"<em>Tom &amp; Jerry</em>"),
    nomatch = binary:match(Html, ~"<script>").

export_email_attaches_csv(_Config) ->
    #{id := AliceId} = Alice = blog_data_fixtures:user(~"alice"),
    #{id := PostId} = blog_data_fixtures:post(Alice, #{~"title" => ~"Say \"hello\", world"}),
    ok = shigoto:perform_job(blog_export_worker, #{~"user_id" => AliceId}),
    #hikyaku_email{
        subject = ~"Your posts export",
        attachments = [
            #hikyaku_attachment{
                filename = ~"posts.csv", content_type = ~"text/csv", data = Csv
            }
        ]
    } = blog_jobs_fixtures:receive_email(),
    [~"id,title,status,inserted_at", Row, <<>>] = binary:split(Csv, ~"\r\n", [global]),
    Expected = <<(integer_to_binary(PostId))/binary, ",\"Say \"\"hello\"\", world\",draft,">>,
    {0, _} = binary:match(Row, Expected).

smtp_attachment_keeps_its_filename(_Config) ->
    E0 = hikyaku_email:from(hikyaku_email:new(), {~"Nova Blog", ~"noreply@blog.test"}),
    E1 = hikyaku_email:to(E0, ~"alice@example.com"),
    E2 = hikyaku_email:text_body(hikyaku_email:subject(E1, ~"Export"), ~"Attached."),
    Email = hikyaku_email:attachment(
        E2, hikyaku_attachment:from_data(~"posts.csv", ~"id\r\n1\r\n")
    ),
    Message = mimemail:encode(hikyaku_adapter_smtp:build_mimemail(Email)),
    {_, _} = binary:match(Message, ~"Content-Type: multipart/mixed"),
    {_, _} = binary:match(Message, ~"From: Nova Blog <noreply@blog.test>"),
    {_, _} = binary:match(Message, ~"Content-Type: text/csv"),
    {_, _} = binary:match(Message, ~"Content-Disposition: attachment"),
    {_, _} = binary:match(Message, ~"filename=posts.csv"),
    {_, _} = binary:match(Message, ~"Content-Transfer-Encoding: base64").

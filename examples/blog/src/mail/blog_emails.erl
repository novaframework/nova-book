-module(blog_emails).

-export([comment_notification/1, posts_export/2, confirmation/2, password_reset/2]).

comment_notification(#{post := Post, author := Commenter, body := Body}) ->
    #{title := Title, author := #{email := To, username := AuthorName}} = Post,
    #{username := CommenterName} = Commenter,
    Vars = #{
        author => AuthorName,
        commenter => CommenterName,
        title => Title,
        comment => Body,
        url => post_url(Post)
    },
    Email = new(To, <<CommenterName/binary, " commented on \"", Title/binary, "\"">>),
    with_bodies(Email, blog_email_comment_txt_dtl, blog_email_comment_html_dtl, Vars).

posts_export(#{email := To, username := Username}, Csv) ->
    E0 = new(To, ~"Your posts export"),
    E1 = hikyaku_email:text_body(
        E0, <<"Hi ", Username/binary, ",\r\n\r\nYour posts are attached as a CSV file.\r\n">>
    ),
    hikyaku_email:attachment(E1, hikyaku_attachment:from_data(~"posts.csv", Csv)).

confirmation(#{email := To, username := Username}, Token) ->
    Vars = #{
        username => Username,
        url => account_link(~"/confirm", Token),
        days => nova_auth:config(blog_auth, confirm_validity_days)
    },
    Email = new(To, ~"Confirm your email address"),
    with_bodies(Email, blog_email_confirm_txt_dtl, blog_email_confirm_html_dtl, Vars).

password_reset(#{email := To, username := Username}, Token) ->
    Vars = #{
        username => Username,
        url => account_link(~"/password/reset", Token),
        hours => nova_auth:config(blog_auth, reset_validity_hours)
    },
    Email = new(To, ~"Reset your password"),
    with_bodies(Email, blog_email_reset_txt_dtl, blog_email_reset_html_dtl, Vars).

new(To, Subject) ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, application:get_env(blog, mail_from, ~"noreply@blog.test")),
    E2 = hikyaku_email:to(E1, To),
    hikyaku_email:subject(E2, Subject).

with_bodies(Email, TextTemplate, HtmlTemplate, Vars) ->
    E1 = hikyaku_email:text_body(Email, render(TextTemplate, Vars)),
    hikyaku_email:html_body(E1, render(HtmlTemplate, Vars)).

render(Template, Vars) ->
    {ok, IoData} = Template:render(Vars),
    iolist_to_binary(IoData).

post_url(#{id := Id}) ->
    <<(base_url())/binary, "/api/posts/", (integer_to_binary(Id))/binary>>.

account_link(Path, Token) ->
    Query = uri_string:compose_query([{~"token", Token}]),
    <<(base_url())/binary, Path/binary, "?", Query/binary>>.

base_url() ->
    application:get_env(blog, base_url, ~"http://localhost:8080").

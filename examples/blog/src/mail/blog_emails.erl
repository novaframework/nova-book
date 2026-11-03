-module(blog_emails).

-export([comment_notification/1, posts_export/2]).

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
    BaseUrl = application:get_env(blog, base_url, ~"http://localhost:8080"),
    <<BaseUrl/binary, "/api/posts/", (integer_to_binary(Id))/binary>>.

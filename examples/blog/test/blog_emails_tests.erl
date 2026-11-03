-module(blog_emails_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("hikyaku/include/hikyaku.hrl").

comment_notification_test() ->
    Comment = #{
        body => ~"<script>alert(1)</script>",
        author => #{username => ~"bob"},
        post => #{
            id => 42,
            title => ~"Tom & Jerry",
            author => #{email => ~"alice@example.com", username => ~"alice"}
        }
    },
    #hikyaku_email{
        to = [#hikyaku_address{address = ~"alice@example.com"}],
        subject = Subject,
        text_body = Text,
        html_body = Html
    } = blog_emails:comment_notification(Comment),
    ?assertEqual(~"bob commented on \"Tom & Jerry\"", Subject),
    ?assertMatch({_, _}, binary:match(Text, ~"http://localhost:8080/api/posts/42")),
    ?assertMatch({_, _}, binary:match(Html, ~"&lt;script&gt;")),
    ?assertEqual(nomatch, binary:match(Html, ~"<script>")).

posts_export_attaches_the_csv_test() ->
    User = #{email => ~"alice@example.com", username => ~"alice"},
    #hikyaku_email{attachments = [Attachment]} =
        blog_emails:posts_export(User, ~"id,title\r\n1,\"Hi\"\r\n"),
    ?assertMatch(
        #hikyaku_attachment{
            filename = ~"posts.csv", content_type = ~"text/csv", disposition = attachment
        },
        Attachment
    ).

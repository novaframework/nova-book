-module(blog_export_worker).
-behaviour(shigoto_worker).

-export([perform/1, queue/0]).

queue() -> ~"mailers".

perform(#{~"user_id" := UserId}) ->
    case blog_repo:get(blog_user, UserId) of
        {ok, User} ->
            Query = kura_query:order_by(
                kura_query:where(kura_query:from(blog_post), {user_id, UserId}), [{id, asc}]
            ),
            {ok, Posts} = blog_repo:all(Query),
            blog_mailer:deliver_from_job(blog_emails:posts_export(User, posts_csv(Posts)));
        {error, not_found} ->
            ok
    end.

posts_csv(Posts) ->
    Header = ~"id,title,status,inserted_at\r\n",
    Rows = [csv_row(Post) || Post <- Posts],
    iolist_to_binary([Header | Rows]).

csv_row(#{id := Id, title := Title, status := Status, inserted_at := InsertedAt}) ->
    Fields = [
        integer_to_binary(Id),
        csv_field(Title),
        atom_to_binary(Status),
        iso8601(InsertedAt)
    ],
    [lists:join(~",", Fields), ~"\r\n"].

csv_field(Value) ->
    <<"\"", (binary:replace(Value, ~"\"", ~"\"\"", [global]))/binary, "\"">>.

iso8601({{Y, Mo, D}, {H, Mi, S}}) ->
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, Mo, D, H, Mi, S])
    ).

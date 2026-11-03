-module(blog_notify).
-export([channel/0, post_channel/1, broadcast/2, broadcast_post/3]).
-export([post_published/1, comment_created/3, comment/2]).

channel() ->
    ~"notifications".

post_channel(PostId) when is_integer(PostId) ->
    <<"post:", (integer_to_binary(PostId))/binary>>.

broadcast(Event, Payload) ->
    nova_pubsub:broadcast(channel(), Event, Payload).

broadcast_post(PostId, Event, Payload) ->
    nova_pubsub:broadcast(post_channel(PostId), Event, Payload).

post_published(#{id := Id, title := Title}) ->
    broadcast(~"post_published", #{~"id" => Id, ~"title" => Title}).

comment_created(#{id := PostId, title := PostTitle}, #{username := Author} = User, Comment) ->
    ok = broadcast_post(PostId, ~"comment_created", comment(Comment, User)),
    broadcast(~"comment_created", #{
        ~"post_id" => PostId,
        ~"post_title" => PostTitle,
        ~"author" => Author
    }).

comment(#{id := Id, body := Body, inserted_at := InsertedAt}, #{username := Author}) ->
    #{id => Id, body => Body, author => Author, inserted_at => blog_api_json:datetime(InsertedAt)}.

-module(blog_api_json).
-include_lib("kura/include/kura.hrl").

-export([post/1, comment/1, user/1, tag/1, errors/1, datetime/1]).

post(Post) ->
    with_loaded(Post, #{
        id => maps:get(id, Post),
        title => maps:get(title, Post),
        body => maps:get(body, Post),
        status => maps:get(status, Post),
        user_id => maps:get(user_id, Post),
        published_at => datetime(maps:get(published_at, Post)),
        metadata => metadata(maps:get(metadata, Post)),
        lock_version => maps:get(lock_version, Post),
        inserted_at => datetime(maps:get(inserted_at, Post))
    }).

comment(Comment) ->
    with_loaded(Comment, #{
        id => maps:get(id, Comment),
        body => maps:get(body, Comment),
        user_id => maps:get(user_id, Comment),
        inserted_at => datetime(maps:get(inserted_at, Comment))
    }).

with_loaded(Record, Json) ->
    Loaded = [
        {Key, loaded(Key, maps:get(Key, Record))}
     || Key <- [author, comments, tags], maps:is_key(Key, Record)
    ],
    maps:merge(Json, maps:from_list(Loaded)).

loaded(author, Author) -> user(Author);
loaded(comments, Comments) -> [comment(C) || C <- Comments];
loaded(tags, Tags) -> lists:sort([maps:get(name, T) || T <- Tags]).

metadata(undefined) ->
    null;
metadata(Metadata) ->
    #{
        Field => null_if_undefined(maps:get(Field, Metadata, undefined))
     || Field <- kura_schema:field_names(blog_post_metadata)
    }.

null_if_undefined(undefined) -> null;
null_if_undefined(Value) -> Value.

user(User) ->
    #{
        id => maps:get(id, User),
        username => maps:get(username, User),
        role => maps:get(role, User)
    }.

tag(Tag) ->
    Json = #{
        id => maps:get(id, Tag),
        name => maps:get(name, Tag),
        inserted_at => datetime(maps:get(inserted_at, Tag))
    },
    case Tag of
        #{posts := Posts} when is_list(Posts) -> Json#{posts => [post(Post) || Post <- Posts]};
        #{} -> Json
    end.

errors(#kura_changeset{errors = Errors}) ->
    Fields = lists:usort([Field || {Field, _} <- Errors]),
    #{Field => [Message || {F, Message} <- Errors, F =:= Field] || Field <- Fields}.

datetime(undefined) ->
    null;
datetime({{_, _, _}, {_, _, _}} = DateTime) ->
    Seconds = erlang:universaltime_to_posixtime(DateTime),
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{offset, "Z"}])).

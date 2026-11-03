-module(blog_json).
-include_lib("kura/include/kura.hrl").

-export([post/1, user/1, errors/1, datetime/1]).

post(Post) ->
    #{
        id => maps:get(id, Post),
        title => maps:get(title, Post),
        body => maps:get(body, Post),
        status => maps:get(status, Post),
        user_id => maps:get(user_id, Post),
        published_at => datetime(maps:get(published_at, Post)),
        inserted_at => datetime(maps:get(inserted_at, Post))
    }.

user(User) ->
    #{
        id => maps:get(id, User),
        username => maps:get(username, User),
        role => maps:get(role, User)
    }.

errors(#kura_changeset{errors = Errors}) ->
    Fields = lists:usort([Field || {Field, _} <- Errors]),
    #{Field => [Message || {F, Message} <- Errors, F =:= Field] || Field <- Fields}.

datetime(undefined) ->
    null;
datetime({{_, _, _}, {_, _, _}} = DateTime) ->
    Seconds = erlang:universaltime_to_posixtime(DateTime),
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{offset, "Z"}])).

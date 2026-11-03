-module(blog_hello_controller).
-fallback_controller(blog_fallback_controller).

-include_lib("kernel/include/logger.hrl").

-export([
    greet/1,
    create/1
]).

greet(#{bindings := #{~"name" := Name}, parsed_qs := Query}) ->
    maybe
        ok ?= validate_name(Name),
        ?LOG_INFO(#{msg => ~"greeting", name => Name}),
        {json, #{greeting => greeting(Name, Query)}}
    end.

create(#{json := #{~"name" := Name}}) ->
    created(Name);
create(#{params := #{~"name" := Name}}) ->
    created(Name);
create(_Req) ->
    {error, missing_name}.

created(Name) ->
    maybe
        ok ?= validate_name(Name),
        {json, 201, #{~"location" => <<"/hello/", Name/binary>>}, #{name => Name}}
    end.

greeting(Name, #{~"shout" := ~"true"}) ->
    string:uppercase(greeting(Name, #{}));
greeting(Name, _Query) ->
    <<"Hello, ", Name/binary, "!">>.

validate_name(Name) when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 32 ->
    case re:run(Name, ~"^[A-Za-z0-9_-]+$") of
        {match, _} -> ok;
        nomatch -> {error, invalid_name}
    end;
validate_name(_Name) ->
    {error, invalid_name}.

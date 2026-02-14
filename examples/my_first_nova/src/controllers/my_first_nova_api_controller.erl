-module(my_first_nova_api_controller).
-include_lib("kura/include/kura.hrl").
-export([
    index/1,
    show/1,
    create/1
]).

index(_Req) ->
    {ok, Users} = my_first_nova_repo:all(kura_query:from(user)),
    {json, #{users => Users}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(user, binary_to_integer(Id)) of
        {ok, User} ->
            {json, User};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{json := Params}) ->
    CS = kura_changeset:cast(user, #{}, Params, [name, email]),
    CS1 = kura_changeset:validate_required(CS, [name, email]),
    case my_first_nova_repo:insert(CS1) of
        {ok, User} ->
            {json, 201, #{}, User};
        {error, Changeset} ->
            {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and email required">>}}.

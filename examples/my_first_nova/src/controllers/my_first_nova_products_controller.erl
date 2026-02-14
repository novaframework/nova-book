-module(my_first_nova_products_controller).
-include_lib("kura/include/kura.hrl").
-export([
    list/1,
    show/1,
    create/1,
    update/1,
    delete/1
]).

list(_Req) ->
    {ok, Products} = my_first_nova_repo:all(kura_query:from(product)),
    {json, #{products => Products}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Product} ->
            {json, Product};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{json := Params}) ->
    CS = kura_changeset:cast(product, #{}, Params, [name, price, description]),
    CS1 = kura_changeset:validate_required(CS, [name, price]),
    case my_first_nova_repo:insert(CS1) of
        {ok, Product} ->
            {json, 201, #{}, Product};
        {error, Changeset} ->
            {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and price required">>}}.

update(#{bindings := #{<<"id">> := Id}, json := Params}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Existing} ->
            CS = kura_changeset:cast(product, Existing, Params, [name, price, description]),
            case my_first_nova_repo:update(CS) of
                {ok, Updated} ->
                    {json, Updated};
                {error, Changeset} ->
                    {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
            end;
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
update(_Req) ->
    {status, 422, #{}, #{error => <<"invalid request">>}}.

delete(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Record} ->
            CS = kura_changeset:cast(product, Record, #{}, []),
            {ok, _} = my_first_nova_repo:delete(CS),
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
delete(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

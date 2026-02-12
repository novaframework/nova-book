-module(my_first_nova_products_controller).
-export([
         list/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

list(_Req) ->
    Products = [
        #{id => 1, name => <<"Widget">>, price => 999, description => <<"A fine widget">>},
        #{id => 2, name => <<"Gadget">>, price => 1999, description => <<"A fancy gadget">>}
    ],
    {json, #{products => Products}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{id => binary_to_integer(Id),
             name => <<"Widget">>,
             price => 999,
             description => <<"A fine widget">>}};
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{params := #{<<"name">> := Name, <<"price">> := Price}} = Req) ->
    Desc = maps:get(<<"description">>, maps:get(params, Req, #{}), <<>>),
    {json, 201, #{}, #{id => 3, name => Name, price => Price, description => Desc}};
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and price required">>}}.

update(#{bindings := #{<<"id">> := Id},
         params := #{<<"name">> := Name, <<"price">> := Price}} = Req) ->
    Desc = maps:get(<<"description">>, maps:get(params, Req, #{}), <<>>),
    {json, #{id => binary_to_integer(Id), name => Name, price => Price, description => Desc}};
update(_Req) ->
    {status, 422, #{}, #{error => <<"name and price required">>}}.

delete(#{bindings := #{<<"id">> := _Id}}) ->
    {status, 204};
delete(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

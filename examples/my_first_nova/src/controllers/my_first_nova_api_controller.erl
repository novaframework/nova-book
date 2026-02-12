-module(my_first_nova_api_controller).
-export([
         index/1,
         show/1,
         create/1
        ]).

index(_Req) ->
    Users = [
        #{id => 1, name => <<"Alice">>, email => <<"alice@example.com">>},
        #{id => 2, name => <<"Bob">>, email => <<"bob@example.com">>}
    ],
    {json, #{users => Users}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{id => binary_to_integer(Id), name => <<"Alice">>, email => <<"alice@example.com">>}};
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{json := #{<<"name">> := Name, <<"email">> := Email}}) ->
    {json, 201, #{}, #{id => 3, name => Name, email => Email}};
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and email required">>}}.

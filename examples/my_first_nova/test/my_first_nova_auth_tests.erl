-module(my_first_nova_auth_tests).
-include_lib("eunit/include/eunit.hrl").

valid_login_test() ->
    Req = #{params => #{<<"username">> => <<"admin">>,
                        <<"password">> => <<"password">>}},
    ?assertMatch({true, #{authed := true, username := <<"admin">>}},
                 my_first_nova_auth:username_password(Req)).

invalid_password_test() ->
    Req = #{params => #{<<"username">> => <<"admin">>,
                        <<"password">> => <<"wrong">>}},
    ?assertEqual(false, my_first_nova_auth:username_password(Req)).

missing_params_test() ->
    Req = #{params => #{}},
    ?assertEqual(false, my_first_nova_auth:username_password(Req)).

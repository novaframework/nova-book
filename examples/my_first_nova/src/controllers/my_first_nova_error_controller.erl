-module(my_first_nova_error_controller).
-export([
         not_found/1,
         server_error/1
        ]).

not_found(_Req) ->
    {ok, [{title, <<"404 - Not Found">>},
          {message, <<"The page you are looking for does not exist.">>}],
     #{view => error_page, status_code => 404}}.

server_error(_Req) ->
    {ok, [{title, <<"500 - Server Error">>},
          {message, <<"Something went wrong. Please try again later.">>}],
     #{view => error_page, status_code => 500}}.

-module(blog_data_fixtures).

-export([start/0, checkout/0, checkin/0, user/1, post/2, url/1, request/3, request/2]).
-export([token/1, request_as/3, request_as/4]).

start() ->
    ok = application:set_env(nova_auth, pbkdf2_iterations, 1000),
    {ok, _} = application:ensure_all_started(blog),
    {ok, _} = application:ensure_all_started(inets),
    kura_sandbox:start().

checkout() ->
    kura_sandbox:checkout(blog_repo, #{shared => true}).

checkin() ->
    kura_sandbox:checkin(blog_repo).

user(Name) ->
    Params = #{
        ~"email" => <<Name/binary, "@example.com">>,
        ~"username" => Name,
        ~"password" => ~"correct horse battery",
        ~"password_confirmation" => ~"correct horse battery"
    },
    {ok, User} = blog_repo:insert(blog_user:registration_changeset(#{}, Params)),
    User.

post(Author, Params) ->
    {ok, Post} = blog_posts:create_post(Author, maps:merge(#{~"body" => ~"Body text"}, Params)),
    Post.

url(Path) ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    "http://localhost:" ++ integer_to_list(Port) ++ Path.

request(Method, Path) ->
    {ok, {{_, Status, _}, _, Body}} = httpc:request(Method, {url(Path), []}, [], [
        {body_format, binary}
    ]),
    {Status, decode(Body)}.

request(Method, Path, Json) ->
    Body = iolist_to_binary(json:encode(Json)),
    {ok, {{_, Status, _}, _, RespBody}} =
        httpc:request(Method, {url(Path), [], "application/json", Body}, [], [{body_format, binary}]),
    {Status, decode(RespBody)}.

token(User) ->
    {ok, Token} = blog_accounts:issue_token(User),
    Token.

request_as(Token, Method, Path) ->
    {ok, {{_, Status, _}, _, Body}} = httpc:request(
        Method, {url(Path), [bearer(Token)]}, [], [{body_format, binary}]
    ),
    {Status, decode(Body)}.

request_as(Token, Method, Path, Json) ->
    Body = iolist_to_binary(json:encode(Json)),
    {ok, {{_, Status, _}, _, RespBody}} = httpc:request(
        Method, {url(Path), [bearer(Token)], "application/json", Body}, [], [{body_format, binary}]
    ),
    {Status, decode(RespBody)}.

bearer(Token) ->
    {"authorization", "Bearer " ++ binary_to_list(Token)}.

decode(<<>>) -> <<>>;
decode(Body) -> json:decode(Body).

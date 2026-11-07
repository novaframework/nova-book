-module(blog_data_fixtures).

-export([start/0, start/1, checkout/0, checkin/0, user/1, post/2, url/1, request/3, request/2]).
-export([token/1, request_as/3, request_as/4, session/1, admin/1]).
-export([log_in/3, api_token/3, reset_rate_limit/0]).

start() ->
    start([]).

start(Config) ->
    ok = application:set_env(nova_auth, pbkdf2_iterations, 1000),
    Config1 = nova_test:start(blog, Config),
    kura_sandbox:start(),
    Config1.

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

admin(Name) ->
    {ok, Admin} = blog_repo:update(blog_user:role_changeset(user(Name), #{role => admin})),
    Admin.

session(User) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    ok = nova_session_ets:set_value(SessionId, ~"user_token", token(User)),
    SessionId.

log_in(Config, Email, Password) ->
    Session = nova_test:session(Config),
    {ok, #{status := 200} = Form} = nova_test:get("/login", Session),
    {ok, #{status := 302} = Resp} = nova_test:post(
        "/login",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"email", Email},
                {~"password", Password}
            ]
        },
        Session
    ),
    "/posts" = nova_test:header("location", Resp),
    Session.

api_token(Config, Email, Password) ->
    Credentials = #{~"email" => Email, ~"password" => Password},
    {ok, #{status := 201} = Resp} = nova_test:post("/api/tokens", #{json => Credentials}, Config),
    #{~"token" := Token} = nova_test:json(Resp),
    Token.

reset_rate_limit() ->
    try
        seki:reset(blog_login_limiter, {127, 0, 0, 1})
    catch
        error:{limiter_not_found, blog_login_limiter} -> ok
    end.

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

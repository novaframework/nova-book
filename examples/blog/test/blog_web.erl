-module(blog_web).

-export([
    get/2,
    get/3,
    post_form/3,
    post_json/3,
    post_json/4,
    request/5,
    csrf_token/1,
    log_in/2,
    api_token/2,
    header/2,
    set_cookie/2,
    reset_rate_limit/0
]).

get(Jar, Path) ->
    get(Jar, Path, []).

get(Jar, Path, Headers) ->
    request(get, Jar, Path, Headers, none).

post_form(Jar, Path, Fields) ->
    Body = uri_string:compose_query([{to_list(K), to_list(V)} || {K, V} <- Fields]),
    request(post, Jar, Path, [], {"application/x-www-form-urlencoded", Body}).

post_json(Jar, Path, Json) ->
    post_json(Jar, Path, Json, []).

post_json(Jar, Path, Json, Headers) ->
    request(post, Jar, Path, Headers, {"application/json", json:encode(Json)}).

request(Method, Jar, Path, Headers, Body) ->
    AllHeaders = cookie_header(Jar) ++ Headers,
    Request =
        case Body of
            none -> {blog_data_fixtures:url(Path), AllHeaders};
            {ContentType, Data} -> {blog_data_fixtures:url(Path), AllHeaders, ContentType, Data}
        end,
    {ok, {{_, Status, _}, RespHeaders, RespBody}} =
        httpc:request(Method, Request, [{autoredirect, false}], [{body_format, binary}]),
    {Status, RespHeaders, RespBody, update_jar(Jar, RespHeaders)}.

csrf_token(Html) ->
    {match, [Token]} = re:run(
        Html, ~"name=\"_csrf_token\" value=\"([^\"]+)\"", [{capture, all_but_first, binary}]
    ),
    Token.

log_in(Email, Password) ->
    {200, _, Form, Jar} = get(#{}, "/login"),
    {302, Headers, _, Jar1} = post_form(Jar, "/login", [
        {"_csrf_token", csrf_token(Form)}, {"email", Email}, {"password", Password}
    ]),
    "/posts" = header("location", Headers),
    Jar1.

api_token(Email, Password) ->
    Credentials = #{email => iolist_to_binary(Email), password => iolist_to_binary(Password)},
    {201, _, Body, _} = post_json(#{}, "/api/tokens", Credentials),
    #{~"token" := Token} = json:decode(Body),
    Token.

header(Name, Headers) ->
    proplists:get_value(Name, Headers).

set_cookie(Name, Headers) ->
    [Cookie || {"set-cookie", Cookie} <- Headers, lists:prefix(Name ++ "=", Cookie)].

reset_rate_limit() ->
    try
        seki:reset(blog_login_limiter, {127, 0, 0, 1})
    catch
        error:{limiter_not_found, blog_login_limiter} -> ok
    end.

cookie_header(Jar) when map_size(Jar) =:= 0 ->
    [];
cookie_header(Jar) ->
    [{"cookie", lists:join("; ", [[Name, "=", Value] || Name := Value <- Jar])}].

update_jar(Jar, Headers) ->
    update_cookies(Jar, [Cookie || {"set-cookie", Cookie} <- Headers]).

update_cookies(Jar, []) ->
    Jar;
update_cookies(Jar, [Cookie | Rest]) ->
    [Pair | Attributes] = string:split(Cookie, ";", all),
    [Name, Value] = string:split(Pair, "="),
    Expired = lists:any(fun is_expiry/1, Attributes),
    case Expired of
        true -> update_cookies(maps:remove(Name, Jar), Rest);
        false -> update_cookies(Jar#{Name => Value}, Rest)
    end.

is_expiry(Attribute) ->
    string:lowercase(string:trim(Attribute)) =:= "max-age=0".

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value.

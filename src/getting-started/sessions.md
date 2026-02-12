# Sessions

In the previous chapter we built authentication with a form POST. But if you navigate to another page, the auth is lost — there is no session. Let's fix that.

## How sessions work in Nova

Nova has a built-in session system backed by ETS (Erlang Term Storage). Session IDs are stored in a `session_id` cookie. When a request comes in, you use the session API to get and set values tied to that session.

The session manager is configured in `sys.config`:

```erlang
{nova, [
    {session_manager, nova_session_ets}
]}
```

`nova_session_ets` is the default. It stores session data in an ETS table and replicates changes across clustered nodes using `nova_pubsub`.

## The session API

```erlang
%% Get a value from the session
nova_session:get(Req, <<"key">>) -> {ok, Value} | {error, not_found}.

%% Set a value in the session
nova_session:set(Req, <<"key">>, <<"value">>) -> ok.

%% Delete the entire session (clears the cookie)
nova_session:delete(Req) -> {ok, Req1}.

%% Delete a specific key from the session
nova_session:delete(Req, <<"key">>) -> {ok, Req1}.

%% Generate a new session ID
nova_session:generate_session_id() -> {ok, SessionId}.
```

All functions take the Cowboy request map to read the `session_id` cookie.

## Adding session-based auth

We need two security functions — one for the login POST (username/password) and one for subsequent requests (session check).

Update `src/my_first_nova_auth.erl`:

```erlang
-module(my_first_nova_auth).
-export([
         username_password/1,
         session_auth/1
        ]).

%% Used for the login POST
username_password(#{params := Params}) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            {true, #{authed => true, username => Username}};
        _ ->
            false
    end.

%% Used for pages that need an active session
session_auth(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {true, #{authed => true, username => Username}};
        {error, _} ->
            false
    end.
```

## Creating the session on login

Update the controller to create a session when authentication succeeds:

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1,
         login/1,
         login_post/1,
         logout/1
        ]).

index(#{auth_data := #{authed := true, username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {redirect, "/login"}.

login(_Req) ->
    {ok, [], #{view => login}}.

login_post(#{auth_data := #{authed := true, username := Username}} = Req) ->
    {ok, SessionId} = nova_session:generate_session_id(),
    Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req,
                                       #{path => <<"/">>, http_only => true}),
    nova_session_ets:set_value(SessionId, <<"username">>, Username),
    {redirect, "/"};
login_post(_Req) ->
    {ok, [{error, <<"Invalid username or password">>}], #{view => login}}.

logout(Req) ->
    {ok, _Req1} = nova_session:delete(Req),
    {redirect, "/login"}.
```

The login flow:
1. Generate a session ID
2. Set the `session_id` cookie on the response
3. Store the username in the session
4. Redirect to the home page

## Updating the routes

```erlang
routes(_Environment) ->
  [
    %% Public routes
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    },

    %% Login POST (uses username/password auth)
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login_post/1, #{methods => [post]}}
                ]
    },

    %% Protected pages (uses session auth)
    #{prefix => "",
      security => fun my_first_nova_auth:session_auth/1,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                 {"/logout", fun my_first_nova_main_controller:logout/1, #{methods => [get]}}
                ]
    }
  ].
```

Now the flow is:
1. User visits `/login` → sees the login form
2. Form POSTs to `/login` → `username_password/1` checks credentials
3. On success, a session is created and the user is redirected to `/`
4. On `/`, `session_auth/1` checks the session cookie
5. `/logout` deletes the session and redirects to `/login`

## Reading session data in controllers

Once a session is active, you can read values from it in any controller:

```erlang
profile(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {json, #{username => Username}};
        {error, _} ->
            {status, 401}
    end.
```

## Cookie options

When setting the session cookie, control its behaviour with options:

```erlang
cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req, #{
    path => <<"/">>,          %% Cookie is valid for all paths
    http_only => true,        %% Not accessible from JavaScript
    secure => true,           %% Only sent over HTTPS
    max_age => 86400          %% Expires after 24 hours (in seconds)
}).
```

```admonish warning
For production, always set `http_only` and `secure` to `true`.
```

## Custom session backends

If you want to store sessions in a database or Redis instead of ETS, implement the `nova_session` behaviour:

```erlang
-module(my_redis_session).
-behaviour(nova_session).

-export([start_link/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2]).

start_link() ->
    %% Start your Redis connection
    ignore.

get_value(SessionId, Key) ->
    %% Read from Redis
    {ok, Value}.

set_value(SessionId, Key, Value) ->
    %% Write to Redis
    ok.

delete_value(SessionId) ->
    %% Delete entire session from Redis
    ok.

delete_value(SessionId, Key) ->
    %% Delete a single key from Redis
    ok.
```

Then configure it:

```erlang
{nova, [
    {session_manager, my_redis_session}
]}
```

---

We now have a complete authentication and session system. Next, let's move beyond HTML and build a [JSON API](../building-apis/json-apis.md).

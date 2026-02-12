# Authentication

In previous chapters we set up routing, plugins, and a login view. Now let's add authentication so we can handle the login form submission.

## Security in route groups

Authentication in Nova is configured per route group using the `security` key. It points to a function that receives the request and returns either `{true, AuthData}` (allow) or `false` (deny).

## Creating a security module

Create `src/my_first_nova_auth.erl`:

```erlang
-module(my_first_nova_auth).
-export([username_password/1]).

username_password(#{params := Params}) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            {true, #{authed => true, username => Username}};
        _ ->
            false
    end.
```

This function checks the decoded form parameters. If the password matches `"password"`, it returns `{true, AuthData}` — the auth data map is attached to the request and accessible in your controller as `auth_data`.

```admonish warning
This is a hardcoded password for demonstration only. In a real application you would validate credentials against a database with properly hashed passwords.
```

## Updating the routes

Rearrange the router to separate public and protected routes:

```erlang
routes(_Environment) ->
  [
    %% Public routes — no security
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    },

    %% Protected route — requires authentication
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [post]}}
                ]
    }
  ].
```

The login page is public. The `POST /` route uses `my_first_nova_auth:username_password/1` as its security function. When a POST comes in, Nova calls the security function first — if it returns `false`, the request is rejected with a 401.

## Using auth data in controllers

Update the controller to use the authenticated username:

```erlang
index(#{auth_data := #{authed := true, username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {status, 401}.
```

Pattern matching on `auth_data` lets you access the data your security function returned. If there is no auth data (someone bypassed security somehow), we return 401.

## Testing the flow

Start the server with `rebar3 nova serve`, then:

1. Go to `http://localhost:8080/login`
2. Enter any username and `password` as the password
3. Submit the form
4. You should see "Hello USERNAME" on the welcome page

If you enter a wrong password, the security function returns `false` and Nova responds with 401.

## How security works

The security flow for each request is:

1. Nova matches the request to a route group
2. If `security` is `false`, skip to the controller
3. If `security` is a function, call it with the request map
4. If it returns `{true, AuthData}`, merge `auth_data => AuthData` into the request and continue to the controller
5. If it returns `false`, trigger the 401 error handler

You can have different security functions for different route groups — one for API token auth, another for session auth, and so on.

---

Our login works, but the authentication is lost on the next request — there is no session. Let's fix that with [sessions](sessions.md).

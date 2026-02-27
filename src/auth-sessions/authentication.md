# Authentication

Now let's protect routes so only logged-in users can access them. We'll build session-based authentication by hand, then see how the `gen_auth` generator scaffolds a complete email/password system.

## Security in route groups

Authentication in Nova is configured per route group using the `security` key. It points to a function that receives the request and returns either `{true, AuthData}` (allow) or a denial value (deny).

## Creating a security module

Create `src/blog_auth.erl`:

```erlang
-module(blog_auth).
-export([session_auth/1]).

session_auth(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {true, #{username => Username}};
        {error, _} ->
            {redirect, "/login"}
    end.
```

`session_auth/1` checks whether the session contains a username. If so, it returns `{true, AuthData}` — the auth data map is merged into the request and accessible in your controller as `auth_data`. If the session is empty, it redirects to the login page.

```admonish tip
Returning `{redirect, "/login"}` instead of bare `false` gives users a friendly redirect to the login page. A bare `false` would trigger the generic 401 error handler, which is more appropriate for APIs.
```

## Processing the login form

Credential validation belongs in the controller, not the security function. The security function's job is to *gate access* — the login POST route is public by definition (unauthenticated users need to reach it), so it uses `security => false`.

The controller checks the submitted credentials and either creates a session or re-renders the form with an error:

```erlang
login_post(#{params := Params} = Req) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            nova_session:set(Req, <<"username">>, Username),
            {redirect, "/"};
        _ ->
            {ok, [{error, <<"Invalid username or password">>}], #{view => login}}
    end.
```

On success, we store the username in the session and redirect to the home page. On failure, we re-render the login template with an error message — the user sees the form again instead of a raw error page.

```admonish warning
This is a hardcoded password for demonstration only. In a real application you would validate credentials against a database with properly hashed passwords.
```

## How security works

The security flow for each request is:

1. Nova matches the request to a route group
2. If `security` is `false`, skip to the controller
3. If `security` is a function, call it with the request map
4. If it returns `{true, AuthData}`, merge `auth_data => AuthData` into the request and continue to the controller
5. If it returns `true`, continue to the controller (no auth data attached)
6. If it returns `false`, trigger the 401 error handler
7. If it returns `{redirect, Path}`, send a 302 redirect without calling the controller
8. If it returns `{false, StatusCode, Headers, Body}`, respond with a custom error

The structured `{false, StatusCode, Headers, Body}` form is useful for APIs where you want to return JSON error details instead of triggering the generic 401 handler.

You can have different security functions for different route groups — one for API token auth, another for session auth, and so on.

## Wiring up the login flow

Update the controller to handle login, logout, and the home page:

```erlang
-module(blog_main_controller).
-export([
         index/1,
         login/1,
         login_post/1,
         logout/1
        ]).

index(#{auth_data := #{username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]}.

login(_Req) ->
    {ok, [], #{view => login}}.

login_post(#{params := Params} = Req) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            nova_session:set(Req, <<"username">>, Username),
            {redirect, "/"};
        _ ->
            {ok, [{error, <<"Invalid username or password">>}], #{view => login}}
    end.

logout(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    {redirect, "/login", Req1}.
```

### Updating the routes

```erlang
routes(_Environment) ->
  [
    %% Public routes (no auth required)
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun blog_main_controller:login/1, #{methods => [get]}},
                 {"/login", fun blog_main_controller:login_post/1, #{methods => [post]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    },

    %% Protected routes (session auth required)
    #{prefix => "",
      security => fun blog_auth:session_auth/1,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}},
                 {"/logout", fun blog_main_controller:logout/1, #{methods => [get]}}
                ]
    }
  ].
```

## The gen_auth scaffold

For a production-ready authentication system, use the `gen_auth` generator:

```shell
rebar3 nova gen_auth
```

This generates a complete email/password auth system:

- **Migration** — `users` table with email, password_hash, and confirmation fields
- **Schema** — `user.erl` with registration and login changesets
- **Context module** — `blog_accounts.erl` with create_user, authenticate, token management
- **Security callback** — `blog_auth.erl` with session and token-based authentication
- **Controllers** — Registration, login, password reset controllers
- **Test suite** — Common Test suite covering the auth flow

The generated code uses `bcrypt` for password hashing and includes:

- Email/password registration with confirmation
- Login with session creation
- Logout with session destruction
- Password reset flow with time-limited tokens
- Remember-me tokens

```admonish tip
`gen_auth` is a starting point. Review the generated code, adjust the changeset validations, and wire in your email adapter (see [Sending Email](../email/sending-email.md)) for confirmation and password reset emails.
```

---

Next, let's look at [authorization](authorization.md) — controlling what authenticated users can do.

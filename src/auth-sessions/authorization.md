# Authorization

Authentication answers "who are you?" — authorization answers "what can you do?" This chapter covers patterns for controlling access based on user roles and permissions.

## Role-based security functions

The simplest approach is checking roles in your security function:

```erlang
-module(blog_auth).
-export([session_auth/1, admin_auth/1]).

session_auth(Req) ->
    case nova_session:get(Req, <<"user_id">>) of
        {ok, UserId} ->
            {ok, User} = blog_repo:get(user, UserId),
            {true, User};
        {error, _} ->
            {redirect, "/login"}
    end.

admin_auth(Req) ->
    case session_auth(Req) of
        {true, #{role := admin} = User} ->
            {true, User};
        {true, _User} ->
            {false, 403, #{}, #{error => <<"forbidden">>}};
        Other ->
            Other
    end.
```

Use different security functions for different route groups:

```erlang
routes(_Environment) ->
  [
    #{prefix => "",
      security => fun blog_auth:session_auth/1,
      routes => [
                 {"/", fun blog_main_controller:index/1, #{methods => [get]}}
                ]
    },
    #{prefix => "/admin",
      security => fun blog_auth:admin_auth/1,
      routes => [
                 {"/dashboard", fun blog_admin_controller:index/1, #{methods => [get]}}
                ]
    }
  ].
```

## Resource-level authorization

Sometimes you need to check ownership — "can this user edit this post?" This happens in the controller:

```erlang
update(#{bindings := #{<<"id">> := Id}, json := Params,
         auth_data := #{id := UserId}}) ->
    case blog_repo:get(post, binary_to_integer(Id)) of
        {ok, #{user_id := UserId} = Post} ->
            %% User owns this post — allow update
            CS = post:changeset(Post, Params),
            case blog_repo:update(CS) of
                {ok, Updated} -> {json, post_to_json(Updated)};
                {error, CS1} -> {json, 422, #{}, #{errors => changeset_errors_to_json(CS1)}}
            end;
        {ok, _Post} ->
            %% Different user — forbidden
            {status, 403, #{}, #{error => <<"you can only edit your own posts">>}};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"post not found">>}}
    end.
```

## Token-based API authentication

For APIs, use bearer tokens instead of sessions:

```erlang
api_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            case blog_accounts:verify_token(Token) of
                {ok, User} -> {true, User};
                {error, _} -> {false, 401, #{}, #{error => <<"invalid token">>}}
            end;
        _ ->
            {false, 401, #{}, #{error => <<"missing authorization header">>}}
    end.
```

## Combining authentication strategies

Different route groups can use different strategies:

```erlang
routes(_Environment) ->
  [
    %% Public
    #{prefix => "", security => false,
      routes => [
          {"/login", fun blog_main_controller:login/1, #{methods => [get, post]}}
      ]},

    %% Session-based (HTML pages)
    #{prefix => "", security => fun blog_auth:session_auth/1,
      routes => [
          {"/", fun blog_main_controller:index/1, #{methods => [get]}},
          {"/logout", fun blog_main_controller:logout/1, #{methods => [get]}}
      ]},

    %% Token-based (API)
    #{prefix => "/api", security => fun blog_auth:api_auth/1,
      routes => [
          {"/posts", fun blog_posts_controller:list/1, #{methods => [get]}},
          {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}}
      ]}
  ].
```

---

With authentication and authorization covered, let's move to the visual layer. Next: [ErlyDTL Templates](../views-templates/templates.md).

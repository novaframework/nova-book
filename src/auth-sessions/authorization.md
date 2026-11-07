# Authorization

Authentication says who is asking; authorization says whether they may. The blog has three rules:

- Anyone logged in may write posts and comments.
- Only a post's author, or an admin, may edit or delete it, or see it while it is a draft.
- Only admins may see the user list.

Nova can check rules at two levels: per route group with a security function, and per resource inside the controller. The blog uses both, with the rules themselves written once in a policy module.

## The policy module

`nova_auth_policy` provides rule builders. Each returns a map with a `condition`, a function of the actor and a context map:

| Builder | Allows |
|---------|--------|
| `allow_authenticated()` | any actor that is not `undefined` |
| `allow_role(Role)` or `allow_role([Roles])` | actors whose `role` is one of them |
| `allow_claim(Key, Values)` | actors with a matching claim, for OIDC groups |
| `allow_owner(Field)` | actors whose `id` equals the record's `Field` |
| `deny_all()` | nobody |

They were written for `nova_resource`, but they work on their own. `src/blog_policy.erl` maps each action to a list of rules, and allows the action if any rule allows it:

```erlang
-module(blog_policy).

-export([can/3]).

can(undefined, _Action, _Resource) ->
    false;
can(Actor, Action, Resource) ->
    Extra = #{type => Action, record => Resource},
    lists:member(true, [Condition(Actor, Extra) || #{condition := Condition} <- policies(Action)]).

policies(create_post) ->
    [nova_auth_policy:allow_authenticated()];
policies(Action) when Action =:= update_post; Action =:= delete_post; Action =:= view_draft ->
    [nova_auth_policy:allow_role(admin), nova_auth_policy:allow_owner(user_id)];
policies(manage_users) ->
    [nova_auth_policy:allow_role(admin)];
policies(manage_tags) ->
    [nova_auth_policy:allow_role(admin)];
policies(_Action) ->
    [nova_auth_policy:deny_all()].
```

- The first clause answers for anonymous visitors before any rule runs: `allow_role/1` and `allow_owner/1` expect a map.
- An unknown action is denied. A typo in an action name fails closed.
- `role` in the actor is the atom from the `{enum, [author, admin]}` field, so `allow_role(admin)` matches it directly.

The rules are plain data, so the policy is tested without HTTP:

```erlang
policy_rules(Config) ->
    Alice = blog_accounts:actor(?config(alice, Config)),
    Bob = blog_accounts:actor(?config(bob, Config)),
    Admin = blog_accounts:actor(?config(admin, Config)),
    Post = ?config(post, Config),
    true = blog_policy:can(Alice, update_post, Post),
    false = blog_policy:can(Bob, update_post, Post),
    true = blog_policy:can(Admin, delete_post, Post),
    true = blog_policy:can(Bob, create_post, #{}),
    false = blog_policy:can(undefined, create_post, #{}),
    false = blog_policy:can(Alice, manage_users, #{}),
    true = blog_policy:can(Admin, manage_users, #{}),
    false = blog_policy:can(Admin, launch_rockets, #{}).
```

## Route level: the admin pages

When a rule depends only on who is asking, check it in the security function and the controller never runs for anyone else. `require_admin/1` in `src/blog_security.erl` builds on `require_user/1`:

```erlang
require_admin(Req) ->
    case require_user(Req) of
        {true, Actor} ->
            case blog_policy:can(Actor, manage_users, #{}) of
                true -> {true, Actor};
                false -> {false, 403, #{~"content-type" => ~"text/html"}, blog_view:forbidden()}
            end;
        Redirect ->
            Redirect
    end.
```

An anonymous visitor is sent to `/login`; a logged-in author gets a 403 page. The page is rendered to a binary by `blog_view:forbidden/0`, since a security function returns a body, not a template:

```erlang
forbidden() ->
    {ok, Html} = blog_forbidden_dtl:render(#{}),
    iolist_to_binary(Html).
```

The `/admin` route group (shown in [Authentication](authentication.md#the-routes)) uses it, so the controller is two lines of query:

```erlang
-module(blog_admin_controller).

-export([users/1]).

users(Req) ->
    {ok, Users} = blog_repo:all(kura_query:order_by(kura_query:from(blog_user), [{id, asc}])),
    Rows = [
        #{username => Username, email => Email, role => atom_to_binary(Role)}
     || #{username := Username, email := Email, role := Role} <- Users
    ],
    blog_view:render(Req, blog_admin_users, #{users => Rows}).
```

Only the fields the page shows are passed to the template; `hashed_password` never gets near it.

## Resource level: the API

"Is this your post?" needs the post, so the check moves into the controller. `blog_posts_controller` loads the post, asks the policy, and only then acts:

```erlang
update(#{bindings := #{~"id" := Id}, json := Params, auth_data := Actor}) ->
    case authorize(Actor, update_post, blog_posts:get_post(Id)) of
        {ok, Post} ->
            case blog_posts:update_post(Post, Params) of
                {ok, Updated} -> {json, blog_api_json:post(Updated)};
                {error, stale} -> {json, 409, #{}, #{error => ~"post was changed by someone else"}};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        Denied ->
            Denied
    end;
```

```erlang
authorize(Actor, Action, {ok, Post}) ->
    case blog_policy:can(Actor, Action, Post) of
        true -> {ok, Post};
        false -> {json, 403, #{}, #{error => ~"you can only change your own posts"}}
    end;
authorize(_Actor, _Action, {error, not_found}) ->
    not_found().
```

`delete/1` and `update_tags/1` use `authorize/3` the same way. `create/1` needs no check beyond the bearer token: anyone with a token may write, and the post is theirs. A comment belongs to its writer and can be added to any post.

## Resource level: the HTML pages

The HTML controller wraps every action on an existing post in one function, so no action can forget the check:

```erlang
edit(Req) ->
    with_own_post(Req, update_post, fun edit_form/2).

update(Req) ->
    with_own_post(Req, update_post, fun save/2).

delete(Req) ->
    with_own_post(Req, delete_post, fun remove/2).
```

```erlang
with_own_post(#{bindings := #{~"id" := Id}, auth_data := Actor} = Req, Action, Next) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_policy:can(Actor, Action, Post) of
                true -> Next(Req, Post);
                false -> {status, 403, #{~"content-type" => ~"text/html"}, blog_view:forbidden()}
            end;
        {error, not_found} ->
            blog_error_controller:not_found(Req)
    end.
```

`edit_form/2`, `save/2` and `remove/2` are local functions that receive the post already checked. The post page asks the same question to decide whether to show the Edit and Delete buttons:

```erlang
render_post(Req, Actor, Post) ->
    Vars = #{post => blog_view:post(Post), can_edit => blog_policy:can(Actor, update_post, Post)},
    blog_view:render(Req, blog_post_show, Vars).
```

Hiding a button is a courtesy, not a control. The test checks both: Bob sees no Edit link on Alice's post, and a hand-made request is refused anyway.

```erlang
html_author_cannot_edit_others_posts(Config) ->
    #{id := Id} = ?config(post, Config),
    Path = "/posts/" ++ integer_to_list(Id),
    Jar = blog_web:log_in("bob@example.com", ?PASSWORD),
    {200, _, Show, _} = blog_web:get(Jar, Path),
    ?assertEqual(nomatch, binary:match(Show, ~"/edit\"")),
    {403, Headers, Body, _} = blog_web:get(Jar, Path ++ "/edit"),
    "text/html" = blog_web:header("content-type", Headers),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<h1>Forbidden</h1>")),
    Token = blog_web:csrf_token(Show),
    {403, _, _, _} = blog_web:post_form(Jar, Path ++ "/delete", [{"_csrf_token", Token}]),
    {403, _, _, _} = blog_web:post_form(Jar, Path, [{"_csrf_token", Token}, {"title", "Mine now"}]),
    {ok, #{title := ~"Alice writes", deleted_at := undefined}} = blog_repo:get(blog_post, Id).
```

## Drafts

A draft is visible to its author and admins only. To everyone else it does not exist, so the answer is 404, not 403: a 403 would confirm that post 7 is a draft.

```erlang
show(#{bindings := #{~"id" := Id}} = Req) ->
    Actor = maps:get(auth_data, Req, undefined),
    case blog_posts:get_post(Id, [author]) of
        {ok, #{status := published} = Post} ->
            render_post(Req, Actor, Post);
        {ok, Post} ->
            case blog_policy:can(Actor, view_draft, Post) of
                true -> render_post(Req, Actor, Post);
                false -> blog_error_controller:not_found(Req)
            end;
        {error, not_found} ->
            blog_error_controller:not_found(Req)
    end.
```

`auth_data` is only there for a logged-in reader (the route group uses `current_user/1`), hence the `undefined` default, which `can/3` denies.

```admonish warning title="The JSON API"
The read endpoints from [CRUD & the Query Builder](../data-layer/crud.md) are not covered by this: at this point `GET /api/posts?status=draft` still lists every draft. [Security](../production/security.md#drafts-in-the-json-api) closes that gap with the scoping technique of the next section.
```

## Scoping queries to the owner

For lists, checking each row after loading it is wasteful and easy to get wrong with pagination. `allow_owner/1` has a second mode for this: given a context with `type => read`, its condition returns a function that adds the owner filter to a Kura query.

```erlang
owner_policy_scopes_reads(Config) ->
    #{id := BobId} = Bob = ?config(bob, Config),
    _ = blog_data_fixtures:post(Bob, #{~"title" => ~"Bob writes"}),
    #{condition := Condition} = nova_auth_policy:allow_owner(user_id),
    Scope = Condition(#{id => BobId}, #{type => read}),
    {ok, [#{title := ~"Bob writes"}]} = blog_repo:all(Scope(kura_query:from(blog_post))).
```

The fixture also has a post by Alice; the scoped query only returns Bob's. The database does the filtering, so counts and pages stay correct.

```admonish tip title="Do not call an action read"
`can/3` passes the action as `type`. An action named `read` would make `allow_owner/1` return a query function instead of `true`. `can/3` would then deny it (a function is not `true`), so the mistake fails closed, but give your actions other names.
```

## The audit trail

[Data Safety](../data-layer/data-safety.md#an-audit-log) records changes in `audit_log` with an actor. [Authentication](authentication.md#security-functions) already sets the actor in `authenticated/1`, on every authenticated request, browser or API; the test below proves it.

`kura_audit:set_actor/1` stores the actor in the process dictionary. Cowboy runs each request in its own process, so the security function, the controller and the Kura hooks all see it, and the next request starts clean.

```erlang
audit_log_records_the_actor(Config) ->
    #{id := Id} = ?config(post, Config),
    #{id := AliceId} = ?config(alice, Config),
    Token = blog_data_fixtures:token(?config(alice, Config)),
    {200, _} = blog_data_fixtures:request_as(
        Token, put, "/api/posts/" ++ integer_to_list(Id), #{title => ~"Edited"}
    ),
    Query = kura_query:where(kura_query:from(kura_audit_log), {action, ~"update"}),
    {ok, [#{actor := Actor}]} = blog_repo:all(Query),
    ?assertEqual(integer_to_binary(AliceId), Actor).
```

## Auditing the routes

`rebar3 nova audit` lists every route without a security function. It is a quick way to spot a write route you forgot to protect:

```shell
rebar3 nova audit
```

```
=== Security Audit ===

  Audited routes(dev) of blog. Not covered: static files and error pages, arizona_nova routes (served before nova_router). nova_apps read from ./config/dev_sys.config.src.

  WARNINGS:
    POST /api/tokens (blog_tokens_controller) has no security
    POST /contact (blog_contact_controller) has no security
    POST /hello (blog_hello_controller) has no security
    POST /login (blog_session_controller) has no security
    POST /register (blog_registration_controller) has no security

  INFO:
    GET / (blog_main_controller) has no security
    GET /about (blog_pages_controller) has no security
    GET /about-us (blog_pages_controller) has no security
    GET /about.md (blog_pages_controller) has no security
    GET /api/posts (blog_posts_controller) has no security
    GET /api/posts/{id} (blog_posts_controller) has no security
    GET /contact (blog_contact_controller) has no security
    GET /heartbeat (blog_router) has no security
    GET /hello/{name} (blog_hello_controller) has no security
    GET /version (blog_pages_controller) has no security

  Summary: 5 warning(s), 10 info(s)
```

Read the warnings one by one. `/login`, `/register` and `/api/tokens` are open on purpose: they are how you get credentials, and they are rate-limited. `/hello` and `/contact` are the demos from Part I. The post writes are gone from the list: they sit behind `require_bearer/1` now. Reading posts stays public, so `GET /api/posts` and `GET /api/posts/{id}` are listed as information.

## Running the tests

```shell
rebar3 ct --suite=test/blog_auth_policy_SUITE
```

`blog_auth_policy_SUITE` covers the policy rules, query scoping, the API and HTML checks for authors and admins, drafts, the admin pages and the audit actor.

---

Part III is done: the blog knows who is reading and what they may do. Part IV gives them pages to do it on, starting with [ErlyDTL Templates](../views-templates/templates.md).

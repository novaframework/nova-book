# Cheat Sheet

Quick reference for the APIs this book uses, at the versions it was tested with: Nova 0.18.1, Kura 2.25 with kura_postgres 1.1, nova_auth 0.2, Arizona 0.4 with arizona_nova 0.1, Shigoto 1.9, Hikyaku 0.2 and nova_test 0.2. Each section links to the chapter that explains it.

## Routing

[Routing](../foundations/routing.md). `routes/1` in the router returns a list of route groups:

```erlang
#{
    prefix => "/api",
    security => fun blog_security:require_bearer/1,
    plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],
    routes => [
        {"/posts", fun blog_posts_controller:index/1, #{methods => [get]}},
        {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}},
        {"/ws/echo", blog_echo_ws, #{protocol => ws}},
        {"/assets/[...]", "assets"},
        {404, fun blog_error_controller:not_found/1, #{}}
    ]
}
```

| Group key | Default | Meaning |
|---|---|---|
| `routes` | `[]` | The route entries |
| `prefix` | `""` | Prepended to every path in the group |
| `security` | `false` | `false` or `fun Mod:Fun/1`, run before every controller in the group |
| `plugins` | the global list | Replaces the global plugin list for this group |
| `host` | `'_'` | Only match this host |

| Entry | Meaning |
|---|---|
| `{Path, fun Mod:Fun/1, Options}` | Controller function |
| `{Path, Module, #{protocol => ws}}` | WebSocket handler |
| `{StatusCode, fun Mod:Fun/1, Options}` | Handler for a status Nova produces itself (404, 500, ...) |
| `{Path, LocalPath}`, `{Path, LocalPath, Options}` | Static file or directory under `priv` (`mimetype`, `index_files`, `list_dir`) |

- `methods => [get, post, ...]` (lowercase atoms, default `'_'`); a known path with the wrong method gets `405`.
- `:name` segments are bindings with **binary** keys and raw (percent-encoded) values: `#{bindings := #{~"id" := Id}}`.
- `[...]` at the end matches the rest of the path.
- `rebar3 nova serve` does not reload routes; restart after editing the router.

## Controller return values

[Controllers & Responses](../foundations/controllers.md).

| Return | Response |
|---|---|
| `{ok, Vars}` / `{view, Vars}` | Render the controller's default template |
| `{ok, Vars, #{view => V, status_code => S, headers => H}}` | Named template, status, headers (all optional) |
| `{json, Data}` | JSON, 200 (201 for POST) |
| `{json, Status, Headers, Data}` | JSON with status and headers |
| `{status, Status}` | Status only, empty body; status handlers are not used |
| `{status, Status, Headers}` | Same, with headers |
| `{status, Status, Headers, Body}` | Binary body as is, a map as JSON |
| `{redirect, Path}` | `302` to `Path` |
| `{sendfile, Status, Headers, {Offset, Length, Path}, Mime}` | Stream a file |
| Any of the above plus a trailing `Req` | Use a modified request; for JSON it is `{json, Status, Headers, Req, Data}` |
| `throw({Status, Reason})` | Status handler for `Status` |
| Any other tuple, such as `{error, Reason}` | The module's `-fallback_controller(Mod)`, `Mod:resolve(Req, Return)`; 500 without one |

The request map: `method` (`~"GET"`), `path`, `qs`, `headers`, `bindings`, `peer`, plus `json`, `params`, `parsed_qs`, `body` from `nova_request_plugin`, `auth_data` from the security function, `csrf_token` from `nova_csrf_plugin`. Decoded JSON keys are binaries.

## Security functions

[Routing](../foundations/routing.md#security), [Authentication](../auth-sessions/authentication.md).

| Return | Effect |
|---|---|
| `true` | Continue |
| `{true, AuthData}` | Continue; the controller gets `auth_data => AuthData` |
| `false` | `401` |
| `{false, Headers}` | `401` with headers |
| `{false, Status, Headers}` | `Status` with headers |
| `{false, Status, Headers, Body}` | `Status`, headers and body; use this form when the client needs a body |
| `{redirect, Path}` | `302`; `Path` must be a string (`"/login"`), a binary crashes |

Security runs after the group's plugins, so on a CSRF-protected group a POST without a token is `403` before security is asked.

## Plugins

[Plugins](../foundations/plugins.md).

```erlang
-module(blog_require_json_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"...", version => ~"1.0.0", url => ~"...", authors => [~"..."],
      description => ~"...", options => []}.
```

| Return from `pre_request/4` or `post_request/4` | Effect |
|---|---|
| `{ok, Req, State}` | Continue with the next plugin |
| `{ok, Reply, Req, State}` | Set the response, continue |
| `{break, Req, State}`, `{break, Reply, Req, State}` | Crashes the request in 0.18.1 (500); use `stop` |
| `{stop, Reply, Req, State}` | Send `Reply` and stop the request |
| `{stop, Req, State}` | Stop; the plugin already replied |

`Reply` is `{reply, Body}`, `{reply, Status, Body}` or `{reply, Status, Headers, Body}` with a headers map. Do not return `{error, Reason}` from a plugin in 0.18.1; it crashes the request. Configure plugins as `{pre_request | post_request, Module, Options}`, globally in the `nova` env or per route group. They run in list order.

| Built-in plugin | Options | Adds |
|---|---|---|
| `nova_request_plugin` | `decode_json_body`, `read_urlencoded_body`, `parse_qs => true \| list` | `json`, `params`, `parsed_qs`, `body` |
| `nova_csrf_plugin` | `field_name`, `header_name`, `session_key`, `excluded_paths` | `csrf_token` |
| `nova_correlation_plugin` | `request_correlation_header`, `logger_metadata_key` | `correlation_id` |
| `nova_cors_plugin` | `allow_origins` | |
| `nova_multipart_plugin` | `handler => {Mod, Args}`, `max_parts`, `max_part_size`, `max_field_size`, `max_total_size`, `read_timeout` | `params`, `files` |

## Configuration

[Your First Nova Application](../foundations/create-app.md), [Sessions](../auth-sessions/sessions.md).

```erlang
{nova, [
    {environment, dev},
    {bootstrap_application, blog},
    {use_stacktrace, false},
    {use_strict_routing, false},
    {render_error_pages, true},
    {session_manager, nova_session_ets},
    {cowboy_configuration, #{
        port => 8080,
        stream_handlers => [nova_stream_h, blog_session_h, cowboy_compress_h, cowboy_stream_h]
    }},
    {plugins, [{pre_request, nova_request_plugin, #{decode_json_body => true}}]}
]},
{blog, [
    {json_lib, blog_json},
    {use_sessions, true},
    {nova_apps, []}
]}
```

`json_lib` and `use_sessions` are read from the **bootstrap application's** env, not `nova`'s. `json_lib` must export `encode/1` and `decode/1 -> {ok, Term} | {error, Reason}`, so plain OTP `json` needs an adapter like `blog_json`.

## Sessions

[Sessions](../auth-sessions/sessions.md).

| Function | Returns |
|---|---|
| `nova_session:get(Req, Key)` | `{ok, Value}` or `{error, not_found}` |
| `nova_session:set(Req, Key, Value)` | `ok`, or `{error, session_id_not_set}` |
| `nova_session:delete(Req, Key)` | `{ok, Req}` |
| `nova_session:delete(Req)` | `{ok, Req1}` with an expired cookie in `Req1` |

Nova's cookie has no `HttpOnly`/`Secure`/`SameSite` and is never rotated; the blog's `blog_session_h` sets the flags and `blog_session:log_in/2` issues a fresh ID.

## nova_auth

[Authentication](../auth-sessions/authentication.md), [Authorization](../auth-sessions/authorization.md).

| Call | Returns |
|---|---|
| `nova_auth_password:hash(Password)` | PBKDF2-SHA256 hash |
| `nova_auth_password:verify(Password, Hash)` | `boolean()`, constant time |
| `nova_auth_accounts:authenticate(blog_auth, Email, Password)` | `{ok, User}` or `{error, invalid_credentials}` |
| `nova_auth_session:generate_session_token(blog_auth, User)` | `{ok, Token}` |
| `nova_auth_session:get_user_by_session_token(blog_auth, Token)` | `{ok, User}` or `{error, _}` |
| `nova_auth_session:delete_session_token(blog_auth, Token)` | `ok` |
| `nova_auth_actor:store(Req, Actor)` | `ok` |
| `nova_auth_actor:fetch(Req)` | `{ok, Actor}` or `{error, not_found}` |
| `nova_auth_actor:delete(Req)` | `{ok, Req}` |
| `nova_auth_confirm:generate_confirm_token(blog_auth, User)` | `{ok, Token}` |
| `nova_auth_confirm:confirm_user(blog_auth, Token)` | `{ok, User}` or `{error, _}` |
| `nova_auth_reset:generate_reset_token(blog_auth, User)` | `{ok, Token}` |
| `nova_auth_reset:reset_password(blog_auth, Token, Params)` | `{ok, User}` or `{error, _}` |
| `nova_auth:config(blog_auth, Key)` | A config value |

Policies (`nova_auth_policy`): `allow_authenticated()`, `allow_role(Role | [Roles])`, `allow_claim(Key, Values)`, `allow_owner(Field)`, `deny_all()`. Each is `#{action := atom(), condition := fun((Actor, Extra) -> boolean())}`.

Tokens are standard base64: URL-encode them in links. `nova_auth_rate_limit` needs a wrapper plugin in 0.2.0 (`blog_rate_limit`).

## Kura: schemas

[Schemas & Migrations](../data-layer/schemas-migrations.md), [Associations](../data-layer/associations.md). The blog's `blog_post`, without its changeset and hooks:

```erlang
-module(blog_post).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0, embeds/0]).

table() -> ~"posts".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = title, type = string, nullable = false},
        #kura_field{name = body, type = text, nullable = false},
        #kura_field{
            name = status,
            type = {enum, [draft, published, archived]},
            nullable = false,
            default = draft
        },
        #kura_field{name = user_id, type = bigint, nullable = false},
        #kura_field{name = published_at, type = utc_datetime},
        #kura_field{name = metadata, type = jsonb},
        #kura_field{name = lock_version, type = integer, nullable = false, default = 1},
        #kura_field{name = deleted_at, type = utc_datetime},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[user_id], #{}},
        {[status], #{}}
    ].

associations() ->
    [
        #kura_assoc{
            name = author,
            type = belongs_to,
            schema = blog_user,
            foreign_key = user_id,
            on_delete = cascade
        },
        #kura_assoc{name = comments, type = has_many, schema = blog_comment, foreign_key = post_id},
        #kura_assoc{
            name = tags,
            type = many_to_many,
            schema = blog_tag,
            join_through = blog_post_tag,
            join_keys = {post_id, tag_id}
        }
    ].

embeds() ->
    [#kura_embed{name = metadata, type = embeds_one, schema = blog_post_metadata}].
```

| Type | PostgreSQL | Erlang |
|---|---|---|
| `id` | `BIGSERIAL` | integer |
| `integer`, `bigint`, `smallint` | `INTEGER`, `BIGINT`, `SMALLINT` | integer |
| `float`, `decimal` | `DOUBLE PRECISION`, `NUMERIC` | number |
| `string` | `VARCHAR(255)` | binary |
| `text`, `binary` | `TEXT`, `BYTEA` | binary |
| `boolean` | `BOOLEAN` | `true` / `false` |
| `date`, `time` | `DATE`, `TIME` | `{Y, M, D}`, `{H, Mi, S}` |
| `utc_datetime`, `naive_datetime` | `TIMESTAMPTZ`, `TIMESTAMP` | `{{Y, M, D}, {H, Mi, S}}` |
| `uuid` | `UUID` | binary |
| `jsonb` | `JSONB` | map or list |
| `{enum, [atom()]}` | `VARCHAR(255)` | atom (pass binaries in `where`) |
| `{array, Type}` | `Type[]` | list |
| `{encrypted, Type}` | `BYTEA` | the inner type |

Virtual fields: `virtual = true`. An embed also needs a `jsonb` field of the same name in `fields/0`, and its module drops `-behaviour(kura_schema)`.

## Kura: changesets

[Changesets & Validation](../data-layer/changesets.md).

```erlang
CS = kura_changeset:cast(blog_post, Data, Params, [title, body, status]),
CS1 = kura_changeset:validate_required(CS, [title, body]),
CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
CS3 = kura_changeset:foreign_key_constraint(CS2, user_id),
CS4 = kura_changeset:cast_embed(CS3, metadata, #{with => fun blog_post_metadata:changeset/2}).
```

| Function | Purpose |
|---|---|
| `cast(Schema \| Types, Data, Params, Fields)` | Start a changeset; a types map makes it schemaless |
| `validate_required/2`, `validate_format/3`, `validate_length/3` | Presence, regex, length (`min`, `max`, `is`; counts bytes) |
| `validate_number/3` | `greater_than`, `less_than`, `greater_than_or_equal_to`, `less_than_or_equal_to`, `equal_to` |
| `validate_inclusion/3`, `validate_exclusion/3`, `validate_subset/3` | Value in / not in a list |
| `validate_confirmation/2,3` | `<field>_confirmation` matches |
| `validate_change(CS, Field, Fun)` | `Fun(Value) -> ok \| {error, Message}` |
| `unique_constraint/2,3`, `foreign_key_constraint/2,3`, `check_constraint/3,4` | Turn database errors into changeset errors |
| `optimistic_lock(CS, Field)` | Stale updates return `{error, stale}` |
| `cast_assoc/2,3`, `put_assoc/3`, `cast_embed/2,3` | Nested data |
| `get_change/2,3`, `get_field/2,3`, `put_change/3`, `add_error/3` | Read and change |
| `apply_changes/1`, `apply_action(CS, Action)` | Without the database |
| `traverse_errors(CS, Fun)` | Errors grouped by field |

`CS#kura_changeset.valid`, `.errors` (`[{Field, Message}]`) and `.changes` are record fields; include `kura/include/kura.hrl`.

## Kura: queries

[CRUD & the Query Builder](../data-layer/crud.md), [Advanced Queries](../data-layer/advanced-queries.md).

```erlang
Q = kura_query:from(blog_post),
Q1 = kura_query:where(Q, {'and', [{status, ~"published"}, {deleted_at, is_nil}]}),
Q2 = kura_query:order_by(Q1, [{inserted_at, desc}]),
Q3 = kura_query:limit(kura_query:offset(Q2, 20), 10),
{ok, Posts} = blog_repo:all(kura_query:preload(Q3, [author, {comments, [author]}])).
```

| Condition | SQL |
|---|---|
| `{Field, Value}` | `= $1` |
| `{Field, '!=', V}`, `'<'`, `'>'`, `'<='`, `'>='` | comparison |
| `{Field, in, List}`, `{Field, not_in, List}` | `IN`, `NOT IN` |
| `{Field, between, {Low, High}}` | `BETWEEN` |
| `{Field, like, P}`, `{Field, ilike, P}` | `LIKE`, `ILIKE` |
| `{Field, is_nil}`, `{Field, is_not_nil}` | `IS NULL`, `IS NOT NULL` |
| `{'and', [...]}`, `{'or', [...]}`, `{'not', C}` | boolean logic |
| `{fragment, SQL, Params}` | raw SQL with `?` placeholders, parameters bound |

Also: `select/2`, `select_expr/2`, `join/4,5`, `group_by/2`, `having/2`, `distinct/1,2`, `lock/2`, `with_cte/3`, `union/2`, `union_all/2`, `over/2`, `with_deleted/1`, `only_deleted/1`. Columns are unqualified, so avoid names both sides of a join share.

## Kura: the repo

[CRUD](../data-layer/crud.md), [Transactions & Bulk Operations](../data-layer/transactions-bulk.md), [Data Safety](../data-layer/data-safety.md).

| Call | Returns |
|---|---|
| `blog_repo:all(Q)` | `{ok, [Row]}` |
| `blog_repo:get(Schema, Id)`, `get_by(Schema, Clauses)`, `one(Q)` | `{ok, Row}` or `{error, not_found}` |
| `blog_repo:exists(Q)`, `count(Q)` | `{ok, boolean()}`, `{ok, N}` |
| `blog_repo:insert(CS)`, `update(CS)`, `delete(CS)` | `{ok, Row}` or `{error, CS}`; `update/1` also `{error, stale}` |
| `blog_repo:insert(CS, #{on_conflict => {Target, Action}})` | Upsert; `Action` is `nothing`, `replace_all` or `{replace, Fields}` |
| `blog_repo:insert_all(Schema, Rows)`, `insert_all/3` | `{ok, Count}` |
| `blog_repo:update_all(Q, Changes)`, `delete_all(Q)` | `{ok, Count}` |
| `blog_repo:soft_delete(CS)`, `restore(CS)` | `{ok, Row}` |
| `blog_repo:preload(Schema, Rows, Assocs)` | rows with associations |
| `blog_repo:query(SQL, Params)` | `{ok, Rows}` (jsonb as text) |
| `blog_repo:transaction(Fun)` | `Fun`'s result; only an exception rolls back |
| `blog_repo:multi(Multi)` | `{ok, Results}` or `{error, Step, Reason, Completed}` |

```erlang
M = kura_multi:update(kura_multi:new(), post, blog_post:changeset(Post, #{status => published})),
M1 = kura_multi:run(M, notify, fun(#{post := Published}) -> {ok, Published} end),
{ok, #{post := _, notify := _}} = blog_repo:multi(M1).
```

`count/1`, `aggregate/2`, `kura_stream` and raw `query/2` see soft-deleted rows: add `{deleted_at, is_nil}` yourself.

## Arizona live views

[Arizona Fundamentals](../arizona/fundamentals.md), [Live Views](../arizona/live-views.md), [Events](../arizona/events.md).

```erlang
-module(blog_live_counter).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(_Bindings) ->
    {#{id => ~"counter", title => ~"Counter", count => 0}, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {span, [{id, ~"count"}], [?get(count)]},
            {button, [{id, ~"inc"}, {az_click, arizona_js:push_event(~"inc")}], [~"+"]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.
```

| Callback | Returns |
|---|---|
| `mount(Bindings)` | `{Bindings, Resets}` (runs for the HTTP render and again on connect) |
| `render(Bindings)` | `?html(...)` |
| `handle_event(Event, Payload, Bindings)` | `{Bindings, Resets, Effects}` |
| `handle_info(Message, Bindings)` | `{Bindings, Resets, Effects}` |
| `handle_update(Props, Bindings, Effects)` | `{Bindings, Resets, Effects}` (new props, or a `patch`) |
| `unmount(Bindings)` | anything |

| Macro | Use |
|---|---|
| `?html(Template)` | `{Tag, Attrs, Children}`, binaries (escaped), lists |
| `?get(Key)`, `?get(Key, Default)` | A binding; a missing key raises without a default |
| `?each(Fun, ListOrStream)` | Render each item |
| `?stateless(Mod, Fun, Props)`, `?stateless(Fun, Props)` | Function component |
| `?stateful(Mod, Props)` | Stateful component; `Props` needs a unique `id` |
| `?raw(Html)` | Unescaped HTML, trusted content only |
| `?connected` | `true` in the live process, `false` for the HTTP render |
| `?send(Msg)`, `?send_after(Ms, Msg)` | Message this view (reads `id` from a variable named `Bindings`) |
| `?send(ViewId, Msg)` | Message another view |

Attributes: `az_click`, `az_submit`, `az_change`, `az_input`, `az_keydown`, `az_focusout` take an `arizona_js` command; `az_debounce`, `az_throttle`, `az_key` tune them; `az_navigate` and `az_patch` on links. Effects (`arizona_js`): `push_event/1,2`, `toggle/1`, `show/1`, `hide/1`, `add_class/2`, `remove_class/2`, `toggle_class/2`, `set_attr/3`, `remove_attr/2`, `focus/1`, `blur/1`, `scroll_to/1`, `reset_form/1`, `set_title/1`, `navigate/1,2`, `patch/1`, `dispatch_event/2`. Streams: `arizona_stream:new/2`, `insert/2`, `update/3`, `to_list/1`.

Routes go in the `arizona_nova` env:

```erlang
{arizona_nova, [{routes, [
    {live, ~"/live/posts/:id", blog_live_post, #{
        layouts => [{blog_live_layout, render}],
        middlewares => [{blog_live_middleware, path_bindings}, {arizona_middleware, fetch_flash}]
    }},
    {ws, ~"/live/ws", #{}},
    {asset, ~"/assets/arizona", {priv_dir, arizona, "static/assets/js"}}
]}]}
```

A middleware is `Fun(Req, Bindings) -> {cont, Req, Bindings} | {halt, Req}`. Arizona routes bypass Nova's plugins and security; read the user with `blog_security:session_actor(arizona_req:raw(Req))`, which checks the session's token in the database ([Security](../production/security.md#live-views-verify-the-token)). Arizona needs OTP 29.

## WebSockets

[WebSockets](../real-time/websockets.md).

```erlang
-module(blog_echo_ws).
-behaviour(nova_websocket).
-export([init/1, websocket_handle/2, websocket_info/2]).

init(State) -> {ok, State#{received => 0}}.
websocket_handle({text, Text}, State) -> {reply, {text, Text}, State};
websocket_handle(_Frame, State) -> {ok, State}.
websocket_info(_Message, State) -> {ok, State}.
```

| Callback | Notes |
|---|---|
| `init(State)` | During the upgrade; `State` has `req` and `auth_data`. `{ok, State}` accepts |
| `websocket_init(State)` | Optional; in the connection process, subscribe here |
| `websocket_handle(Frame, State)` | `{text, Bin}`, `{binary, Bin}`, `ping`, `pong` |
| `websocket_info(Message, State)` | Any Erlang message |
| `terminate(Reason, PartialReq, State)` | Optional |

Returns: `{ok, State}`, `{reply, Frame | [Frame], State}`, `{stop, State}`, each optionally with a trailing `hibernate`. Route: `{"/ws/echo", blog_echo_ws, #{protocol => ws}}`.

## nova_pubsub

[Pub/Sub](../real-time/pubsub.md).

| Call | Returns |
|---|---|
| `nova_pubsub:join(Channel)`, `join(Channel, Pid)` | `ok` |
| `nova_pubsub:leave(Channel)`, `leave(Channel, Pid)` | `ok` or `not_joined` |
| `nova_pubsub:broadcast(Channel, Topic, Payload)` | `ok`, every node |
| `nova_pubsub:local_broadcast(Channel, Topic, Payload)` | `ok`, this node only |
| `nova_pubsub:get_members(Channel)`, `get_local_members(Channel)` | `[pid()]` |

Subscribers receive `#nova_pubsub{channel, sender, topic, payload}` from `nova/include/nova_pubsub.hrl`. The sender gets its own broadcasts too.

## Hikyaku email

[Sending Email](../email/sending-email.md).

```erlang
-include_lib("hikyaku/include/hikyaku.hrl").

E0 = hikyaku_email:new(),
E1 = hikyaku_email:from(E0, {~"Nova Blog", ~"noreply@blog.test"}),
E2 = hikyaku_email:to(E1, ~"alice@example.com"),
E3 = hikyaku_email:subject(E2, ~"Your posts export"),
E4 = hikyaku_email:text_body(E3, ~"Attached."),
E5 = hikyaku_email:attachment(E4, hikyaku_attachment:from_data(~"posts.csv", Csv)),
{ok, _Receipt} = blog_mailer:deliver(E5).
```

| Function | Notes |
|---|---|
| `from/2`, `reply_to/2` | Address: `~"a@b"`, `{Name, Address}` or `#hikyaku_address{}` |
| `to/2`, `cc/2`, `bcc/2` | Add one; `put_to/2`, `put_cc/2`, `put_bcc/2` replace with a list |
| `subject/2`, `text_body/2`, `html_body/2` | Binaries |
| `header/3`, `provider_option/3`, `attachment/2` | |
| `hikyaku_attachment:from_data(Filename, Data)`, `from_data/3` | In-memory file; opts `content_type`, `filename` |
| `hikyaku_attachment:from_path(Path)`, `from_path/2` | File on disk |
| `hikyaku_attachment:inline(Cid, Path)` | Inline image referenced as `cid:` |

There are no getters: match `#hikyaku_email{subject = S, to = To, ...}`. A mailer implements `-behaviour(hikyaku_mailer)` with `config/0` returning `#{adapter => Module, ...}`, and `hikyaku_mailer:deliver(Mailer, Email)` returns `{ok, Receipt}` or `{error, Reason}`. Adapters: `hikyaku_adapter_smtp`, `_mailgun`, `_sendgrid`, `_ses`, `_logger` (`level`), `_test` (`pid`, sends `{hikyaku_email, Email}`).

## Shigoto jobs

[Background Jobs](../jobs/background-jobs.md), [Scheduling](../jobs/scheduling.md), [Operations](../jobs/operations.md).

```erlang
-module(blog_comment_notification_worker).
-behaviour(shigoto_worker).
-export([perform/1, queue/0, max_attempts/0]).

queue() -> ~"mailers".
max_attempts() -> 5.

perform(#{~"comment_id" := CommentId}) ->
    ok.
```

`perform/1` returns `ok`, `{ok, Result}`, `{error, Reason}` (retry) or `{snooze, Seconds}` (snoozing uses an attempt). Optional callbacks: `queue/0`, `max_attempts/0` (3), `priority/0`, `timeout/0` (ms), `backoff(Attempt, Reason)` (seconds), `unique/0`, `tags/0`, `on_discard/2`, `middleware/0`, `rate_limit/0`, `concurrency/0`, `global_concurrency/0`, `circuit_breaker/0`.

```erlang
{ok, #{id := Id}} = shigoto:insert(#{
    worker => blog_publish_worker,
    args => #{~"post_id" => PostId},
    scheduled_at => {{2026, 12, 1}, {9, 0, 0}}
}).
```

| Call | Returns |
|---|---|
| `shigoto:insert(Job)`, `insert(Job, Opts)` | `{ok, Job}`, `{ok, {conflict, Existing}}` for a unique duplicate, or `{error, _}` |
| `shigoto:insert_all(Jobs)` | `{ok, [Job]}` |
| `shigoto:cancel(Id)`, `retry(Id)`, `cancel_by(Filters)`, `retry_by(Filters)` | filters: `worker`, `queue`, `tags`, `args` |
| `shigoto:get_job(Id)` | `{ok, Job}` or `{error, _}` |
| `shigoto:drain_queue(Queue)` | `ok`; runs due jobs now, in the caller |
| `shigoto:pause_queue/1`, `resume_queue/1`, `health/0` | `ok`, `ok`, `{ok, Map}` |

Job keys: `worker`, `args` (binary keys), `queue`, `priority`, `max_attempts`, `tags`, `scheduled_at`, `meta`, `depends_on`, `batch`. Config: `{shigoto, [{repo, blog_repo}, {queues, [{~"default", 10}]}]}`. Enqueue after the Kura transaction commits; `shigoto:transaction/1` does not join a Kura transaction. Cron does not fire in 1.9.12; reschedule from the worker.

## Testing

[Unit Testing](../testing/unit-testing.md), [Integration Testing](../testing/integration-testing.md), [Testing Real-Time](../testing/testing-realtime.md).

| Tool | Calls |
|---|---|
| EUnit | `*_test()`, `*_test_()` generators, `?assert`, `?assertEqual`, `?assertMatch`, `?_assert...`, `{setup, S, C, Tests}`, `{foreach, S, C, Tests}` |
| `nova_test_req` | `new(Method, Path)`, `with_bindings/2`, `with_json/2`, `with_header/3`, `with_query/2` (sets `qs` only), `with_body/2`, `with_auth_data/2`, `with_peer/2` |
| `nova_test.hrl` (unit) | `?assertJsonResponse(Pattern, R)`, `?assertJsonResponse(Status, Pattern, R)`, `?assertStatusResponse/2`, `?assertRedirect/2`, `?assertOkResponse/2` |
| `nova_test` (HTTP) | `start(App, Config)`, `stop/1`, `get/2,3`, `post/2,3`, `put/3`, `patch/3`, `delete/2,3`, `request/4`; opts `json`, `body`, `content_type`, `headers` |
| `nova_test.hrl` (HTTP) | `?assertStatus/2`, `?assertJson/2`, `?assertBody/2`, `?assertHeader/3` |
| `kura_sandbox` | `start()`, `checkout(Repo)`, `checkout(Repo, #{shared => true})`, `allow(Repo, Owner, Pid)`, `checkin(Repo)` |
| `shigoto_testing` | `shigoto:perform_job(Worker, Args)`, `perform_job/3`, `assert_enqueued/1`, `refute_enqueued/1`, `all_enqueued/0,1`, `reset/0`; modes `{shigoto, [{testing, manual \| inline}, {testing_confirm_no_persistence, true}]}` |
| `meck` | `new/1`, `expect/3,4`, `called/3`, `capture/5`, `validate/1`, `unload/1` |

## Commands

| Command | Does |
|---|---|
| `rebar3 new nova blog` | New project from the `nova` template |
| `rebar3 nova serve` | Dev server with reload (needs `inotifywait` on Linux; restart after router edits) |
| `rebar3 nova routes`, `middleware`, `audit`, `config`, `doctor` | Inspect routes, plugin chains, unsecured routes, config, project health |
| `rebar3 nova gen_controller --name pages --actions about,contact` | Controller stub (run `rebar3 fmt` after) |
| `rebar3 nova gen_resource --name tags`, `gen_test --name tags` | Resource controller, JSON schema and route hints; a CT suite to fix up |
| `rebar3 nova openapi --output priv/openapi/openapi.json --title "Blog API" --api-version 1.0.0` | OpenAPI spec from the routes |
| `rebar3 kura setup --name blog_repo` | Repo module and migrations directory |
| `rebar3 kura compile` | Generate migrations from schema changes (also a compile hook) |
| `rebar3 kura check` | Fail on schemas without migrations; run before compiling |
| `rebar3 kura lint_migrations` | Flag drops, renames, type changes |
| `rebar3 kura gen_schemas` | Schemas from an existing database |
| `rebar3 fmt`, `rebar3 fmt --check` | Format with erlfmt, or check |
| `rebar3 xref`, `rebar3 dialyzer` | Cross reference, types |
| `rebar3 eunit [--module=M] [--test=M:F] [--generator=M:F]` | EUnit |
| `rebar3 ct [--suite=S] [--group=G] [--case=C] [--sys_config=F] [--cover]` | Common Test |
| `rebar3 cover --verbose` | Coverage report from the last `--cover` runs |
| `rebar3 as prod release`, `rebar3 as prod tar` | Production release |

# JSON APIs with Generators

In Part II we wrote the posts API by hand. `rebar3_nova` can scaffold the pieces of a resource for you: a controller, a JSON schema, route hints and a test suite. In this chapter we use the generators to add a second resource, a **tags API**, look closely at what they produce, and turn it into working, tested code. On the way we validate request bodies against a JSON schema.

The finished API:

| Method | Path | Success | Errors |
|---|---|---|---|
| `GET` | `/api/tags` | 200, all tags by name | |
| `GET` | `/api/tags/:id` | 200, the tag and its posts | 404 |
| `POST` | `/api/tags` | 201 and a `location` header | 400, 415, 422 |
| `PUT` | `/api/tags/:id` | 200, the renamed tag | 400, 404, 415, 422 |
| `DELETE` | `/api/tags/:id` | 204 | 404 |

Tags already exist in the database. `blog_tag` and the `post_tags` join schema come from [Many-to-Many & Embedded Schemas](../data-layer/advanced-data.md), and `PUT /api/posts/:id/tags` attaches them to posts. What is missing is a way to manage the tags themselves.

## Generating a resource

```shell
rebar3 nova gen_resource --name tags
```

```
===> Created .../blog/src/controllers/blog_tags_controller.erl
===> Created .../blog/priv/schemas/tag.json
===>
Add these routes to your router:

===>   {<<"/tags">>, {blog_tags_controller, list}, #{methods => [get]}}
===>   {<<"/tags/:id">>, {blog_tags_controller, show}, #{methods => [get]}}
===>   {<<"/tags">>, {blog_tags_controller, create}, #{methods => [post]}}
===>   {<<"/tags/:id">>, {blog_tags_controller, update}, #{methods => [put]}}
===>   {<<"/tags/:id">>, {blog_tags_controller, delete}, #{methods => [delete]}}
```

The name is used as given: `--name tags` makes `blog_tags_controller` and, after stripping one trailing `s`, `tag.json`. `--actions list,show` limits the actions, as with `gen_controller`. The generator never overwrites a file that already exists.

### The controller

```erlang
-module(blog_tags_controller).

-export([list/1, show/1, create/1, update/1, delete/1]).

list(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

show(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

create(#{req := _Req} = _NovaReq) ->
    {status, 201, #{}, #{<<"message">> => <<"TODO">>}}.

update(#{req := _Req} = _NovaReq) ->
    {json, #{<<"message">> => <<"TODO">>}}.

delete(#{req := _Req} = _NovaReq) ->
    {status, 204}.
```

```admonish bug
These are the same stubs `gen_controller` writes, with the same bug as in [Controllers](../foundations/controllers.md#generating-controllers): the request map has no `req` key, so every action fails with `function_clause` and a 500. Treat the file as a list of function names and replace the bodies. The generated files are not erlfmt-formatted either, so run `rebar3 fmt` before `rebar3 fmt --check`.
```

### The JSON schema

`priv/schemas/tag.json` is written on one line:

```json
{"$schema":"http://json-schema.org/draft-07/schema#","properties":{"id":{"type":"integer"},"name":{"type":"string"}},"required":["id","name"],"type":"object"}
```

```admonish bug
The schema cannot be used as it is. It requires `id`, which no create request carries. Worse, it declares draft-07, and `nova_json_schemas` (below) validates with jesse 1.8.1, which only supports drafts 3, 4 and 6: every request is rejected with `{schema_unsupported, <<"http://json-schema.org/draft-07/schema#">>}`. We rewrite it as a draft-06 schema. The test `generated_draft07_schema_is_rejected` in the companion app checks this.
```

### The route hints

The printed routes use binary paths and the `{Module, Function}` callback form. Nova 0.18.1 still accepts them, but logs a deprecation warning for every such route at startup:

```
Deprecation warning. Since version: v0.9.24, Message: The {Mod,Fun} format have been deprecated. Use the new format for routes.
```

Write them the way the rest of the router does, with a string path and `fun Module:Function/1`. We will do that in [Routes](#routes) below, after the code they point to exists.

## A context module

As with posts, the controller should not build queries itself. `src/blog_tags.erl` holds the tag operations:

```erlang
-module(blog_tags).

-export([list_tags/0, get_tag/1, get_tag/2, create_tag/1, update_tag/2, delete_tag/1]).

list_tags() ->
    blog_repo:all(kura_query:order_by(kura_query:from(blog_tag), [{name, asc}])).

get_tag(Id) ->
    case kura_types:cast(id, Id) of
        {ok, TagId} when is_integer(TagId) -> blog_repo:get(blog_tag, TagId);
        _ -> {error, not_found}
    end.

get_tag(Id, Preloads) ->
    case get_tag(Id) of
        {ok, Tag} -> {ok, blog_repo:preload(blog_tag, Tag, Preloads)};
        {error, not_found} -> {error, not_found}
    end.

create_tag(Params) ->
    blog_repo:insert(blog_tag:changeset(#{}, Params)).

update_tag(Tag, Params) ->
    blog_repo:update(blog_tag:changeset(Tag, Params)).

delete_tag(Tag) ->
    blog_repo:delete(kura_changeset:cast(blog_tag, Tag, #{}, [])).
```

`get_tag/1` works like `blog_posts:get_post/1`: the id arrives as a binary from the path, and anything that does not cast to an integer is simply "not found". `blog_tag:changeset/2` already validates the name (required, at most 30 characters, lowercase letters, digits and dashes), and because `blog_tag:indexes/0` declares a unique index on `name`, a duplicate comes back as a changeset error without any extra code (see [Constraints](../data-layer/changesets.md#constraints)).

Deleting a tag also removes its rows in `post_tags`; the join table's foreign key was created with `on_delete = cascade`. The posts themselves stay.

## The controller

Here is the controller after filling in the stubs:

```erlang
-module(blog_tags_controller).
-fallback_controller(blog_api_fallback_controller).

-export([list/1, show/1, create/1, update/1, delete/1]).

list(_Req) ->
    {ok, Tags} = blog_tags:list_tags(),
    {json, #{tags => [blog_api_json:tag(Tag) || Tag <- Tags]}}.

show(#{bindings := #{~"id" := Id}}) ->
    maybe
        {ok, Tag} ?= blog_tags:get_tag(Id, [posts]),
        {json, blog_api_json:tag(Tag)}
    end.

create(#{json := Params}) ->
    maybe
        {ok, #{id := Id} = Tag} ?= blog_tags:create_tag(Params),
        Location = iolist_to_binary([~"/api/tags/", integer_to_binary(Id)]),
        {json, 201, #{~"location" => Location}, blog_api_json:tag(Tag)}
    end.

update(#{bindings := #{~"id" := Id}, json := Params}) ->
    maybe
        {ok, Tag} ?= blog_tags:get_tag(Id),
        {ok, Updated} ?= blog_tags:update_tag(Tag, Params),
        {json, blog_api_json:tag(Updated)}
    end.

delete(#{bindings := #{~"id" := Id}}) ->
    maybe
        {ok, Tag} ?= blog_tags:get_tag(Id),
        {ok, _} ?= blog_tags:delete_tag(Tag),
        {status, 204}
    end.
```

Every action is the happy path. When a step fails, the `maybe` expression returns that step's value: `{error, not_found}` from `get_tag`, or `{error, Changeset}` from an insert or update. Nova has no handler for `error` tuples, so it passes them to the module named in `-fallback_controller` (introduced in [Controllers](../foundations/controllers.md#fallback-controllers)). One fallback for the whole API keeps the error responses consistent:

```erlang
-module(blog_api_fallback_controller).
-include_lib("kernel/include/logger.hrl").
-include_lib("kura/include/kura.hrl").

-export([resolve/2]).

resolve(_Req, {error, not_found}) ->
    {json, 404, #{}, #{error => ~"not_found"}};
resolve(_Req, {error, #kura_changeset{} = CS}) ->
    {json, 422, #{}, #{errors => blog_api_json:errors(CS)}};
resolve(_Req, Unexpected) ->
    ?LOG_ERROR(#{msg => ~"unexpected controller return", return => Unexpected}),
    {json, 500, #{}, #{error => ~"internal_error"}}.
```

`blog_api_json:errors/1` is the changeset helper from [Rendering errors in JSON responses](../data-layer/changesets.md#rendering-errors-in-json-responses). The [Error Handling](error-handling.md) chapter looks at where else API errors come from.

The JSON for a tag lives next to `post/1` in `src/blog_api_json.erl`. The `posts` key is only there when the posts were preloaded, as in `show/1`:

```erlang
tag(Tag) ->
    Json = #{
        id => maps:get(id, Tag),
        name => maps:get(name, Tag),
        inserted_at => datetime(maps:get(inserted_at, Tag))
    },
    case Tag of
        #{posts := Posts} when is_list(Posts) -> Json#{posts => [post(Post) || Post <- Posts]};
        #{} -> Json
    end.
```

Add `tag/1` to the module's export list.

```admonish note title="Who may change tags?"
Like the posts API at this stage, the tags API has `security => false`, so anyone can create and delete tags. Part III adds authentication and decides who may manage them; `rebar3 nova audit` (next chapter) lists every write route that is still open.
```

## Validating request bodies with JSON schemas

A changeset checks values: is the name short enough, is it unique. Before that, it is worth rejecting bodies that have the wrong *shape*: an array instead of an object, a number where a string belongs, fields the API does not know. That is what JSON schemas are for, and [`nova_json_schemas`](https://github.com/novaframework/nova_json_schemas) is a Nova plugin that checks a decoded body against one. Nova itself does not ship schema validation, so add the dependency to `rebar.config`:

```erlang
{deps, [
    ...
    {nova_json_schemas, "~> 0.3"},
    ...
]}.
```

and `nova_json_schemas` to `applications` in `src/blog.app.src` so it is part of the release. It pulls in [jesse](https://github.com/for-GET/jesse), the validator.

```admonish note
The Hex package of `nova_json_schemas` 0.3.0 declares a dependency on nova `0.13.7`. Rebar3 always prefers the version the project itself asks for, so the blog keeps nova 0.18.1, and the plugin works with it.
```

Replace the generated `priv/schemas/tag.json` with a schema for what clients send:

```json
{
  "$schema": "http://json-schema.org/draft-06/schema#",
  "title": "tag",
  "type": "object",
  "properties": {
    "name": {"type": "string", "description": "Lowercase letters, digits and dashes"}
  },
  "required": ["name"],
  "additionalProperties": false
}
```

The schema describes the body: an object with a string `name` and nothing else. The rules about *which* strings are allowed stay in `blog_tag:changeset/2`, so they live in one place, and they produce field-level messages.

### Routes

The tags get their own route group in `src/blog_router.erl`, placed after the posts group and before the last group (static files and status codes):

```erlang
#{
    prefix => "/api",
    security => false,
    plugins => [
        {pre_request, nova_request_plugin, #{decode_json_body => true}},
        {pre_request, blog_require_json_plugin, #{}},
        {pre_request, nova_json_schemas, #{render_errors => false}}
    ],
    routes => [
        {"/tags", fun blog_tags_controller:list/1, #{methods => [get]}},
        {"/tags/:id", fun blog_tags_controller:show/1, #{methods => [get]}},
        {"/tags", fun blog_tags_controller:create/1, #{
            methods => [post], extra_state => #{json_schema => "schemas/tag.json"}
        }},
        {"/tags/:id", fun blog_tags_controller:update/1, #{
            methods => [put], extra_state => #{json_schema => "schemas/tag.json"}
        }},
        {"/tags/:id", fun blog_tags_controller:delete/1, #{methods => [delete]}},
        {"/openapi.json", fun blog_api_docs_controller:openapi/1, #{methods => [get]}}
    ]
},
```

(The `/openapi.json` route belongs to the [next chapter](openapi-tools.md#serving-the-spec).)

A route opts in to validation with `extra_state => #{json_schema => Path}`, where the path is relative to the application's `priv` directory. Nova copies a route's `extra_state` into the request map, which is where the plugin looks.

A group's `plugins` replace the global ones (see [Plugins](../foundations/plugins.md)), so the list starts with `nova_request_plugin`. The order matters: plugins run top to bottom, and the schema plugin needs the `json` key that the request plugin adds.

```admonish warning
The `nova_json_schemas` README says the request plugin must come *after* it because "Nova runs the plugins bottom-up". That is not true for Nova 0.18.1, which runs them in list order. Put `nova_request_plugin` first.
```

### Requests without a JSON body

When a request has no decoded body (no body at all, a form post, `text/plain`), there is no `json` key, and `nova_json_schemas` returns `{error, body_not_parsed}`. Nova 0.18.1 does not handle an `{error, _}` return from a plugin: the request process crashes and the client gets an empty 500. `blog_require_json_plugin` sits between the two plugins and answers such write requests itself:

```erlang
-module(blog_require_json_plugin).
-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(#{json := _} = Req, _Env, _Options, State) ->
    {ok, Req, State};
pre_request(#{method := Method} = Req, _Env, _Options, State) when
    Method =:= ~"POST"; Method =:= ~"PUT"; Method =:= ~"PATCH"
->
    Body = json:encode(#{error => ~"expected a JSON body"}),
    {stop, {reply, 415, #{~"content-type" => ~"application/json"}, Body}, Req, State};
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"Require a JSON body",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"The Nova Book"],
        description => ~"Answers 415 when a write request has no decoded JSON body",
        options => []
    }.
```

Returning `{stop, {reply, Status, Headers, Body}, Req, State}` sends that response and skips the rest of the chain, controller included. `415 Unsupported Media Type` tells the client what is wrong. GET and DELETE requests pass through untouched. [Custom Plugins & CORS](../production/plugins-cors.md) covers writing plugins in depth.

### Rendering schema errors

With `render_errors => true` the plugin answers a failed validation with a JSON list of jesse errors. Here is a body with an unknown field (trimmed path):

```
HTTP/1.1 400 Bad Request
content-type: application/json

[{"error_type":"no_extra_properties_allowed","actual_value":{"title":"x"},"error_context":"schema_violation","expected_value":["title"],"field_info":{"$id":"file:///.../blog/schemas/tag.json","$schema":"http://json-schema.org/draft-06/schema#","additionalProperties":false,"properties":{"name":{"description":"Lowercase letters, digits and dashes","type":"string"}},"required":["name"],"title":"tag","type":"object"}}]
```

```admonish warning
For errors about the whole object, `field_info` contains the entire schema, and its `$id` is the **absolute path on the server** (jesse sets it, whatever you write in the file). The labels are also misleading (`expected_value` holds the path of the offending value) and only the first error is reported. The blog sets `render_errors => false`, so a shape error is a plain `400` with no body, and leaves the detailed messages to the changeset.
```

## Trying it

Restart the node (router changes need a restart) and create two tags:

```shell
curl -i -X POST localhost:8080/api/tags -H 'content-type: application/json' -d '{"name":"erlang"}'
```

```
HTTP/1.1 201 Created
content-length: 61
content-type: application/json
location: /api/tags/1
...

{"id":1,"name":"erlang","inserted_at":"2026-11-03T08:22:09Z"}
```

```shell
curl -s -X POST localhost:8080/api/tags -H 'content-type: application/json' -d '{"name":"nova"}'
curl -s localhost:8080/api/tags
```

```json
{"tags":[{"id":1,"name":"erlang","inserted_at":"2026-11-03T08:22:09Z"},{"id":2,"name":"nova","inserted_at":"2026-11-03T08:22:09Z"}]}
```

`show` includes the tag's posts, here none yet. Tag a post with `PUT /api/posts/:id/tags` and it appears in the list:

```shell
curl -s localhost:8080/api/tags/1
```

```json
{"id":1,"name":"erlang","inserted_at":"2026-11-03T08:22:09Z","posts":[]}
```

Each layer rejects what it is responsible for. The changeset rejects a duplicate and a badly formed name, with messages per field:

```shell
curl -i -X POST localhost:8080/api/tags -H 'content-type: application/json' -d '{"name":"erlang"}'
```

```
HTTP/1.1 422 Unprocessable Entity
content-length: 46
content-type: application/json
...

{"errors":{"name":["has already been taken"]}}
```

```shell
curl -s -X POST localhost:8080/api/tags -H 'content-type: application/json' -d '{"name":"Not Valid!"}'
```

```json
{"errors":{"name":["has invalid format"]}}
```

The schema rejects a body of the wrong shape, the require-JSON plugin a body that is not JSON, and the request plugin JSON that does not parse:

```shell
curl -i -X POST localhost:8080/api/tags -H 'content-type: application/json' -d '{"title":"erlang"}'
```

```
HTTP/1.1 400 Bad Request
content-length: 0
...
```

```shell
curl -i -X POST localhost:8080/api/tags -d 'name=erlang'
```

```
HTTP/1.1 415 Unsupported Media Type
content-length: 32
content-type: application/json
...

{"error":"expected a JSON body"}
```

```shell
curl -i -X POST localhost:8080/api/tags -H 'content-type: application/json' -d '{"name":'
```

```
HTTP/1.1 400 Bad Request
content-length: 0
...
```

Renaming, deleting and missing tags:

```shell
curl -s -X PUT localhost:8080/api/tags/2 -H 'content-type: application/json' -d '{"name":"otp"}'
```

```json
{"id":2,"name":"otp","inserted_at":"2026-11-03T08:22:09Z"}
```

```shell
curl -i -X DELETE localhost:8080/api/tags/2
```

```
HTTP/1.1 204 No Content
...
```

```shell
curl -i -X DELETE localhost:8080/api/tags/2
```

```
HTTP/1.1 404 Not Found
content-length: 21
content-type: application/json
...

{"error":"not_found"}
```

A method the path does not support gets `405` with an `allow` header, from Nova's router:

```shell
curl -i -X PATCH localhost:8080/api/tags/1
```

```
HTTP/1.1 405 Method Not Allowed
allow: DELETE, GET, PUT
content-length: 0
...
```

## Generating a test suite

```shell
rebar3 nova gen_test --name tags
```

```
===> Created .../blog/test/blog_tags_controller_SUITE.erl
```

The suite it writes:

```erlang
-module(blog_tags_controller_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_list/1, test_show/1, test_create/1, test_update/1, test_delete/1]).

all() ->
    [test_list, test_show, test_create, test_update, test_delete].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(blog),
    Config.

end_per_suite(_Config) ->
    application:stop(blog),
    ok.

test_list(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request("http://localhost:8080/tags").

test_show(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request("http://localhost:8080/tags/1").

test_create(_Config) ->
    {ok, {{_, 201, _}, _, _}} = httpc:request(post, {"http://localhost:8080/tags", [], "application/json", "{}"}, [], []).

test_update(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request(put, {"http://localhost:8080/tags/1", [], "application/json", "{}"}, [], []).

test_delete(_Config) ->
    {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {"http://localhost:8080/tags/1", []}, [], []).
```

It compiles, but every case fails:

```shell
rebar3 ct --suite=test/blog_tags_controller_SUITE
```

```
%%% blog_tags_controller_SUITE ==> test_delete: FAILED
%%% blog_tags_controller_SUITE ==> {{badmatch,{error,{failed_connect,[{to_address,{"localhost",8080}},
                                   {inet,[inet],econnrefused}]}}},
...
Failed 5 tests. Passed 0 tests.
```

```admonish bug title="What the generated suite gets wrong"
- The URL is hardcoded to `localhost:8080`, but the test config listens on `8081`.
- The route prefix is missing: the paths are `/tags`, not `/api/tags`.
- `show`, `update` and `delete` assume a tag with id 1 exists.
- `create` and `update` send `{}`, which the schema rejects with a 400.
- The results of `ensure_all_started` are ignored, so a failed start shows up later as a confusing connection error, and nothing isolates the database between tests.
```

Fix it the way the Part II suites work: start the app with `blog_data_fixtures:start/0`, give each test a database sandbox, create the tag the test needs, and use the fixtures' `request/2,3`, which take the port from the config and decode the JSON. Rename the file to `test/blog_api_tags_SUITE.erl` (and the `-module`) so the API suites sit together. The five generated cases become:

```erlang
init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    {ok, Tag} = blog_tags:create_tag(#{~"name" => ~"erlang"}),
    [{tag_path, "/api/tags/" ++ integer_to_list(maps:get(id, Tag))} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

test_list(_Config) ->
    {200, #{~"tags" := [#{~"name" := ~"erlang"}]}} = blog_data_fixtures:request(get, "/api/tags").

test_show(Config) ->
    {200, #{~"name" := ~"erlang", ~"posts" := []}} =
        blog_data_fixtures:request(get, ?config(tag_path, Config)).

test_create(_Config) ->
    {201, #{~"name" := ~"otp", ~"id" := Id}} =
        blog_data_fixtures:request(post, "/api/tags", #{name => ~"otp"}),
    true = is_integer(Id).

test_update(Config) ->
    {200, #{~"name" := ~"beam"}} =
        blog_data_fixtures:request(put, ?config(tag_path, Config), #{name => ~"beam"}).

test_delete(Config) ->
    {204, <<>>} = blog_data_fixtures:request(delete, ?config(tag_path, Config)),
    {404, _} = blog_data_fixtures:request(get, ?config(tag_path, Config)).
```

Then add a case for every error path. For requests the JSON helper cannot make (a wrong content type, broken JSON) the suite has a small `raw/4`:

```erlang
create_without_json_is_415(_Config) ->
    {415, _, Body} = raw(post, "/api/tags", "text/plain", ~"otp"),
    #{~"error" := ~"expected a JSON body"} = json:decode(Body).

create_malformed_json_is_400(_Config) ->
    {400, _, <<>>} = raw(post, "/api/tags", "application/json", ~"{\"name\":").
```

```erlang
raw(Method, Path, ContentType, Body) ->
    {ok, {{_, Status, _}, Headers, RespBody}} =
        httpc:request(
            Method,
            {blog_data_fixtures:url(Path), [], ContentType, Body},
            [],
            [{body_format, binary}]
        ),
    {Status, Headers, RespBody}.
```

The full suite also covers sorting, the `location` header, tagged and soft-deleted posts in `show`, duplicates, invalid names, each kind of schema violation, unknown ids, deleting a tag that posts use, and 405:

```shell
rebar3 ct --suite=test/blog_api_tags_SUITE
```

```
%%% blog_api_tags_SUITE: ..........................
All 26 tests passed.
```

## The workflow

Adding a resource to the blog's API comes down to:

1. Define or reuse the Kura schema; `rebar3 compile` generates the migration.
2. `rebar3 nova gen_resource --name <plural>` for the controller and schema files, then `rebar3 fmt`.
3. Write a context module with the Kura calls.
4. Replace the stubs with happy-path actions and a fallback controller.
5. Rewrite `priv/schemas/<singular>.json` as a draft-06 schema of the request body.
6. Add a route group with the plugins and `json_schema` options; restart the node.
7. `rebar3 nova gen_test --name <plural>`, then fix it as above and cover the error paths.

---

Next, let's [document the API and inspect it](openapi-tools.md) with the rest of the `rebar3 nova` tooling.

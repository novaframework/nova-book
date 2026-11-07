# CRUD and the Query Builder

We have schemas and changesets. Time to read and write rows, and to put a JSON API in front of them.

## The repo functions

Every repo function returns `{ok, Result}` or `{error, Reason}`, and rows come back as maps with atom keys.

```admonish note
The shell sessions in this Part were captured on the finished blog, so post maps also show `metadata`, `lock_version` and `deleted_at`. Those fields arrive in later chapters.
```

### Insert

`insert/1` takes a changeset. If it is invalid, nothing is sent to the database and you get the changeset back with its errors.

```erlang
1> {ok, Draft} = blog_repo:insert(kura_changeset:put_change(blog_post:changeset(#{}, #{~"title" => ~"Working title", ~"body" => ~"First draft."}), user_id, 1)).
{ok,#{id => 6,status => draft,title => <<"Working title">>,
      metadata => undefined,body => <<"First draft.">>,
      user_id => 1,
      inserted_at => {{2026,11,3},{7,48,24}},
      updated_at => {{2026,11,3},{7,48,24}},
      published_at => undefined,lock_version => 1,
      deleted_at => undefined}}
```

The returned map is the row as stored: `id` from the sequence, `status` from the column default, both timestamps set by Kura. `user_id` is set with `put_change/3` because it is not user input; it will come from the logged-in user.

### Get

```erlang
2> blog_repo:get(blog_post, 6).
{ok,#{id => 6,status => draft,title => <<"Working title">>, ...}}
3> blog_repo:get(blog_post, 9999).
{error,not_found}
4> blog_repo:get_by(blog_user, [{username, ~"bob"}]).
{ok,#{id => 2,role => author,username => <<"bob">>, ...}}
```

`get/2` looks up by primary key, `get_by/2` by any fields. `one/1` runs a query and expects exactly one row, `exists/1` only asks whether there is a match:

```erlang
5> blog_repo:exists(kura_query:where(kura_query:from(blog_user), {email, ~"bob@example.com"})).
{ok,true}
```

### Update

`update/1` takes a changeset built on the loaded record and writes only the changed fields:

```erlang
6> {ok, Renamed} = blog_repo:update(blog_post:changeset(Draft, #{~"title" => ~"Final title"})).
...
7> maps:with([id, title, updated_at], Renamed).
#{id => 6,title => <<"Final title">>,
  updated_at => {{2026,11,3},{7,48,25}}}
```

`updated_at` moved on its own.

### Delete

`delete/1` also takes a changeset, so lifecycle hooks (see [Data Safety](data-safety.md)) can see what is being deleted. With no changes to make, cast an empty params map:

```erlang
8> blog_repo:delete(kura_changeset:cast(blog_post, Renamed, #{}, [])).
{ok,#{id => 6, ...}}
9> blog_repo:get(blog_post, 6).
{error,not_found}
```

## The query builder

`blog_repo:all/1` runs a query built with `kura_query`. Queries are plain records, so you build them up step by step and nothing touches the database until you call the repo:

```erlang
10> Q = kura_query:from(blog_post).
11> Q1 = kura_query:where(Q, {status, ~"published"}).
12> Q2 = kura_query:order_by(kura_query:select(Q1, [id, title]), [{id, asc}]).
13> blog_repo:all(Q2).
{ok,[#{id => 2,title => <<"Changesets">>},
     #{id => 3,title => <<"Supervision trees">>},
     #{id => 4,title => <<"Pattern matching">>},
     #{id => 5,title => <<"Postgres full text search">>}]}
```

`kura_query_compiler:to_sql/2` shows what Kura sends. Every value is a bind parameter, never spliced into the SQL text:

```erlang
14> kura_query_compiler:to_sql(blog_repo, kura_query:limit(Q2, 2)).
{<<"SELECT \"id\", \"title\" FROM \"posts\" WHERE \"status\" = $1 ORDER BY \"id\" ASC LIMIT $2">>,
 [<<"published">>,2]}
```

The builder functions:

| Function | SQL |
|---|---|
| `from(Schema)` | `FROM` |
| `where(Q, Condition)` | `WHERE` (several calls are combined with `AND`) |
| `select(Q, Fields)` | column list (default: all fields) |
| `order_by(Q, [{Field, asc \| desc}])` | `ORDER BY` |
| `limit(Q, N)`, `offset(Q, N)` | `LIMIT`, `OFFSET` |
| `distinct(Q)` | `SELECT DISTINCT` |
| `preload(Q, Assocs)` | load associations (next chapter) |

Conditions:

| Condition | SQL |
|---|---|
| `{Field, Value}` | `= $1` |
| `{Field, '!=', Value}`, `'<'`, `'>'`, `'<='`, `'>='` | comparisons |
| `{Field, in, List}`, `{Field, not_in, List}` | `IN (...)`, `NOT IN (...)` |
| `{Field, between, {Low, High}}` | `BETWEEN` |
| `{Field, like, Pattern}`, `{Field, ilike, Pattern}` | `LIKE`, `ILIKE` |
| `{Field, is_nil}`, `{Field, is_not_nil}` | `IS NULL`, `IS NOT NULL` |
| `{'and', [...]}`, `{'or', [...]}`, `{'not', C}` | boolean logic |
| `{fragment, SQL, Params}` | raw SQL with `?` placeholders ([Advanced Queries](advanced-queries.md)) |

```erlang
15> blog_repo:all(kura_query:where(kura_query:from(blog_post), {'and', [{user_id, 2}, {title, ilike, ~"%pattern%"}]})).
{ok,[#{id => 4,status => published,title => <<"Pattern matching">>, ...}]}
```

Casting turns `~"published"` into the atom `published`, and loaded rows have atoms. A value in `where` that is compared to a field of the `from` schema goes through the field's type too, so `{status, published}` and `{status, ~"published"}` are the same condition. The blog writes binaries, since that is what arrives from a request. Never turn request input into an atom to build a condition: atoms are never garbage collected.

## A context module

Controllers should not build queries. The blog keeps its post logic in a context module, `src/blog_posts.erl`, and controllers call it. Its first version:

```erlang
-module(blog_posts).
-include_lib("kura/include/kura.hrl").

-export([
    list_posts/1,
    get_post/1,
    create_post/2,
    update_post/2,
    delete_post/1
]).

list_posts(Params) ->
    Types = #{
        page => integer, page_size => integer, status => {enum, [draft, published, archived]}
    },
    CS = kura_changeset:cast(Types, #{}, Params, [page, page_size, status]),
    CS1 = kura_changeset:validate_number(CS, page, [{greater_than, 0}]),
    CS2 = kura_changeset:validate_number(CS1, page_size, [
        {greater_than, 0}, {less_than_or_equal_to, 50}
    ]),
    case kura_changeset:apply_action(CS2, list) of
        {ok, Filters} ->
            Query = filter_by_status(kura_query:from(blog_post), Filters),
            Newest = kura_query:order_by(Query, [{inserted_at, desc}, {id, desc}]),
            kura_paginator:paginate(blog_repo, Newest, #{
                page => maps:get(page, Filters, 1),
                page_size => maps:get(page_size, Filters, 10)
            });
        {error, _} = Error ->
            Error
    end.

filter_by_status(Query, #{status := Status}) ->
    kura_query:where(Query, {status, atom_to_binary(Status)});
filter_by_status(Query, #{}) ->
    Query.

get_post(Id) ->
    case kura_types:cast(id, Id) of
        {ok, PostId} when is_integer(PostId) -> blog_repo:get(blog_post, PostId);
        _ -> {error, not_found}
    end.

create_post(#{id := AuthorId}, Params) ->
    CS = blog_post:changeset(#{}, Params),
    blog_repo:insert(kura_changeset:put_change(CS, user_id, AuthorId)).

update_post(Post, Params) ->
    blog_repo:update(blog_post:changeset(Post, Params)).

delete_post(Post) ->
    blog_repo:delete(kura_changeset:cast(blog_post, Post, #{}, [])).
```

- `list_posts/1` finishes what the previous chapter started. The validated filters build the query, and `kura_paginator:paginate/3` runs it twice: once for the page of entries, once for the total count. It returns `#{entries, page, page_size, total_entries, total_pages}`.
- The status filter goes through `atom_to_binary/1` because of the enum rule above. `Status` is one of three known atoms here, never raw input.
- `get_post/1` takes the id straight from the URL. `kura_types:cast(id, Id)` turns `~"42"` into `42` and anything else into an error, so `/api/posts/abc` is a 404 rather than a crash.
- `create_post/2` takes the author as a record. Where the author comes from is the controller's business.

```admonish tip
For very large tables, offset pagination gets slower the deeper you page. The [Advanced Queries](advanced-queries.md#cursor-pagination) chapter shows cursor pagination.
```

## The posts controller

`src/blog_api_json.erl` turns records into maps the JSON encoder understands. Its `post/1` at this stage:

```erlang
post(Post) ->
    #{
        id => maps:get(id, Post),
        title => maps:get(title, Post),
        body => maps:get(body, Post),
        status => maps:get(status, Post),
        user_id => maps:get(user_id, Post),
        published_at => datetime(maps:get(published_at, Post)),
        inserted_at => datetime(maps:get(inserted_at, Post))
    }.

datetime(undefined) ->
    null;
datetime({{_, _, _}, {_, _, _}} = DateTime) ->
    Seconds = erlang:universaltime_to_posixtime(DateTime),
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{offset, "Z"}])).
```

Listing the fields explicitly means a new column is never exposed by accident. Datetimes become RFC 3339 strings and `undefined` becomes `null`.

The controller, `src/controllers/blog_posts_controller.erl`:

```erlang
-module(blog_posts_controller).
-include_lib("kura/include/kura.hrl").

-export([
    index/1,
    show/1,
    create/1,
    update/1,
    delete/1
]).

index(Req) ->
    Params = maps:from_list(cowboy_req:parse_qs(Req)),
    case blog_posts:list_posts(Params) of
        {ok, #{entries := Posts} = Page} ->
            {json, #{
                posts => [blog_api_json:post(Post) || Post <- Posts],
                page => maps:get(page, Page),
                total_pages => maps:get(total_pages, Page),
                total_entries => maps:get(total_entries, Page)
            }};
        {error, #kura_changeset{} = CS} ->
            {json, 400, #{}, #{errors => blog_api_json:errors(CS)}}
    end.

show(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} -> {json, blog_api_json:post(Post)};
        {error, not_found} -> not_found()
    end.

create(#{json := Params} = Req) ->
    case current_author(Req) of
        {ok, Author} ->
            case blog_posts:create_post(Author, Params) of
                {ok, Post} -> {json, 201, #{}, blog_api_json:post(Post)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {error, not_found} ->
            {json, 422, #{}, #{errors => #{user_id => [~"does not exist"]}}}
    end;
create(_Req) ->
    {json, 400, #{}, #{error => ~"expected a JSON body"}}.

update(#{bindings := #{~"id" := Id}, json := Params}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            case blog_posts:update_post(Post, Params) of
                {ok, Updated} -> {json, blog_api_json:post(Updated)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {error, not_found} ->
            not_found()
    end;
update(_Req) ->
    {json, 400, #{}, #{error => ~"expected a JSON body"}}.

delete(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id) of
        {ok, Post} ->
            {ok, _} = blog_posts:delete_post(Post),
            {status, 204};
        {error, not_found} ->
            not_found()
    end.

current_author(#{json := #{~"user_id" := UserId}}) when is_integer(UserId) ->
    blog_repo:get(blog_user, UserId);
current_author(_Req) ->
    {error, not_found}.

not_found() ->
    {json, 404, #{}, #{error => ~"post not found"}}.

unprocessable(CS) ->
    {json, 422, #{}, #{errors => blog_api_json:errors(CS)}}.
```

The `json` key in the request map is the body decoded by `nova_request_plugin` (configured in [Plugins](../foundations/plugins.md)). A request without a JSON body has no `json` key and falls through to the 400 clause.

```admonish warning title="Temporary: the author comes from the body"
`current_author/1` trusts a `user_id` in the JSON body. That is fine for trying out the API locally and nowhere else: anyone could post as anyone. [Authentication](../auth-sessions/authentication.md#api-tokens) removes it: the write routes then require a bearer token, and the author is the user the token belongs to.
```

## Routes

Add a route group for the API to `src/blog_router.erl`, before the last group (the one with the static files and the 404 handler):

```erlang
#{
    prefix => "/api",
    security => false,
    routes => [
        {"/posts", fun blog_posts_controller:index/1, #{methods => [get]}},
        {"/posts", fun blog_posts_controller:create/1, #{methods => [post]}},
        {"/posts/:id", fun blog_posts_controller:show/1, #{methods => [get]}},
        {"/posts/:id", fun blog_posts_controller:update/1, #{methods => [put]}},
        {"/posts/:id", fun blog_posts_controller:delete/1, #{methods => [delete]}}
    ]
},
```

Restart the node after editing the router (see [Routing](../foundations/routing.md)).

## Trying it with curl

We need a user first. In the shell:

```erlang
1> blog_repo:insert(blog_user:registration_changeset(#{}, #{~"email" => ~"alice@example.com", ~"username" => ~"alice", ~"password" => ~"correct horse battery", ~"password_confirmation" => ~"correct horse battery"})).
{ok,#{id => 1,role => author,username => <<"alice">>, ...}}
```

Create two posts:

```shell
curl -s -X POST localhost:8080/api/posts -H 'content-type: application/json' -d '{"user_id": 1, "title": "Hello Kura", "body": "Kura is the data layer of the blog."}'
```

```json
{"id":1,"status":"draft","title":"Hello Kura","body":"Kura is the data layer of the blog.","user_id":1,"published_at":null,"inserted_at":"2026-11-03T07:51:15Z"}
```

```shell
curl -s -X POST localhost:8080/api/posts -H 'content-type: application/json' -d '{"user_id": 1, "title": "Changesets", "body": "Cast, validate, persist.", "status": "published"}'
```

```json
{"id":2,"status":"published","title":"Changesets","body":"Cast, validate, persist.","user_id":1,"published_at":"2026-11-03T07:51:15Z","inserted_at":"2026-11-03T07:51:15Z"}
```

List them, newest first, and filter:

```shell
curl -s localhost:8080/api/posts
```

```json
{"page":1,"total_pages":1,"total_entries":2,"posts":[{"id":2,"status":"published","title":"Changesets","body":"Cast, validate, persist.","user_id":1,"published_at":"2026-11-03T07:51:15Z","inserted_at":"2026-11-03T07:51:15Z"},{"id":1,"status":"draft","title":"Hello Kura","body":"Kura is the data layer of the blog.","user_id":1,"published_at":null,"inserted_at":"2026-11-03T07:51:15Z"}]}
```

```shell
curl -s 'localhost:8080/api/posts?status=published&page_size=5'
```

```json
{"page":1,"total_pages":1,"total_entries":1,"posts":[{"id":2,"status":"published","title":"Changesets","body":"Cast, validate, persist.","user_id":1,"published_at":"2026-11-03T07:51:15Z","inserted_at":"2026-11-03T07:51:15Z"}]}
```

Update:

```shell
curl -s -X PUT localhost:8080/api/posts/1 -H 'content-type: application/json' -d '{"title": "Hello again, Kura"}'
```

```json
{"id":1,"status":"draft","title":"Hello again, Kura","body":"Kura is the data layer of the blog.","user_id":1,"published_at":null,"inserted_at":"2026-11-03T07:51:15Z"}
```

Validation errors are a 422 with the errors per field:

```shell
curl -s -i -X POST localhost:8080/api/posts -H 'content-type: application/json' -d '{"user_id": 1, "title": "Hi", "body": ""}'
```

```
HTTP/1.1 422 Unprocessable Entity
content-length: 84
content-type: application/json
...

{"errors":{"title":["should be at least 3 character(s)"],"body":["can't be blank"]}}
```

An unknown author is a 422 too, and a bad query string a 400 from the schemaless changeset:

```shell
curl -s -i -X POST localhost:8080/api/posts -H 'content-type: application/json' -d '{"user_id": 99, "title": "Ghost", "body": "Nobody wrote this."}'
```

```
HTTP/1.1 422 Unprocessable Entity
...

{"errors":{"user_id":["does not exist"]}}
```

```shell
curl -s -i 'localhost:8080/api/posts?page=0&page_size=500'
```

```
HTTP/1.1 400 Bad Request
...

{"errors":{"page":["must be greater than 0"],"page_size":["must be less than or equal to 50"]}}
```

Delete, then the post is gone; a non-numeric id is a 404 as well:

```shell
curl -s -i -X DELETE localhost:8080/api/posts/1
```

```
HTTP/1.1 204 No Content
...
```

```shell
curl -s -i localhost:8080/api/posts/1
```

```
HTTP/1.1 404 Not Found
...

{"error":"post not found"}
```

```shell
curl -s -i localhost:8080/api/posts/abc
```

```
HTTP/1.1 404 Not Found
...

{"error":"post not found"}
```

## Testing against the database

The test suites use `kura_sandbox`: each test case checks out a connection wrapped in a transaction, and checking in rolls it back. Tests start from an empty database and never see each other's rows. The shared helpers in `test/blog_data_fixtures.erl`:

```erlang
start() ->
    ok = application:set_env(nova_auth, pbkdf2_iterations, 1000),
    {ok, _} = application:ensure_all_started(blog),
    {ok, _} = application:ensure_all_started(inets),
    kura_sandbox:start().

checkout() ->
    kura_sandbox:checkout(blog_repo, #{shared => true}).

checkin() ->
    kura_sandbox:checkin(blog_repo).
```

`shared => true` lets every process use the test's sandbox connection, which matters for HTTP tests: the request is handled in a Cowboy process, not in the test process. That also means suites using it must not run test cases in parallel. Lowering `pbkdf2_iterations` keeps user creation fast in tests; never do that in production.

`test/blog_data_crud_SUITE.erl` wires it up per test case and drives the API through `httpc`:

```erlang
init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    [{alice, blog_data_fixtures:user(~"alice")} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

api_create_show_update_delete(Config) ->
    #{id := AliceId} = Alice = ?config(alice, Config),
    Token = blog_data_fixtures:token(Alice),
    {201, #{~"id" := Id, ~"status" := ~"draft", ~"user_id" := AliceId}} =
        blog_data_fixtures:request_as(Token, post, "/api/posts", #{
            title => ~"My First Post", body => ~"Hello from Nova!"
        }),
    Path = "/api/posts/" ++ integer_to_list(Id),
    {200, #{~"title" := ~"My First Post", ~"published_at" := null}} =
        blog_data_fixtures:request_as(Token, get, Path),
    {200, #{~"status" := ~"published", ~"published_at" := <<_/binary>>}} =
        blog_data_fixtures:request_as(Token, put, Path, #{status => ~"published"}),
    {204, <<>>} = blog_data_fixtures:request_as(Token, delete, Path),
    {404, #{~"error" := ~"post not found"}} = blog_data_fixtures:request(get, Path),
    {404, _} = blog_data_fixtures:request(get, "/api/posts/abc").
```

This is the test as it stands at the end of the book: writes need a bearer token (Part III), and a draft is only shown to its author ([Security](../production/security.md#drafts-in-the-json-api)), so the test reads its own draft with the token too. `token/1` issues one for a user and `request_as/3,4` sends it. Until you get there, call `request/2,3` and put `user_id => AliceId` in the body, as the curl examples above do.

Run these two suites with:

```shell
rebar3 ct --suite=test/blog_data_changeset_SUITE,test/blog_data_crud_SUITE
```

---

Posts have authors and comments. Next: [Associations and Preloading](associations.md).

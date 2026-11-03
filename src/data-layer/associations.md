# Associations and Preloading

A post has an author and comments. In this chapter we tell Kura how the tables relate, let it add the foreign keys, and load related rows without writing joins by hand.

## Declaring associations

Associations go in an optional `associations/0` callback that returns `#kura_assoc{}` records. A post **belongs to** its author and **has many** comments. In `blog_post.erl`:

```erlang
-export([table/0, fields/0, indexes/0, associations/0]).

associations() ->
    [
        #kura_assoc{
            name = author,
            type = belongs_to,
            schema = blog_user,
            foreign_key = user_id,
            on_delete = cascade
        },
        #kura_assoc{name = comments, type = has_many, schema = blog_comment, foreign_key = post_id}
    ].
```

- `belongs_to` means this table holds the foreign key (`posts.user_id`). The association is called `author`, which reads better than `user` and is what you will write when preloading.
- `on_delete = cascade` on a `belongs_to` makes the generator add a real foreign key with `ON DELETE CASCADE`: delete a user and their posts go too. The alternatives are `restrict`, `set_null` and `no_action`.
- `has_many` describes the other side: `comments.post_id` points here. It adds nothing to this table.

The user side, in `blog_user.erl`:

```erlang
associations() ->
    [
        #kura_assoc{name = posts, type = has_many, schema = blog_post, foreign_key = user_id},
        #kura_assoc{name = comments, type = has_many, schema = blog_comment, foreign_key = user_id}
    ].
```

Kura also has `has_one` and `has_many` with `through = [...]` for reaching across an intermediate association. We use `many_to_many` in the [next chapter](advanced-data.md).

## The comment schema

`src/schemas/blog_comment.erl`:

```erlang
-module(blog_comment).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, indexes/0, associations/0]).
-export([changeset/2, import_changeset/2]).

table() -> ~"comments".

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = body, type = text, nullable = false},
        #kura_field{name = post_id, type = bigint, nullable = false},
        #kura_field{name = user_id, type = bigint, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].

indexes() ->
    [
        {[post_id], #{}},
        {[user_id], #{}}
    ].

associations() ->
    [
        #kura_assoc{
            name = post,
            type = belongs_to,
            schema = blog_post,
            foreign_key = post_id,
            on_delete = cascade
        },
        #kura_assoc{
            name = author,
            type = belongs_to,
            schema = blog_user,
            foreign_key = user_id,
            on_delete = cascade
        }
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [body]),
    validate(CS).

import_changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [body, user_id]),
    validate(kura_changeset:validate_required(CS, [user_id])).

validate(CS) ->
    CS1 = kura_changeset:validate_required(CS, [body]),
    CS2 = kura_changeset:validate_length(CS1, body, [{max, 5000}]),
    CS3 = kura_changeset:foreign_key_constraint(CS2, post_id),
    kura_changeset:foreign_key_constraint(CS3, user_id).
```

A reader can only set the `body`; post and author are set by the code. `import_changeset/2` is for the import feature at the end of this chapter, where the author of each comment comes from the imported data.

`foreign_key_constraint/2` turns a foreign key violation into a changeset error. The post changeset gets the same line for `user_id`:

```erlang
changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [title, body, status]),
    CS1 = kura_changeset:validate_required(CS, [title, body]),
    CS2 = kura_changeset:validate_length(CS1, title, [{min, 3}, {max, 200}]),
    CS3 = kura_changeset:foreign_key_constraint(CS2, user_id),
    stamp_published_at(CS3).
```

## The generated migration

`rebar3 compile` generates `m20261103072804_update_schema.erl`:

```erlang
up() ->
    [
        {create_table, ~"comments", [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = body, type = text, nullable = false},
            #kura_column{
                name = post_id,
                type = bigint,
                nullable = false,
                references = {~"posts", id},
                on_delete = cascade
            },
            #kura_column{
                name = user_id,
                type = bigint,
                nullable = false,
                references = {~"users", id},
                on_delete = cascade
            },
            #kura_column{name = inserted_at, type = utc_datetime},
            #kura_column{name = updated_at, type = utc_datetime}
        ]},
        {execute,
            ~"ALTER TABLE \"posts\" ADD CONSTRAINT \"posts_user_id_fkey\" FOREIGN KEY (\"user_id\") REFERENCES \"users\" (\"id\") ON DELETE CASCADE"},
        {create_index, ~"comments", [post_id], #{}},
        {create_index, ~"comments", [user_id], #{}}
    ].
```

The new table gets its foreign keys as column references. `posts` already exists, so its new foreign key is added with an `execute` of a generated `ALTER TABLE`. The constraint names (`posts_user_id_fkey`, `comments_post_id_fkey`) are the ones `foreign_key_constraint/2` expects.

## Foreign keys as errors

Comments are created through the context, in `blog_posts.erl`:

```erlang
add_comment(#{id := PostId}, #{id := AuthorId}, Params) ->
    CS = blog_comment:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, post_id, PostId),
    blog_repo:insert(kura_changeset:put_change(CS1, user_id, AuthorId)).
```

Commenting on a post that does not exist fails in the database, and comes back as a normal changeset error:

```erlang
1> {ok, Al} = blog_repo:get(blog_user, 1).
{ok,#{id => 1,role => author,username => <<"alice">>, ...}}
2> {error, Orphan} = blog_posts:add_comment(#{id => 9999}, Al, #{~"body" => ~"Orphan"}).
...
3> Orphan#kura_changeset.errors.
[{post_id,<<"does not exist">>}]
```

## Preloading

Loading a post does not load its author or comments. You ask for them with a **preload**, and Kura fetches each association with one extra query per association (not one per row).

### In a query

```erlang
4> blog_repo:all(kura_query:preload(kura_query:select(kura_query:where(kura_query:from(blog_post), {id, 2}), [id, title, user_id]), [author])).
{ok,[#{id => 2,title => <<"Changesets">>,
       author =>
           #{id => 1,role => author,username => <<"alice">>,
             email => <<"alice@example.com">>,
             hashed_password =>
                 <<"$pbkdf2-sha256$600000$NyP/2YvxTsToa6elnvDwvg==$1BrPIaLtuZlwYwPpNT71X0XAU5NHXsdDOMcqXAEJNF4=">>,
             ...},
       user_id => 1}]}
```

The association's name becomes a key in the row. Note that the whole user row is loaded, `hashed_password` included. This is one reason the JSON helper lists fields explicitly.

### Standalone and nested

`blog_repo:preload/3` preloads on records you already have. A `{Name, Nested}` tuple preloads inside the association:

```erlang
5> {ok, P2} = blog_repo:get(blog_post, 2).
6> maps:get(comments, blog_repo:preload(blog_post, P2, [{comments, [author]}])).
[#{id => 1,
   author => #{id => 2,role => author,username => <<"bob">>, ...},
   body => <<"Great post!">>,post_id => 2,user_id => 2, ...},
 #{id => 3,
   author => #{id => 1,role => author,username => <<"alice">>, ...},
   body => <<"Thanks Bob!">>,post_id => 2,user_id => 1, ...}]
```

`has_many` works from the user side too:

```erlang
7> [maps:get(title, P) || P <- maps:get(posts, blog_repo:preload(blog_user, Al, [posts]))].
[<<"Changesets">>,<<"Supervision trees">>]
```

The context gets a `get_post/2` that preloads:

```erlang
get_post(Id, Preloads) ->
    case get_post(Id) of
        {ok, Post} -> {ok, blog_repo:preload(blog_post, Post, Preloads)};
        {error, not_found} -> {error, not_found}
    end.
```

## Nested inserts with cast_assoc

Sometimes the parent and its children arrive together, for example when importing a post with its comments from an old blog. `cast_assoc/3` casts a list of child params into child changesets, and the repo inserts parent and children in one transaction:

```erlang
import_post(#{id := AuthorId}, Params) ->
    CS = blog_post:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, user_id, AuthorId),
    CS2 = kura_changeset:cast_assoc(CS1, comments, #{with => fun blog_comment:import_changeset/2}),
    blog_repo:insert(CS2).
```

```erlang
8> {ok, Imported} = blog_posts:import_post(Al, #{~"title" => ~"Imported post", ~"body" => ~"From the old blog.", ~"comments" => [#{~"body" => ~"First!", ~"user_id" => 2}, #{~"body" => ~"Welcome back", ~"user_id" => 1}]}).
...
9> [maps:with([id, post_id, body], C) || C <- maps:get(comments, Imported)].
[#{id => 5,body => <<"First!">>,post_id => 7},
 #{id => 6,body => <<"Welcome back">>,post_id => 7}]
```

Kura filled in `post_id` on every comment. If any child is invalid, nothing is inserted. The parent changeset is then invalid, and the child errors live in `assoc_changes`:

```erlang
10> {error, BadImport} = blog_posts:import_post(Al, #{~"title" => ~"Imported post", ~"body" => ~"x", ~"comments" => [#{~"body" => ~""}]}).
...
11> #kura_changeset{assoc_changes = #{comments := [Failed]}} = BadImport.
12> blog_api_json:errors(Failed).
#{body => [<<"can't be blank">>],
  user_id => [<<"can't be blank">>]}
```

```admonish warning title="Always pass `with` to cast_assoc"
Without `with`, `cast_assoc` builds child changesets that cast every stored field of the child except its primary key and the parent's foreign key. For comments that includes `user_id` and the timestamps. Give it a changeset function with an explicit allow-list, as above.
```

## Cascading deletes

Because the foreign keys say `ON DELETE CASCADE`, deleting a user removes their posts and comments, and deleting a post removes its comments. From `test/blog_data_assoc_SUITE.erl`:

```erlang
deleting_a_user_cascades(Config) ->
    {_, Alice} = post_with_comments(Config),
    {ok, _} = blog_repo:delete(kura_changeset:cast(blog_user, Alice, #{}, [])),
    {ok, []} = blog_repo:all(kura_query:from(blog_post)),
    {ok, []} = blog_repo:all(kura_query:from(blog_comment)).
```

## Comments in the API

The JSON helper learns to render comments and any association that was preloaded:

```erlang
post(Post) ->
    with_loaded(Post, #{
        id => maps:get(id, Post),
        ...
    }).

comment(Comment) ->
    with_loaded(Comment, #{
        id => maps:get(id, Comment),
        body => maps:get(body, Comment),
        user_id => maps:get(user_id, Comment),
        inserted_at => datetime(maps:get(inserted_at, Comment))
    }).

with_loaded(Record, Json) ->
    Loaded = [
        {Key, loaded(Key, maps:get(Key, Record))}
     || Key <- [author, comments], maps:is_key(Key, Record)
    ],
    maps:merge(Json, maps:from_list(Loaded)).

loaded(author, Author) -> user(Author);
loaded(comments, Comments) -> [comment(C) || C <- Comments].

user(User) ->
    #{
        id => maps:get(id, User),
        username => maps:get(username, User),
        role => maps:get(role, User)
    }.
```

Authors are rendered with `user/1`, which only exposes `id`, `username` and `role`.

In the controller, `show/1` preloads the author and the comments with their authors, and a new action creates comments:

```erlang
show(#{bindings := #{~"id" := Id}}) ->
    case blog_posts:get_post(Id, [author, {comments, [author]}]) of
        {ok, Post} -> {json, blog_api_json:post(Post)};
        {error, not_found} -> not_found()
    end.

create_comment(#{bindings := #{~"id" := Id}, json := Params} = Req) ->
    case {blog_posts:get_post(Id), current_author(Req)} of
        {{ok, Post}, {ok, Author}} ->
            case blog_posts:add_comment(Post, Author, Params) of
                {ok, Comment} -> {json, 201, #{}, blog_api_json:comment(Comment)};
                {error, #kura_changeset{} = CS} -> unprocessable(CS)
            end;
        {{error, not_found}, _} ->
            not_found();
        {_, {error, not_found}} ->
            {json, 422, #{}, #{errors => #{user_id => [~"does not exist"]}}}
    end.
```

Add `create_comment/1` to the export list and the route to the `/api` group:

```erlang
{"/posts/:id/comments", fun blog_posts_controller:create_comment/1, #{
    methods => [post]
}},
```

With Alice's published post and a second user, Bob:

```shell
curl -s -X POST localhost:8080/api/posts/1/comments -H 'content-type: application/json' -d '{"user_id": 2, "body": "Great post!"}'
```

```json
{"id":1,"body":"Great post!","user_id":2,"inserted_at":"2026-11-03T07:51:43Z"}
```

```shell
curl -s -i -X POST localhost:8080/api/posts/1/comments -H 'content-type: application/json' -d '{"user_id": 2, "body": ""}'
```

```
HTTP/1.1 422 Unprocessable Entity
...

{"errors":{"body":["can't be blank"]}}
```

```shell
curl -s -i -X POST localhost:8080/api/posts/42/comments -H 'content-type: application/json' -d '{"user_id": 2, "body": "Hello?"}'
```

```
HTTP/1.1 404 Not Found
...

{"error":"post not found"}
```

```shell
curl -s localhost:8080/api/posts/1
```

```json
{"id":1,"status":"published","title":"Changesets","author":{"id":1,"role":"author","username":"alice"},"body":"Cast, validate, persist.","user_id":1,"inserted_at":"2026-11-03T07:51:43Z","published_at":"2026-11-03T07:51:43Z","comments":[{"id":1,"author":{"id":2,"role":"author","username":"bob"},"body":"Great post!","user_id":2,"inserted_at":"2026-11-03T07:51:43Z"}]}
```

---

Next: tags, which need a [many-to-many association, and post metadata stored as an embedded schema](advanced-data.md).

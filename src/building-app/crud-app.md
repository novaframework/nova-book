# CRUD Application

Time to tie everything together. In this chapter we build a complete CRUD (Create, Read, Update, Delete) notes application with both an HTML frontend and a JSON API backend.

## The plan

We will build:
- HTML pages for listing, creating, and editing notes
- JSON API endpoints for the same operations
- Database persistence with PostgreSQL
- Authentication for the HTML pages

## Database setup

Create the notes table:

```sql
CREATE TABLE notes (
    id SERIAL PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    body TEXT,
    author VARCHAR(255),
    inserted_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);
```

## Repository module

Create `src/my_first_nova_note_repo.erl`:

```erlang
-module(my_first_nova_note_repo).
-export([
         all/0,
         get/1,
         create/3,
         update/3,
         delete/1
        ]).

all() ->
    case pgo:query("SELECT id, title, body, author, inserted_at FROM notes ORDER BY inserted_at DESC") of
        #{rows := Rows} ->
            {ok, [row_to_map(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

get(Id) ->
    case pgo:query("SELECT id, title, body, author, inserted_at FROM notes WHERE id = $1", [Id]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

create(Title, Body, Author) ->
    case pgo:query("INSERT INTO notes (title, body, author) VALUES ($1, $2, $3) "
                   "RETURNING id, title, body, author, inserted_at",
                   [Title, Body, Author]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

update(Id, Title, Body) ->
    case pgo:query("UPDATE notes SET title = $1, body = $2, updated_at = NOW() WHERE id = $3 "
                   "RETURNING id, title, body, author, inserted_at",
                   [Title, Body, Id]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

delete(Id) ->
    case pgo:query("DELETE FROM notes WHERE id = $1", [Id]) of
        #{command := delete, num_rows := 1} -> ok;
        #{num_rows := 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

row_to_map({Id, Title, Body, Author, InsertedAt}) ->
    #{id => Id,
      title => Title,
      body => Body,
      author => Author,
      inserted_at => InsertedAt}.
```

## JSON API controller

We can scaffold the API controller and a JSON schema in one step with the code generator:

```shell
rebar3 nova gen_resource --name notes
===> Writing src/controllers/my_first_nova_notes_api_controller.erl
===> Writing priv/schemas/note.json
```

This gives us a controller with stub functions and a JSON schema that the [OpenAPI generator](../developer-tools/openapi.md) can pick up later. Now let's replace the stubs with our actual implementation.

Create (or replace) `src/controllers/my_first_nova_notes_api_controller.erl`:

```erlang
-module(my_first_nova_notes_api_controller).
-export([
         index/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

index(_Req) ->
    {ok, Notes} = my_first_nova_note_repo:all(),
    {json, #{notes => Notes}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_note_repo:get(binary_to_integer(Id)) of
        {ok, Note} ->
            {json, Note};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end.

create(#{params := #{<<"title">> := Title, <<"body">> := Body,
                     <<"author">> := Author}}) ->
    case my_first_nova_note_repo:create(Title, Body, Author) of
        {ok, Note} ->
            {json, 201, #{}, Note};
        {error, Reason} ->
            {status, 422, #{}, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"title, body and author required">>}}.

update(#{bindings := #{<<"id">> := Id},
         params := #{<<"title">> := Title, <<"body">> := Body}}) ->
    case my_first_nova_note_repo:update(binary_to_integer(Id), Title, Body) of
        {ok, Note} ->
            {json, Note};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end;
update(_Req) ->
    {status, 422, #{}, #{error => <<"title and body required">>}}.

delete(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_note_repo:delete(binary_to_integer(Id)) of
        ok ->
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end.
```

## HTML controller

Create `src/controllers/my_first_nova_notes_controller.erl`:

```erlang
-module(my_first_nova_notes_controller).
-export([
         index/1,
         new/1,
         create/1,
         edit/1,
         update/1,
         delete/1
        ]).

index(#{auth_data := #{username := Username}}) ->
    {ok, Notes} = my_first_nova_note_repo:all(),
    {ok, [{notes, Notes}, {username, Username}], #{view => notes_index}};
index(_Req) ->
    {redirect, "/login"}.

new(#{auth_data := #{authed := true}}) ->
    {ok, [], #{view => notes_new}};
new(_Req) ->
    {redirect, "/login"}.

create(#{auth_data := #{username := Username},
         params := #{<<"title">> := Title, <<"body">> := Body}}) ->
    my_first_nova_note_repo:create(Title, Body, Username),
    {redirect, "/notes"};
create(_Req) ->
    {redirect, "/login"}.

edit(#{auth_data := #{authed := true},
       bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_note_repo:get(binary_to_integer(Id)) of
        {ok, Note} ->
            {ok, [{note, Note}], #{view => notes_edit}};
        {error, not_found} ->
            {status, 404}
    end;
edit(_Req) ->
    {redirect, "/login"}.

update(#{auth_data := #{authed := true},
         bindings := #{<<"id">> := Id},
         params := #{<<"title">> := Title, <<"body">> := Body}}) ->
    my_first_nova_note_repo:update(binary_to_integer(Id), Title, Body),
    {redirect, "/notes"};
update(_Req) ->
    {redirect, "/login"}.

delete(#{auth_data := #{authed := true},
         bindings := #{<<"id">> := Id}}) ->
    my_first_nova_note_repo:delete(binary_to_integer(Id)),
    {redirect, "/notes"};
delete(_Req) ->
    {redirect, "/login"}.
```

## Views

Create the templates in `src/views/`.

**`notes_index.dtl`** — List all notes:

```html
<html>
<head><title>Notes</title></head>
<body>
  <h1>Notes</h1>
  <p>Welcome, {{ username }}</p>
  <a href="/notes/new">New Note</a>
  <ul>
  {% for note in notes %}
    <li>
      <strong>{{ note.title }}</strong> by {{ note.author }}
      <br>{{ note.body }}
      <br>
      <a href="/notes/{{ note.id }}/edit">Edit</a>
      <form action="/notes/{{ note.id }}/delete" method="post" style="display:inline">
        <input type="submit" value="Delete">
      </form>
    </li>
  {% empty %}
    <li>No notes yet.</li>
  {% endfor %}
  </ul>
</body>
</html>
```

**`notes_new.dtl`** — Create a new note:

```html
<html>
<head><title>New Note</title></head>
<body>
  <h1>New Note</h1>
  <form action="/notes" method="post">
    <label for="title">Title:</label><br>
    <input type="text" id="title" name="title"><br>
    <label for="body">Body:</label><br>
    <textarea id="body" name="body" rows="10" cols="50"></textarea><br>
    <input type="submit" value="Create">
  </form>
  <a href="/notes">Back</a>
</body>
</html>
```

**`notes_edit.dtl`** — Edit a note:

```html
<html>
<head><title>Edit Note</title></head>
<body>
  <h1>Edit Note</h1>
  <form action="/notes/{{ note.id }}" method="post">
    <label for="title">Title:</label><br>
    <input type="text" id="title" name="title" value="{{ note.title }}"><br>
    <label for="body">Body:</label><br>
    <textarea id="body" name="body" rows="10" cols="50">{{ note.body }}</textarea><br>
    <input type="submit" value="Update">
  </form>
  <a href="/notes">Back</a>
</body>
</html>
```

## Routing

The full router with all route groups:

```erlang
-module(my_first_nova_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
  [
    %% Public routes
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/ws", my_first_nova_ws_handler, #{protocol => ws}}
                ]
    },

    %% Auth endpoint
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [post]}}
                ]
    },

    %% HTML notes (with session auth)
    #{prefix => "/notes",
      security => fun my_first_nova_auth:session_auth/1,
      routes => [
                 {"/", fun my_first_nova_notes_controller:index/1, #{methods => [get]}},
                 {"/new", fun my_first_nova_notes_controller:new/1, #{methods => [get]}},
                 {"/", fun my_first_nova_notes_controller:create/1, #{methods => [post]}},
                 {"/:id/edit", fun my_first_nova_notes_controller:edit/1, #{methods => [get]}},
                 {"/:id", fun my_first_nova_notes_controller:update/1, #{methods => [post]}},
                 {"/:id/delete", fun my_first_nova_notes_controller:delete/1, #{methods => [post]}}
                ]
    },

    %% JSON API (no auth for simplicity)
    #{prefix => "/api",
      security => false,
      routes => [
                 {"/notes", fun my_first_nova_notes_api_controller:index/1, #{methods => [get]}},
                 {"/notes/:id", fun my_first_nova_notes_api_controller:show/1, #{methods => [get]}},
                 {"/notes", fun my_first_nova_notes_api_controller:create/1, #{methods => [post]}},
                 {"/notes/:id", fun my_first_nova_notes_api_controller:update/1, #{methods => [put]}},
                 {"/notes/:id", fun my_first_nova_notes_api_controller:delete/1, #{methods => [delete]}}
                ]
    }
  ].
```

## Testing it

Start the application and test the JSON API:

```shell
# Create a note
curl -s -X POST localhost:8080/api/notes \
  -H "Content-Type: application/json" \
  -d '{"title": "My first note", "body": "Hello from Nova!", "author": "Alice"}'

# List all notes
curl -s localhost:8080/api/notes

# Get a specific note
curl -s localhost:8080/api/notes/1

# Update a note
curl -s -X PUT localhost:8080/api/notes/1 \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated title", "body": "Updated body"}'

# Delete a note
curl -s -X DELETE localhost:8080/api/notes/1
```

For the HTML interface, go to `localhost:8080/login`, log in, then navigate to `localhost:8080/notes`.

## What we built

- **Router** with four route groups: public, auth, HTML notes with security, and JSON API
- **Controllers** for both HTML and JSON responses
- **Views** using ErlyDTL templates with loops and variable interpolation
- **Security** module for authentication
- **Database** persistence with PostgreSQL
- **Repository** module for clean data access

This is the pattern that scales. As your application grows, you add more repos, controllers, views, and route groups.

---

Our application works, but what happens when something goes wrong? Let's add proper [error handling](error-handling.md).

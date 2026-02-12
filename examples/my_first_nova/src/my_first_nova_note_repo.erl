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

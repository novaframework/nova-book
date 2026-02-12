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

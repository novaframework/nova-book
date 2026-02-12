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
            nova_pubsub:broadcast(notes, "note_created", Note),
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
            nova_pubsub:broadcast(notes, "note_updated", Note),
            {json, Note};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end;
update(_Req) ->
    {status, 422, #{}, #{error => <<"title and body required">>}}.

delete(#{bindings := #{<<"id">> := Id}}) ->
    IntId = binary_to_integer(Id),
    case my_first_nova_note_repo:delete(IntId) of
        ok ->
            nova_pubsub:broadcast(notes, "note_deleted", #{id => IntId}),
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end.

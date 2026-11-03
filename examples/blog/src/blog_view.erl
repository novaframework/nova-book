-module(blog_view).
-include_lib("kura/include/kura.hrl").

-export([render/3, render/4, post/1, form/2, errors/1, forbidden/0]).

render(Req, View, Vars) ->
    render(Req, View, Vars, 200).

render(Req, View, Vars, Status) ->
    Page = Vars#{flash => blog_flash:take(Req), current_user => current_user(Req)},
    {ok, Page, #{view => View, status_code => Status}}.

current_user(#{auth_data := #{username := Username, role := Role}}) ->
    #{username => Username, admin => Role =:= admin};
current_user(_Req) ->
    false.

post(Post) ->
    Base = #{
        id => maps:get(id, Post),
        title => maps:get(title, Post),
        body => maps:get(body, Post),
        paragraphs => paragraphs(maps:get(body, Post)),
        status => atom_to_binary(maps:get(status, Post)),
        lock_version => maps:get(lock_version, Post),
        inserted_at => maps:get(inserted_at, Post)
    },
    case Post of
        #{author := #{username := Username}} -> Base#{author => Username};
        _ -> Base
    end.

paragraphs(Text) ->
    Lines = binary:replace(Text, ~"\r\n", ~"\n", [global]),
    [string:trim(P) || P <- binary:split(Lines, ~"\n\n", [global, trim_all])].

form(Fields, Source) ->
    #{Field => text(value(Field, Source)) || Field <- Fields}.

value(Field, Source) ->
    case Source of
        #{Field := Value} -> Value;
        #{} -> maps:get(atom_to_binary(Field), Source, ~"")
    end.

text(Value) when is_binary(Value) -> Value;
text(Value) when is_atom(Value), Value =/= undefined -> atom_to_binary(Value);
text(_Value) -> ~"".

errors(#kura_changeset{} = CS) ->
    blog_api_json:errors(CS).

forbidden() ->
    {ok, Html} = blog_forbidden_dtl:render(#{}),
    iolist_to_binary(Html).

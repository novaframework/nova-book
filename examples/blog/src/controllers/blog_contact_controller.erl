-module(blog_contact_controller).

-export([
    show/1,
    submit/1
]).

show(_Req) ->
    {ok, [], #{view => blog_contact}}.

submit(#{params := Params, files := Files}) ->
    Name = string:trim(maps:get(~"name", Params, ~"")),
    Message = string:trim(maps:get(~"message", Params, ~"")),
    case Name =:= ~"" orelse Message =:= ~"" of
        true ->
            Vars = [
                {error, ~"Please fill in your name and a message."},
                {name, Name},
                {message, Message}
            ],
            {ok, Vars, #{view => blog_contact, status_code => 422}};
        false ->
            {ok, [{name, Name}, {attachments, attachments(Files)}], #{view => blog_contact_thanks}}
    end.

attachments(Files) ->
    [
        #{filename => Filename, size => byte_size(Body)}
     || #{filename := Filename, result := #{body := Body}} <- Files
    ].

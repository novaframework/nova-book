-module(blog_ws_client).

-export([connect/1, connect/2, send_text/2, send_json/2, recv/1, recv_json/1, close/1]).

-define(TIMEOUT, 5000).

connect(Path) ->
    connect(Path, []).

connect(Path, Headers) ->
    {ok, Conn} = gun:open("localhost", port(), #{protocols => [http], retry => 0}),
    {ok, http} = gun:await_up(Conn, ?TIMEOUT),
    Stream = gun:ws_upgrade(Conn, Path, Headers),
    receive
        {gun_upgrade, Conn, Stream, [~"websocket"], _RespHeaders} ->
            {ok, {Conn, Stream}};
        {gun_response, Conn, Stream, _Fin, Status, _RespHeaders} ->
            gun:close(Conn),
            {error, Status}
    after ?TIMEOUT ->
        gun:close(Conn),
        {error, timeout}
    end.

send_text({Conn, Stream}, Text) ->
    gun:ws_send(Conn, Stream, {text, Text}).

send_json(Client, Term) ->
    send_text(Client, iolist_to_binary(json:encode(Term))).

recv({Conn, Stream}) ->
    receive
        {gun_ws, Conn, Stream, Frame} -> Frame
    after ?TIMEOUT ->
        timeout
    end.

recv_json(Client) ->
    case recv(Client) of
        {text, Text} -> json:decode(Text);
        Other -> Other
    end.

close({Conn, _Stream}) ->
    gun:close(Conn).

port() ->
    {ok, #{port := Port}} = application:get_env(nova, cowboy_configuration),
    Port.

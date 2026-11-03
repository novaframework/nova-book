-module(blog_otel_h).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-record(state, {otel, next}).

init(StreamID, Req, Opts) ->
    {[], Otel} = otel_nova_stream_h:init(StreamID, Req, Opts#{stream_handlers => []}),
    Req1 = Req#{otel_ctx => otel_ctx:get_current()},
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{otel = Otel, next = Next}}.

data(StreamID, IsFin, Data, #state{otel = Otel, next = Next} = State) ->
    {[], Otel1} = otel_nova_stream_h:data(StreamID, IsFin, Data, Otel),
    {Commands, Next1} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{otel = Otel1, next = Next1}}.

info(StreamID, Info, #state{otel = Otel, next = Next} = State) ->
    {[], Otel1} = otel_nova_stream_h:info(StreamID, without_sendfile(Info), Otel),
    {Commands, Next1} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{otel = Otel1, next = Next1}}.

terminate(StreamID, Reason, #state{otel = Otel, next = Next}) ->
    Result = cowboy_stream:terminate(StreamID, Reason, Next),
    otel_nova_stream_h:terminate(StreamID, Reason, Otel),
    Result.

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

without_sendfile({response, Status, Headers, {sendfile, _Offset, _Length, _Path}}) ->
    {headers, Status, Headers};
without_sendfile(Info) ->
    Info.

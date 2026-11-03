-module(blog_session_h).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

init(StreamID, #{nova_session_id := _} = Req, Opts) ->
    SessionId = blog_session:new_id(),
    Req1 = blog_session:put_cookie(SessionId, Req#{nova_session_id => SessionId}),
    cowboy_stream:init(StreamID, Req1, Opts);
init(StreamID, Req, Opts) ->
    cowboy_stream:init(StreamID, Req, Opts).

data(StreamID, IsFin, Data, Next) ->
    cowboy_stream:data(StreamID, IsFin, Data, Next).

info(StreamID, Info, Next) ->
    cowboy_stream:info(StreamID, Info, Next).

terminate(StreamID, Reason, Next) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

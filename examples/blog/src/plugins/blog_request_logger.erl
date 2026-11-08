-module(blog_request_logger).
-behaviour(nova_plugin).

-include_lib("kernel/include/logger.hrl").

-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    {ok, Req#{request_started_at => erlang:monotonic_time()}, State}.

post_request(#{request_started_at := StartedAt} = Req, _Env, _Options, State) ->
    Elapsed = erlang:monotonic_time() - StartedAt,
    Report = #{
        msg => ~"request",
        method => cowboy_req:method(Req),
        path => cowboy_req:path(Req),
        status => maps:get(resp_status_code, Req, 200),
        duration_us => erlang:convert_time_unit(Elapsed, native, microsecond)
    },
    ?LOG_INFO(with_user(Report, Req)),
    {ok, Req, State};
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
        title => ~"blog_request_logger",
        version => ~"1.0.0",
        url => ~"https://github.com/novaframework/nova-book",
        authors => [~"Erlang Web Development with Nova"],
        description => ~"Logs method, path, status, duration and user id of each request",
        options => []
    }.

with_user(Report, #{auth_data := #{id := UserId}}) ->
    Report#{user_id => UserId};
with_user(Report, _Req) ->
    Report.

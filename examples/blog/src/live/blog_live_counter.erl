-module(blog_live_counter).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(_Bindings) ->
    {#{id => ~"counter", title => ~"Counter", count => 0}, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Counter"]},
            {button, [{id, ~"dec"}, {az_click, arizona_js:push_event(~"dec")}], [~"-"]},
            {span, [{id, ~"count"}], [?get(count)]},
            {button, [{id, ~"inc"}, {az_click, arizona_js:push_event(~"inc")}], [~"+"]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []}.

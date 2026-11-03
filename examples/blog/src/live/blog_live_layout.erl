-module(blog_live_layout).
-include_lib("arizona/include/arizona_stateless.hrl").
-export([render/1]).

render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [{lang, ~"en"}], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}], []},
                {link, [{rel, ~"icon"}, {href, ~"/assets/favicon.ico"}], []},
                {meta, [{name, ~"viewport"}, {content, ~"width=device-width, initial-scale=1"}],
                    []},
                {title, [], [?get(title, ~"Blog"), ~" - Nova Blog"]}
            ]},
            {body, [], [
                {header, [], [
                    {a, [{href, ~"/"}], [~"Nova Blog"]}
                ]},
                ?inner_content,
                {script, [{type, ~"module"}], [
                    ~"""
                    import { connect } from '/assets/arizona/arizona.min.js';
                    connect('/live/ws');
                    """
                ]}
            ]}
        ]}
    ]).

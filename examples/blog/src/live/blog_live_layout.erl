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
                    {a, [{href, ~"/"}], [~"Nova Blog"]},
                    {nav, [], [
                        {a, [{href, ~"/live/counter"}, az_navigate], [~"Counter"]},
                        {a, [{href, ~"/live/editor"}, az_navigate], [~"Write"]},
                        {a, [{href, ~"/live/dashboard/drafts"}, az_navigate], [~"Your posts"]}
                    ]}
                ]},
                ?inner_content,
                {aside, [{'aria-label', ~"Latest activity"}], [
                    {h2, [], [~"Latest"]},
                    {ul, [{id, ~"notifications"}], []}
                ]},
                {script, [{type, ~"module"}, {src, ~"/assets/js/notifications.js"}], []},
                {script, [{type, ~"module"}], [
                    ~"""
                    import { connect } from '/assets/arizona/arizona.min.js';
                    connect('/live/ws');
                    """
                ]}
            ]}
        ]}
    ]).

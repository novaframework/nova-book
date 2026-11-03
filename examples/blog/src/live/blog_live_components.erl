-module(blog_live_components).
-include_lib("arizona/include/arizona_stateless.hrl").
-export([tag_badge/1, field_error/1, word_stats/1]).

tag_badge(Bindings) ->
    ?html({span, [{class, ~"tag"}], [~"#", ?get(tag)]}).

field_error(Bindings) ->
    case ?get(error) of
        undefined -> ?html({span, [{class, ~"field-error"}, hidden], []});
        Message -> ?html({span, [{class, ~"field-error"}, {role, ~"alert"}], [Message]})
    end.

word_stats(Bindings) ->
    ?html(
        {p, [{class, ~"word-stats"}], [
            ?get(words), ~" words, about ", ?get(minutes), ~" min read"
        ]}
    ).

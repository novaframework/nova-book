-module(blog_live_tag_input).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([normalize/1]).

-define(MAX_TAGS, 5).

mount(Props) ->
    {
        #{
            id => maps:get(id, Props),
            notify => maps:get(notify, Props),
            tags => maps:get(tags, Props, []),
            error => undefined
        },
        #{}
    }.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {class, ~"tag-input"}], [
            {ul, [{class, ~"tags"}], [
                ?each(
                    fun(Tag) ->
                        {li, [], [
                            ?stateless(blog_live_components, tag_badge, #{tag => Tag}),
                            {button,
                                [
                                    {type, ~"button"},
                                    {'aria-label', [~"Remove ", Tag]},
                                    {az_click,
                                        arizona_js:push_event(~"remove_tag", #{~"tag" => Tag})}
                                ],
                                [~"x"]}
                        ]}
                    end,
                    ?get(tags)
                )
            ]},
            {form, [{id, ~"tag-form"}, {az_submit, arizona_js:push_event(~"add_tag")}], [
                {input,
                    [
                        {id, ~"tag-name"},
                        {name, ~"tag"},
                        {placeholder, ~"Add a tag and press Enter"},
                        {autocomplete, ~"off"},
                        {az_keydown, arizona_js:on_key(escape, arizona_js:reset_form(~"#tag-form"))}
                    ],
                    []}
            ]},
            ?stateless(blog_live_components, field_error, #{error => ?get(error)})
        ]}
    ).

handle_event(~"add_tag", #{~"tag" := Input}, Bindings) ->
    #{tags := Tags} = Bindings,
    case normalize(Input) of
        {ok, _Tag} when length(Tags) >= ?MAX_TAGS ->
            {Bindings#{error => ~"A post can have at most 5 tags"}, #{}, []};
        {ok, Tag} ->
            NewTags = lists:uniq(Tags ++ [Tag]),
            notify(Bindings, NewTags),
            {Bindings#{tags => NewTags, error => undefined}, #{}, [
                arizona_js:reset_form(~"#tag-form")
            ]};
        {error, Message} ->
            {Bindings#{error => Message}, #{}, []}
    end;
handle_event(~"remove_tag", #{~"tag" := Tag}, Bindings) ->
    NewTags = lists:delete(Tag, maps:get(tags, Bindings)),
    notify(Bindings, NewTags),
    {Bindings#{tags => NewTags, error => undefined}, #{}, []}.

normalize(Input) ->
    Tag = string:lowercase(string:trim(Input)),
    case re:run(Tag, ~"^[a-z0-9-]{1,24}\\z") of
        {match, _} -> {ok, Tag};
        nomatch -> {error, ~"Tags are 1-24 characters: a-z, 0-9 and -"}
    end.

notify(#{notify := ParentId}, Tags) ->
    arizona_live:send(ParentId, {tags_changed, Tags}).

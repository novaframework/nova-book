-module(blog_live_editor).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).
-export([validate/2, paragraphs/1, word_count/1]).

-define(MAX_TITLE, 120).
-define(STATUS_TIMEOUT, 3000).

mount(Bindings) ->
    Post = maps:get(post, Bindings, undefined),
    Title = field(title, Post),
    Body = field(body, Post),
    {
        #{
            id => ~"editor",
            title => ~"Write a post",
            current_user => maps:get(current_user, Bindings),
            post => Post,
            initial_title => Title,
            initial_body => Body,
            post_title => Title,
            body => Body,
            tags => tag_names(Post),
            errors => #{},
            status => ~""
        },
        #{}
    }.

field(_Field, undefined) -> ~"";
field(Field, Post) -> maps:get(Field, Post).

tag_names(#{tags := Tags}) -> [Name || #{name := Name} <- Tags];
tag_names(_Post) -> [].

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}, {class, ~"editor"}], [
            {h1, [], [~"Write a post"]},
            {form,
                [
                    {id, ~"post-form"},
                    {az_input, arizona_js:push_event(~"preview")},
                    {az_debounce, ~"150"},
                    {az_submit, arizona_js:push_event(~"save")}
                ],
                [
                    {label, [{for, ~"post-title"}], [~"Title"]},
                    {input,
                        [
                            {id, ~"post-title"},
                            {name, ~"title"},
                            {value, ?get(initial_title)},
                            {autocomplete, ~"off"}
                        ],
                        []},
                    ?stateless(blog_live_components, field_error, #{
                        error => maps:get(title, ?get(errors), undefined)
                    }),
                    {label, [{for, ~"post-body"}], [~"Body"]},
                    {textarea, [{id, ~"post-body"}, {name, ~"body"}, {rows, ~"10"}], [
                        ?get(initial_body)
                    ]},
                    ?stateless(blog_live_components, field_error, #{
                        error => maps:get(body, ?get(errors), undefined)
                    }),
                    {button, [{type, ~"submit"}, {name, ~"intent"}, {value, ~"save"}], [
                        ~"Save draft"
                    ]},
                    {button, [{type, ~"submit"}, {name, ~"intent"}, {value, ~"publish"}], [
                        ~"Publish"
                    ]}
                ]},
            ?stateful(blog_live_tag_input, #{id => ~"tags", notify => ?get(id), tags => ?get(tags)}),
            {p, [{id, ~"save-status"}, {role, ~"status"}], [?get(status)]},
            {button, [{id, ~"toggle-preview"}, {az_click, arizona_js:toggle(~"#preview")}], [
                ~"Show or hide preview"
            ]},
            {article, [{id, ~"preview"}], [
                {h2, [], [?get(post_title)]},
                ?each(fun(Paragraph) -> {p, [], [Paragraph]} end, paragraphs(?get(body))),
                ?stateless(blog_live_components, word_stats, #{
                    words => word_count(?get(body)),
                    minutes => reading_minutes(word_count(?get(body)))
                })
            ]}
        ]}
    ).

handle_event(~"preview", #{~"title" := Title, ~"body" := Body}, Bindings) ->
    Errors = preview_errors(Title, Body),
    {Bindings#{post_title => Title, body => Body, errors => Errors}, #{}, []};
handle_event(~"save", #{~"title" := Title, ~"body" := Body} = Params, Bindings) ->
    case validate(Title, Body) of
        Errors when map_size(Errors) =:= 0 ->
            Intent = maps:get(~"intent", Params, ~"save"),
            save(Intent, Bindings#{post_title => Title, body => Body});
        Errors ->
            {Bindings#{errors => Errors}, #{}, [arizona_js:focus(first_invalid(Errors))]}
    end.

save(Intent, #{post := Post, current_user := User, post_title := Title, body := Body} = Bindings) ->
    case store(Post, User, #{~"title" => Title, ~"body" => Body}) of
        {ok, Saved} when Intent =:= ~"publish" ->
            publish(Saved, Bindings);
        {ok, Saved} ->
            ?send_after(?STATUS_TIMEOUT, clear_status),
            {Bindings#{post => Saved, errors => #{}, status => ~"Draft saved"}, #{}, [
                arizona_js:set_title(Title)
            ]};
        {error, stale} ->
            {Bindings#{status => ~"Someone else changed this post. Reload to see it."}, #{}, []};
        {error, Changeset} ->
            Errors = changeset_errors(Changeset),
            {Bindings#{errors => Errors}, #{}, [arizona_js:focus(first_invalid(Errors))]}
    end.

store(undefined, User, Params) -> blog_posts:create_post(User, Params);
store(Post, _User, Params) -> blog_posts:update_post(Post, Params).

publish(Post, #{tags := Tags} = Bindings) ->
    {ok, Published} = blog_posts:publish(Post, Tags),
    ok = blog_notify:post_published(Published),
    Flash = #{~"info" => ~"Your post is live"},
    {Bindings#{post => Published}, #{}, [
        arizona_js:navigate(blog_live_post:path(Published), #{flash => Flash})
    ]}.

changeset_errors(Changeset) ->
    Errors = blog_api_json:errors(Changeset),
    #{Field => Message || Field := [Message | _] <- maps:with([title, body], Errors)}.

handle_info({tags_changed, Tags}, Bindings) ->
    {Bindings#{tags => Tags}, #{}, []};
handle_info(clear_status, Bindings) ->
    {Bindings#{status => ~""}, #{}, []}.

validate(Title, Body) ->
    TitleErrors =
        case string:trim(Title) of
            ~"" ->
                #{title => ~"A title is required"};
            Trimmed when byte_size(Trimmed) > ?MAX_TITLE ->
                #{title => ~"Keep the title under 120 characters"};
            _ ->
                #{}
        end,
    case string:trim(Body) of
        ~"" -> TitleErrors#{body => ~"Write something first"};
        _ -> TitleErrors
    end.

preview_errors(~"", _Body) -> #{};
preview_errors(Title, Body) -> maps:with([title], validate(Title, Body)).

paragraphs(Body) ->
    [string:trim(P) || P <- binary:split(Body, [~"\r\n\r\n", ~"\n\n"], [global, trim_all])].

word_count(Body) ->
    length(string:lexemes(Body, [$\s, $\n, $\t, [$\r, $\n]])).

reading_minutes(Words) ->
    max(1, (Words + 199) div 200).

first_invalid(#{title := _}) -> ~"#post-title";
first_invalid(#{body := _}) -> ~"#post-body";
first_invalid(#{}) -> ~"#post-title".

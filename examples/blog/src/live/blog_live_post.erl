-module(blog_live_post).
-include_lib("arizona/include/arizona_stateful.hrl").
-include_lib("nova/include/nova_pubsub.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).
-export([path/1, comment_key/1]).

mount(#{post := Post} = Bindings) ->
    #{id := PostId, title := Title, body := Body, author := #{username := Author}} = Post,
    ?connected andalso nova_pubsub:join(blog_notify:post_channel(PostId)),
    {
        #{
            id => ~"post",
            title => Title,
            post => #{id => PostId, title => Title},
            author => Author,
            paragraphs => blog_live_editor:paragraphs(Body),
            current_user => maps:get(current_user, Bindings),
            comments => arizona_stream:new(fun ?MODULE:comment_key/1, load_comments(PostId)),
            comment_error => undefined,
            flash => maps:get(flash, Bindings, #{})
        },
        #{}
    }.

render(Bindings) ->
    ?html(
        {article, [{id, ?get(id)}, {class, ~"post"}], [
            ?stateless(blog_live_components, flash, #{flash => ?get(flash)}),
            {h1, [], [maps:get(title, ?get(post))]},
            {p, [{class, ~"byline"}], [~"by ", ?get(author)]},
            ?each(fun(Paragraph) -> {p, [], [Paragraph]} end, ?get(paragraphs)),
            {section, [{id, ~"comments"}], [
                {h2, [], [~"Comments"]},
                {ol, [{id, ~"comment-list"}], [
                    ?each(
                        fun(#{author := Author, body := Body, inserted_at := At}, Key) ->
                            {li, [{az_key, Key}, {class, ~"comment"}], [
                                {p, [{class, ~"comment-meta"}], [
                                    {strong, [], [Author]},
                                    ~" ",
                                    {time, [{datetime, At}], [At]}
                                ]},
                                {p, [{class, ~"comment-body"}], [Body]}
                            ]}
                        end,
                        ?get(comments)
                    )
                ]},
                ?stateless(fun comment_form/1, #{
                    signed_in => ?get(current_user) =/= undefined,
                    error => ?get(comment_error)
                })
            ]}
        ]}
    ).

comment_form(#{signed_in := false}) ->
    ?html({p, [{id, ~"comment-login"}], [{a, [{href, ~"/login"}], [~"Log in"]}, ~" to comment."]});
comment_form(Bindings) ->
    ?html(
        {form, [{id, ~"comment-form"}, {az_submit, arizona_js:push_event(~"add_comment")}], [
            {label, [{for, ~"comment-body"}], [~"Add a comment"]},
            {textarea, [{id, ~"comment-body"}, {name, ~"body"}, {rows, ~"3"}], []},
            ?stateless(blog_live_components, field_error, #{error => ?get(error)}),
            {button, [{type, ~"submit"}], [~"Post comment"]}
        ]}
    ).

handle_event(~"add_comment", #{~"body" := Body}, #{current_user := #{id := UserId}} = Bindings) ->
    #{post := Post, comments := Comments} = Bindings,
    {ok, Author} = blog_repo:get(blog_user, UserId),
    case blog_posts:add_comment(Post, Author, #{~"body" => Body}) of
        {ok, Comment} ->
            ok = blog_notify:comment_created(Post, Author, Comment),
            Item = blog_notify:comment(Comment, Author),
            {
                Bindings#{
                    comments => arizona_stream:insert(Comments, Item), comment_error => undefined
                },
                #{},
                [arizona_js:reset_form(~"#comment-form")]
            };
        {error, Changeset} ->
            {Bindings#{comment_error => first_error(Changeset)}, #{}, [
                arizona_js:focus(~"#comment-body")
            ]}
    end;
handle_event(~"add_comment", _Payload, Bindings) ->
    {Bindings, #{}, []}.

handle_info(#nova_pubsub{sender = Sender}, Bindings) when Sender =:= self() ->
    {Bindings, #{}, []};
handle_info(#nova_pubsub{topic = ~"comment_created", payload = #{id := Id} = Item}, Bindings) ->
    #{comments := Comments} = Bindings,
    {Bindings#{comments => arizona_stream:update(Comments, Id, Item)}, #{}, []};
handle_info(_Message, Bindings) ->
    {Bindings, #{}, []}.

path(#{id := Id}) ->
    <<"/live/posts/", (integer_to_binary(Id))/binary>>.

comment_key(#{id := Id}) ->
    Id.

load_comments(PostId) ->
    {ok, #{comments := Comments}} = blog_posts:get_post(PostId, [{comments, [author]}]),
    Sorted = lists:sort([{Id, Comment} || #{id := Id} = Comment <- Comments]),
    [blog_notify:comment(Comment, Author) || {_, #{author := Author} = Comment} <- Sorted].

first_error(Changeset) ->
    case maps:values(blog_api_json:errors(Changeset)) of
        [[Message | _] | _] -> Message;
        [] -> ~"Could not save the comment"
    end.

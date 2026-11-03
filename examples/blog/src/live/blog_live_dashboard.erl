-module(blog_live_dashboard).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_update/3, handle_info/2]).
-export([section/1]).

mount(#{current_user := #{id := UserId} = User} = Params) ->
    Section = section(maps:get(~"section", Params, ~"drafts")),
    Bindings = #{
        id => ~"dashboard",
        title => heading(Section),
        current_user => User,
        section => Section,
        posts => posts(UserId, Section)
    },
    ?connected andalso ?send(update_title),
    {Bindings, #{}}.

handle_update(Params, #{current_user := #{id := UserId}} = Bindings, Effects) ->
    Section = section(maps:get(~"section", Params, ~"drafts")),
    Updated = Bindings#{section => Section, posts => posts(UserId, Section)},
    {Updated, #{}, [document_title(Section) | Effects]}.

handle_info(update_title, Bindings) ->
    {Bindings, #{}, [document_title(maps:get(section, Bindings))]}.

document_title(Section) ->
    arizona_js:set_title(<<(heading(Section))/binary, " - Nova Blog">>).

render(Bindings) ->
    Section = ?get(section),
    ?html(
        {main, [{id, ?get(id)}, {class, ~"dashboard"}], [
            {h1, [], [heading(Section)]},
            {nav, [{'aria-label', ~"Post status"}], [
                {a,
                    [
                        {href, ~"/live/dashboard/drafts"},
                        az_patch,
                        {'aria-current', current(Section, draft)}
                    ],
                    [~"Drafts"]},
                {a,
                    [
                        {href, ~"/live/dashboard/published"},
                        az_patch,
                        {'aria-current', current(Section, published)}
                    ],
                    [~"Published"]}
            ]},
            {ul, [{id, ~"post-list"}], [
                ?each(
                    fun(#{id := Id, title := Title}) ->
                        {li, [], [{a, [{href, post_href(Section, Id)}, az_navigate], [Title]}]}
                    end,
                    ?get(posts)
                )
            ]}
        ]}
    ).

current(Section, Section) -> ~"page";
current(_Section, _Link) -> ~"false".

section(~"published") -> published;
section(_Other) -> draft.

heading(draft) -> ~"Your drafts";
heading(published) -> ~"Your published posts".

post_href(draft, Id) -> <<"/live/editor/", (integer_to_binary(Id))/binary>>;
post_href(published, Id) -> blog_live_post:path(#{id => Id}).

posts(UserId, Status) ->
    Mine = kura_query:where(kura_query:from(blog_post), {user_id, UserId}),
    Query = kura_query:where(Mine, {status, atom_to_binary(Status)}),
    {ok, Posts} = blog_repo:all(kura_query:order_by(Query, [{updated_at, desc}, {id, desc}])),
    Posts.

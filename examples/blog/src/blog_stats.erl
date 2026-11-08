-module(blog_stats).

-export([
    published_count/0,
    latest_publication/0,
    posts_per_author/1,
    post_titles_with_authors/0,
    posts_by_admins/0,
    posts_without_comments/0,
    latest_post_per_author/0,
    activity_feed/1,
    commenters/1,
    published_since/1,
    search/1,
    comments_page/2,
    export_titles/1,
    top_commented/1
]).

published_count() ->
    blog_repo:count(published()).

latest_publication() ->
    blog_repo:aggregate(published(), {max, published_at}).

posts_per_author(MinPosts) ->
    Q = kura_query:group_by(kura_query:from(blog_post), [user_id]),
    Q1 = kura_query:select(Q, [user_id, {count, '*'}]),
    Q2 = kura_query:having(Q1, {fragment, ~"count(*) >= ?", [MinPosts]}),
    blog_repo:all(kura_query:order_by(Q2, [{user_id, asc}])).

post_titles_with_authors() ->
    Q = kura_query:join(kura_query:from(blog_post), inner, blog_user, {user_id, id}),
    Q1 = kura_query:select(Q, [title, username]),
    blog_repo:all(kura_query:order_by(Q1, [{title, asc}])).

posts_by_admins() ->
    Admins = kura_query:select(kura_query:where(kura_query:from(blog_user), {role, ~"admin"}), [id]),
    blog_repo:all(kura_query:where(kura_query:from(blog_post), {user_id, in, {subquery, Admins}})).

posts_without_comments() ->
    Comments = kura_query:where(
        kura_query:from(blog_comment),
        {fragment, ~"\"comments\".\"post_id\" = \"posts\".\"id\"", []}
    ),
    blog_repo:all(kura_query:where(kura_query:from(blog_post), {not_exists, {subquery, Comments}})).

latest_post_per_author() ->
    Ranked = kura_query:select_expr(live_posts(), [
        {id, id},
        {user_id, user_id},
        {title, title},
        {position,
            kura_query:over(row_number, #{
                partition_by => [user_id], order_by => [{inserted_at, desc}]
            })}
    ]),
    Q = kura_query:with_cte(kura_query:from(~"ranked_posts"), ~"ranked_posts", Ranked),
    Q1 = kura_query:where(Q, {position, 1}),
    blog_repo:all(kura_query:order_by(Q1, [{user_id, asc}])).

activity_feed(Limit) ->
    Posts = kura_query:select_expr(live_posts(), [
        {kind, {fragment, ~"'post'", []}}, {text, title}, {at, inserted_at}
    ]),
    Comments = kura_query:select_expr(kura_query:from(blog_comment), [
        {kind, {fragment, ~"'comment'", []}}, {text, body}, {at, inserted_at}
    ]),
    Feed = kura_query:with_cte(
        kura_query:from(~"feed"), ~"feed", kura_query:union_all(Posts, Comments)
    ),
    blog_repo:all(kura_query:limit(kura_query:order_by(Feed, [{at, desc}]), Limit)).

commenters(#{id := PostId}) ->
    Q = kura_query:where(kura_query:from(blog_comment), {post_id, PostId}),
    blog_repo:all(kura_query:distinct(kura_query:select(Q, [user_id]))).

published_since(Days) ->
    blog_repo:all(
        kura_query:where(
            published(), {fragment, ~"published_at > now() - make_interval(days => ?)", [Days]}
        )
    ).

search(Terms) ->
    Q = kura_query:where(published(), {'or', [{title, matches, Terms}, {body, matches, Terms}]}),
    blog_repo:all(kura_query:order_by(Q, [{published_at, desc}])).

comments_page(#{id := PostId}, Opts) ->
    Q = kura_query:where(kura_query:from(blog_comment), {post_id, PostId}),
    kura_paginator:cursor_paginate(blog_repo, Q, Opts).

export_titles(IoDevice) ->
    kura_stream:stream(blog_repo, published(), fun(Batch) -> write_titles(IoDevice, Batch) end, #{
        batch_size => 100
    }).

write_titles(IoDevice, Posts) ->
    ok = file:write(IoDevice, [[maps:get(title, P), $\n] || P <- Posts]).

top_commented(Limit) ->
    blog_repo:query(
        ~"""
        SELECT p.id, p.title, count(c.id) AS comments
        FROM posts p LEFT JOIN comments c ON c.post_id = p.id
        WHERE p.deleted_at IS NULL
        GROUP BY p.id
        ORDER BY comments DESC, p.id
        LIMIT $1
        """,
        [Limit]
    ).

published() ->
    kura_query:where(kura_query:from(blog_post), {status, ~"published"}).

live_posts() ->
    kura_query:where(kura_query:from(blog_post), {deleted_at, is_nil}).

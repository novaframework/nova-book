# Advanced Queries

The basic query builder covers most needs — `where`, `order_by`, `limit`, `offset`. But sometimes you need aggregations, subqueries, common table expressions (CTEs), or window functions. Kura supports all of these.

## Aggregates

```erlang
%% Count all published posts
Q = kura_query:from(post),
Q1 = kura_query:where(Q, {status, published}),
Q2 = kura_query:select(Q1, [{count, id}]),
{ok, [#{count => Count}]} = blog_repo:all(Q2).

%% Multiple aggregates
Q = kura_query:from(post),
Q1 = kura_query:group_by(Q, [user_id]),
Q2 = kura_query:select(Q1, [user_id, {count, id}, {max, inserted_at}]),
{ok, Stats} = blog_repo:all(Q2).
%% [#{user_id => 1, count => 5, max => {{2026,2,23},{12,0,0}}}, ...]
```

Supported aggregate functions: `count`, `sum`, `avg`, `min`, `max`.

## Having clauses

Filter grouped results:

```erlang
%% Users with more than 10 posts
Q = kura_query:from(post),
Q1 = kura_query:group_by(Q, [user_id]),
Q2 = kura_query:select(Q1, [user_id, {count, id}]),
Q3 = kura_query:having(Q2, {count, id, '>', 10}),
{ok, ActiveAuthors} = blog_repo:all(Q3).
```

## Joins

```erlang
%% Join posts with users
Q = kura_query:from(post),
Q1 = kura_query:join(Q, user, {post, user_id, user, id}),
Q2 = kura_query:select(Q1, [{post, [id, title]}, {user, [username]}]),
{ok, Results} = blog_repo:all(Q2).

%% Left join (include posts without comments)
Q = kura_query:from(post),
Q1 = kura_query:left_join(Q, comment, {post, id, comment, post_id}),
Q2 = kura_query:group_by(Q1, [{post, id}]),
Q3 = kura_query:select(Q2, [{post, [id, title]}, {count, {comment, id}}]),
{ok, PostsWithCounts} = blog_repo:all(Q3).
```

## Subqueries

Use a query as a condition in another query:

```erlang
%% Posts by users who joined in the last 30 days
RecentUsers = kura_query:from(user),
RecentUsers1 = kura_query:where(RecentUsers, {inserted_at, '>=', ThirtyDaysAgo}),
RecentUsers2 = kura_query:select(RecentUsers1, [id]),

Q = kura_query:from(post),
Q1 = kura_query:where(Q, {user_id, in, {subquery, RecentUsers2}}),
{ok, Posts} = blog_repo:all(Q1).
```

## Common Table Expressions (CTEs)

CTEs make complex queries readable by breaking them into named steps:

```erlang
%% Find the top 5 authors and their latest post
TopAuthors = kura_query:from(post),
TopAuthors1 = kura_query:group_by(TopAuthors, [user_id]),
TopAuthors2 = kura_query:select(TopAuthors1, [user_id, {count, id}]),
TopAuthors3 = kura_query:order_by(TopAuthors2, [{count, desc}]),
TopAuthors4 = kura_query:limit(TopAuthors3, 5),

Q = kura_query:with(<<"top_authors">>, TopAuthors4),
Q1 = kura_query:from_cte(Q, <<"top_authors">>),
Q2 = kura_query:join(Q1, user, {<<"top_authors">>, user_id, user, id}),
{ok, Results} = blog_repo:all(Q2).
```

## Window functions

Compute values across a set of rows without collapsing them:

```erlang
%% Rank posts by comment count within each user
Q = kura_query:from(post),
Q1 = kura_query:left_join(Q, comment, {post, id, comment, post_id}),
Q2 = kura_query:select(Q1, [
    {post, [id, title, user_id]},
    {count, {comment, id}},
    {window, row_number, [], [{partition_by, {post, user_id}},
                               {order_by, [{count, desc}]}]}
]),
{ok, RankedPosts} = blog_repo:all(Q2).
```

## Union queries

Combine results from multiple queries:

```erlang
Drafts = kura_query:from(post),
Drafts1 = kura_query:where(Drafts, {status, draft}),
Drafts2 = kura_query:select(Drafts1, [id, title, status]),

Archived = kura_query:from(post),
Archived1 = kura_query:where(Archived, {status, archived}),
Archived2 = kura_query:select(Archived1, [id, title, status]),

Q = kura_query:union(Drafts2, Archived2),
{ok, Results} = blog_repo:all(Q).
```

## Distinct

```erlang
Q = kura_query:from(post),
Q1 = kura_query:select(Q, [user_id]),
Q2 = kura_query:distinct(Q1),
{ok, UniqueAuthors} = blog_repo:all(Q2).
```

## Raw SQL escape hatch

When the query builder doesn't cover your case, use raw SQL:

```erlang
SQL = "SELECT p.id, p.title, COUNT(c.id) as comment_count "
      "FROM posts p LEFT JOIN comments c ON c.post_id = p.id "
      "GROUP BY p.id ORDER BY comment_count DESC LIMIT $1",
{ok, Results} = blog_repo:query(SQL, [10]).
```

`query/2` returns rows as maps with atom keys.

---

Next, let's cover [transactions and multi](transactions-bulk.md) for atomic multi-step operations.

# Transactional Email

Transactional email is mail the application sends because something happened: a comment on your post, an export you asked for, a link to confirm your address. This chapter wires those emails into the blog.

None of them is sent from the request. Delivering mail means talking to an SMTP relay or an HTTP API, which is slow, sometimes down and sometimes rate limited. If the comment endpoint delivered the notification itself, a slow provider would make posting a comment slow, and a failed delivery would either fail the request (although the comment was saved) or be lost. Instead the request records "this email should be sent" as a **background job**, answers immediately, and a worker process delivers the email later, retrying if the provider has a bad moment.

The jobs run on [Shigoto](https://hex.pm/packages/shigoto) (仕事, "work"), a job queue that stores jobs in PostgreSQL. [Part IX](../jobs/background-jobs.md) covers it in depth. This chapter uses three pieces of it: a little configuration, a worker module, and `shigoto:insert/1`.

## Shigoto in two minutes

Shigoto is already a dependency (`{shigoto, "~> 1.9"}`) and listed in `applications` before `hikyaku` (see the [previous chapter](sending-email.md#adding-hikyaku)). It keeps its jobs in tables in the blog's own database and talks to it through the repo's connection pool. `config/dev_sys.config.src`:

```erlang
{shigoto, [
    {repo, blog_repo},
    {queues, [{~"default", 10}, {~"mailers", 5}, {~"maintenance", 1}]}
]},
```

`repo` names the pool; `queues` lists the queues this node works on and how many jobs of each it runs at once. Mail gets its own queue, `mailers`, so a backlog of email never holds up other work.

A **worker** is a module with the `shigoto_worker` behaviour. Its `perform/1` gets the job's arguments and returns `ok`, `{error, Reason}` (try again later) or `{snooze, Seconds}` (not now, without counting as a failure). Arguments are stored as JSON, so they come back as a map with binary keys.

A **job** is inserted with `shigoto:insert/1`: a worker and its arguments. A queue process on some node picks it up, runs the worker, and records the outcome.

## Comment notifications

When Bob comments on Alice's post, Alice gets an email. Three things change.

### The worker

`src/workers/blog_comment_notification_worker.erl`:

```erlang
-module(blog_comment_notification_worker).
-behaviour(shigoto_worker).

-export([perform/1, queue/0, max_attempts/0, backoff/2, unique/0]).

queue() -> ~"mailers".

max_attempts() -> 5.

backoff(Attempt, _Reason) -> 30 * Attempt.

unique() -> #{keys => [worker, args], period => infinity}.

perform(#{~"comment_id" := CommentId}) ->
    case blog_repo:get(blog_comment, CommentId) of
        {ok, Comment} ->
            notify(blog_repo:preload(blog_comment, Comment, [author, {post, [author]}]));
        {error, not_found} ->
            ok
    end.

notify(#{user_id := AuthorId, post := #{user_id := AuthorId}}) ->
    ok;
notify(Comment) ->
    blog_mailer:deliver_from_job(blog_emails:comment_notification(Comment)).
```

- The job carries only the comment's id. The worker loads the comment, its author, its post and the post's author when it runs, so the email uses current data, and no personal data (email addresses, comment text) is stored in the job table.
- A comment deleted before the job runs is not an error: there is nothing to send, so the job succeeds.
- Commenting on your own post sends nothing.
- `queue/0`, `max_attempts/0`, `backoff/2` and `unique/0` are optional callbacks that set defaults for every job of this worker: run on `mailers`, give up after five attempts, wait 30 s, 60 s, 90 s... between them, and never queue two notifications for the same comment. [Part IX](../jobs/background-jobs.md) explains each.

The worker turns the mailer's answer into a job result with `blog_mailer:deliver_from_job/1`:

```erlang
deliver_from_job(Email) ->
    case deliver(Email) of
        {ok, _Receipt} -> ok;
        {error, #{status_code := 429}} -> {snooze, 60};
        {error, Reason} -> {error, Reason}
    end.
```

A 429 from an HTTP provider means "too many requests": the job snoozes for a minute instead of failing. Any other error fails the attempt and Shigoto retries it with the worker's backoff.

### Enqueueing

All the places the blog enqueues jobs go through one module, `src/blog_jobs.erl`:

```erlang
comment_added(#{id := CommentId}) ->
    Job = #{worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}},
    case shigoto:insert(Job) of
        {ok, _JobOrConflict} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(#{
                msg => ~"could not enqueue comment notification",
                comment_id => CommentId,
                reason => Reason
            })
    end.
```

`shigoto:insert/1` returns `{ok, Job}`, or `{ok, {conflict, Existing}}` when the worker's `unique/0` finds the same job already queued. A failed enqueue is logged, not returned: the comment is already saved and the request should succeed.

### Calling it

`blog_posts:add_comment/3` from [Part II](../data-layer/associations.md) enqueues the notification once the comment is stored:

```erlang
add_comment(#{id := PostId}, #{id := AuthorId}, Params) ->
    CS = blog_comment:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, post_id, PostId),
    case blog_repo:insert(kura_changeset:put_change(CS1, user_id, AuthorId)) of
        {ok, Comment} ->
            ok = blog_jobs:comment_added(Comment),
            {ok, Comment};
        {error, _} = Error ->
            Error
    end.
```

The controller does not change.

```admonish note title="Why not insert the comment and the job in one transaction?"
It would be nice if the comment and its job committed together. With Shigoto 1.9.12 on Kura's minato pool they cannot: each library runs its transaction on its own connection, so a job inserted inside `blog_repo:transaction/1` is committed even when the transaction rolls back. [Part IX](../jobs/background-jobs.md#transactions) shows the test that proves it. The blog therefore enqueues after the insert, and the worker is written so that a job for a comment that does not exist is harmless.
```

### Trying it

With the dev server running, Alice (user 1) has a post, and Bob (user 2) comments:

```shell
curl -s -i -X POST localhost:8080/api/posts/1/comments \
  -H 'content-type: application/json' \
  -d '{"body": "Great post!", "user_id": 2}'
```

```
HTTP/1.1 201 Created
content-length: 78
content-type: application/json
...

{"id":1,"body":"Great post!","user_id":2,"inserted_at":"2026-11-03T08:51:09Z"}
```

The response comes back at once. A few seconds later (queues poll every five seconds by default) the `mailers` queue runs the job and the logger adapter prints the email:

```
[notice] [2026-11-03T08:51:13.086250+00:00] bcc= cc= html_body="<p>Hi alice,</p>\
<p><strong>bob</strong> left a comment on your post <em>Hello jobs</em>:</p>\
<blockquote>Great post!</blockquote>\
<p><a href=\"http://localhost:8080/api/posts/1\">Read the discussion</a></p>\
" subject="bob commented on \"Hello jobs\"" text_body="Hi alice,\
\
bob left a comment on your post \"Hello jobs\":\
\
Great post!\
\
Read the discussion: http://localhost:8080/api/posts/1\
\
" to="[<<\"alice@example.com\">>]" from="Nova Blog <noreply@blog.test>" msg=hikyaku_email_delivered  ()
```

## Exporting posts by email

An author can ask for all their posts as a CSV file, delivered by email. Building the file can take a while for a prolific author, so it is a job too, in `src/workers/blog_export_worker.erl`:

```erlang
-module(blog_export_worker).
-behaviour(shigoto_worker).

-export([perform/1, queue/0]).

queue() -> ~"mailers".

perform(#{~"user_id" := UserId}) ->
    case blog_repo:get(blog_user, UserId) of
        {ok, User} ->
            Query = kura_query:order_by(
                kura_query:where(kura_query:from(blog_post), {user_id, UserId}), [{id, asc}]
            ),
            {ok, Posts} = blog_repo:all(Query),
            blog_mailer:deliver_from_job(blog_emails:posts_export(User, posts_csv(Posts)));
        {error, not_found} ->
            ok
    end.

posts_csv(Posts) ->
    Header = ~"id,title,status,inserted_at\r\n",
    Rows = [csv_row(Post) || Post <- Posts],
    iolist_to_binary([Header | Rows]).

csv_row(#{id := Id, title := Title, status := Status, inserted_at := InsertedAt}) ->
    Fields = [
        integer_to_binary(Id),
        csv_field(Title),
        atom_to_binary(Status),
        blog_api_json:datetime(InsertedAt)
    ],
    [lists:join(~",", Fields), ~"\r\n"].

csv_field(Value) ->
    <<"\"", (binary:replace(Value, ~"\"", ~"\"\"", [global]))/binary, "\"">>.
```

The title is the only free text, so it is quoted and its quotes doubled, as CSV requires. `blog_emails:posts_export/2` from the previous chapter attaches the result. Enqueueing is one line in `blog_jobs`:

```erlang
export_posts(#{id := UserId}) ->
    shigoto:insert(#{worker => blog_export_worker, args => #{~"user_id" => UserId}}).
```

Mind the [attachment warning](sending-email.md#attachments): with hikyaku 0.2.0 only the Mailgun adapter delivers the file as a proper attachment.

## Testing mail sent from jobs

Mail is now sent from a worker, in a process the test does not control. Shigoto has three ways to deal with that, and the blog's tests use all of them. Part IX's [Operations](../jobs/operations.md#testing-jobs) chapter describes them in full; here is how they look for email.

**Run the worker directly.** `shigoto:perform_job/2` runs a worker's `perform/1` through Shigoto's normal path, without a database row or a queue, and returns its result. From `test/blog_jobs_workers_SUITE.erl`:

```erlang
perform_job_sends_the_notification(Config) ->
    blog_jobs_fixtures:testing(manual),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    ok = shigoto:perform_job(blog_comment_notification_worker, #{~"comment_id" => CommentId}),
    #hikyaku_email{
        to = [#hikyaku_address{address = ~"alice@example.com"}],
        subject = ~"bob commented on \"Hello jobs\""
    } = blog_jobs_fixtures:receive_email().

rate_limited_delivery_snoozes(Config) ->
    blog_jobs_fixtures:testing(manual),
    blog_jobs_fixtures:mailer(#{
        adapter => blog_test_mail_adapter, reply => {error, #{status_code => 429}}
    }),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    {snooze, 60} =
        shigoto:perform_job(blog_comment_notification_worker, #{~"comment_id" => CommentId}).
```

`blog_test_mail_adapter` is a three-line adapter in `test/` that returns whatever its config says, so a test can play a provider that is down or rate limiting:

```erlang
-module(blog_test_mail_adapter).
-behaviour(hikyaku_adapter).

-export([deliver/2]).

deliver(_Email, #{reply := Reply}) ->
    Reply.
```

**Capture or run on insert.** In `manual` mode `shigoto:insert/1` stores the job in a buffer in the test process instead of the database, and `shigoto_testing:assert_enqueued/1` checks it. In `inline` mode the insert runs the job on the spot. From `test/blog_email_notifications_SUITE.erl`:

```erlang
comment_enqueues_a_notification(Config) ->
    blog_jobs_fixtures:testing(manual),
    {ok, #{id := CommentId}} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker,
        queue => ~"mailers",
        args => #{~"comment_id" => CommentId}
    }),
    shigoto_testing:refute_enqueued(#{worker => blog_export_worker}),
    ok = blog_jobs_fixtures:refute_email().

inline_mode_delivers_on_insert(Config) ->
    blog_jobs_fixtures:testing(inline),
    {ok, _} = blog_posts:add_comment(
        ?config(post, Config), ?config(bob, Config), #{~"body" => ~"Nice post"}
    ),
    #hikyaku_email{to = [#hikyaku_address{address = ~"alice@example.com"}]} =
        blog_jobs_fixtures:receive_email().
```

**The real thing.** With no testing mode, jobs go to the database. The test posts a comment over HTTP, sees the job waiting, drains the queue and receives the email:

```erlang
api_comment_is_mailed_by_the_queue(Config) ->
    #{id := PostId} = ?config(post, Config),
    #{id := BobId} = ?config(bob, Config),
    Path = "/api/posts/" ++ integer_to_list(PostId) ++ "/comments",
    {201, #{~"id" := CommentId}} =
        blog_data_fixtures:request(post, Path, #{body => ~"Lovely", user_id => BobId}),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker,
        args => #{~"comment_id" => CommentId},
        state => ~"available"
    }),
    ok = blog_jobs_fixtures:refute_email(),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{text_body = Text} = blog_jobs_fixtures:receive_email(),
    {_, _} = binary:match(Text, ~"Lovely"),
    shigoto_testing:assert_enqueued(#{
        worker => blog_comment_notification_worker, state => ~"completed"
    }).
```

`shigoto:drain_queue/1` runs every due job in a queue in the calling process, so the email arrives in the test process thanks to the explicit `pid` in the test adapter config.

```shell
rebar3 ct --suite=test/blog_email_notifications_SUITE
```

```
%%% blog_email_notifications_SUITE: ...
All 3 tests passed.
```

---

Background jobs carry more than email. Next, [Part IX](../jobs/background-jobs.md) looks at Shigoto properly: queues, retries, unique and scheduled jobs.

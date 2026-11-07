# Transactional Email

Transactional email is mail the application sends because something happened: a comment on your post, an export you asked for, a link to confirm your address. This chapter wires those emails into the blog.

None of them is sent from the request. Delivering mail means talking to an SMTP relay or an HTTP API, which is slow, sometimes down and sometimes rate limited. If the comment endpoint delivered the notification itself, a slow provider would make posting a comment slow, and a failed delivery would either fail the request (although the comment was saved) or be lost. Instead the request records "this email should be sent" as a **background job**, answers immediately, and a worker process delivers the email later, retrying if the provider has a bad moment.

The jobs run on [Shigoto](https://hex.pm/packages/shigoto) (仕事, "work"), a job queue that stores jobs in PostgreSQL. [Part IX](../jobs/background-jobs.md) covers it in depth. This chapter uses three pieces of it: a little configuration, a worker module, and `shigoto:insert/1`.

## Shigoto in two minutes

Shigoto is already a dependency (`{shigoto, "~> 1.10"}`) and listed in `applications` before `hikyaku` (see the [previous chapter](sending-email.md#adding-hikyaku)). It keeps its jobs in tables in the blog's own database and talks to it through the repo's connection pool. `config/dev_sys.config.src`:

```erlang
{shigoto, [
    {repo, blog_repo},
    {queues, [{~"default", 10}, {~"mailers", 5}, {~"maintenance", 1}]},
    {cron, [
        {~"purge-deleted-posts", ~"0 3 * * *", blog_purge_worker, #{~"days" => 30}}
    ]}
]},
```

`repo` names the pool; `queues` lists the queues this node works on and how many jobs of each it runs at once. Mail gets its own queue, `mailers`, so a backlog of email never holds up other work. The `cron` entry is the nightly purge of deleted posts, from [Scheduling](../jobs/scheduling.md#cron).

`blog_app:start/2` also calls `ok = migrate_jobs(),` right after `kura_migrator:migrate/1`, so that Shigoto's tables exist and its queues run on a brand new database; [Background Jobs](../jobs/background-jobs.md#shigotos-tables) shows it and explains why.

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
    shigoto:insert(#{
        worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}
    }).
```

`shigoto:insert/1` returns `{ok, Job}`, `{ok, {conflict, Existing}}` when the worker's `unique/0` finds the same job already queued, or `{error, Reason}`. `comment_added/1` hands that answer back, because the caller decides what a failed enqueue means.

### Calling it

`blog_posts:add_comment/3` from [Part II](../data-layer/associations.md) now stores the comment and its notification together, in one `kura_multi` ([Transactions and Bulk Operations](../data-layer/transactions-bulk.md#multi-named-steps)):

```erlang
add_comment(#{id := PostId}, #{id := AuthorId}, Params) ->
    CS = blog_comment:changeset(#{}, Params),
    CS1 = kura_changeset:put_change(CS, post_id, PostId),
    CS2 = kura_changeset:put_change(CS1, user_id, AuthorId),
    Multi = kura_multi:insert(kura_multi:new(), comment, CS2),
    Multi1 = kura_multi:run(Multi, notification, fun(#{comment := Comment}) ->
        blog_jobs:comment_added(Comment)
    end),
    case blog_repo:multi(Multi1) of
        {ok, #{comment := Comment}} -> {ok, Comment};
        {error, comment, Changeset, _Completed} -> {error, Changeset};
        {error, notification, Reason, _Completed} -> error({notification_not_enqueued, Reason})
    end.
```

Shigoto's jobs live in the same database, and a job inserted inside a Kura transaction joins it, so the comment and its job commit together or not at all. There is never a saved comment whose notification was lost, nor a notification for a comment that was rolled back.

- A comment that fails validation, or names a post that does not exist, stops the multi at the `comment` step. The caller gets the changeset, as before, and no job is inserted.
- If the job cannot be stored, the comment is rolled back too, and `add_comment/3` raises. The request fails with a 500 and the reader can post again; nothing half-done is left behind.
- Why a multi and not `blog_repo:transaction/1` with a `case` inside? A failed insert aborts the PostgreSQL transaction it runs in, so returning `{error, Changeset}` from the function would make the commit fail. The multi rolls back for you and returns the error.

The controller does not change. [Part IX](../jobs/background-jobs.md#transactions) has the tests that show both directions of the shared transaction.

### Trying it

With the dev server running, Alice (user 1) has a post and Bob (user 2) has an API token from `POST /api/tokens` ([Part III](../auth-sessions/authentication.md#api-tokens)). Bob comments:

```shell
curl -s -i -X POST localhost:8080/api/posts/1/comments \
  -H "authorization: Bearer $BOB_TOKEN" \
  -H 'content-type: application/json' \
  -d '{"body": "Great post!"}'
```

```
HTTP/1.1 201 Created
content-length: 78
content-type: application/json
...

{"id":1,"body":"Great post!","user_id":2,"inserted_at":"2026-11-03T09:15:51Z"}
```

The response comes back at once. Within a few seconds (queues poll every five seconds by default) the `mailers` queue runs the job and the logger adapter prints the email:

```
[notice] [2026-11-03T09:15:52.298960+00:00] bcc= cc= html_body="<p>Hi alice,</p>\
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

## Confirming addresses and resetting passwords

[Part III](../auth-sessions/authentication.md#confirmation-and-password-reset) left two `nova_auth` features without a way to reach the user: `nova_auth_confirm` issues a token that confirms an email address, `nova_auth_reset` one that sets a new password. Both tokens only work if they arrive in the user's inbox, so both flows are emails sent from a job. The blog adds:

- a confirmation email when someone registers, and a page that confirms the address;
- a "forgot your password?" form, a reset email, and a page to choose the new password.

### The token is made in the job

The job does not carry the token. It carries the kind of email and the user's id, and the worker asks `nova_auth` for a token when it runs. `src/workers/blog_account_email_worker.erl`:

```erlang
-module(blog_account_email_worker).
-behaviour(shigoto_worker).

-export([perform/1, queue/0, max_attempts/0, unique/0]).

queue() -> ~"mailers".

max_attempts() -> 5.

unique() ->
    #{
        keys => [worker, args],
        states => [available, executing, retryable, completed],
        period => 300
    }.

perform(#{~"email" := Kind, ~"user_id" := UserId}) ->
    case blog_repo:get(blog_user, UserId) of
        {ok, User} -> send(Kind, User);
        {error, not_found} -> ok
    end.

send(~"confirm", #{confirmed_at := undefined} = User) ->
    {ok, Token} = nova_auth_confirm:generate_confirm_token(blog_auth, User),
    blog_mailer:deliver_from_job(blog_emails:confirmation(User, Token));
send(~"confirm", _AlreadyConfirmed) ->
    ok;
send(~"reset", User) ->
    {ok, Token} = nova_auth_reset:generate_reset_token(blog_auth, User),
    blog_mailer:deliver_from_job(blog_emails:password_reset(User, Token)).
```

```admonish danger title="Never put a raw token in job arguments"
`nova_auth` stores only a SHA-256 hash of each token, so a copy of the `user_tokens` table is no use to an attacker. Shigoto stores job arguments as plain JSON in `shigoto_jobs`, keeps finished jobs there for 14 days and then in `shigoto_jobs_archive` for 90 more ([Pruning and archiving](../jobs/operations.md#pruning-and-archiving)). A reset token in the arguments would be a working "log in as this user" link for anyone who can read that table, a backup of it, or a jobs dashboard. Pass ids and let the worker make the secret.
```

The rest of the worker:

- The kind arrives as a binary and is matched against `~"confirm"` and `~"reset"`. Any other value fails with `function_clause`, which is the right answer for a job nobody should have inserted.
- A user who has been deleted, or confirmed in the meantime, gets nothing.
- A retry makes a fresh token. The unused one from the failed attempt is harmless: it is hashed and expires (3 days for confirmation, 1 hour for a reset).
- `unique/0` counts `completed` jobs too and looks back 300 seconds, so each user gets at most one email of each kind per five minutes, however often someone submits the "forgot your password?" form with their address. `executing` is in the list so that a request arriving while the email is being sent does not queue a second one.

`blog_jobs` enqueues it. The guard turns the atom into a binary from a fixed set, so no user input becomes an atom:

```erlang
account_email(Kind, #{id := UserId}) when Kind =:= confirm; Kind =:= reset ->
    enqueue(#{
        worker => blog_account_email_worker,
        args => #{~"email" => atom_to_binary(Kind), ~"user_id" => UserId}
    }).
```

```erlang
enqueue(#{worker := Worker, args := Args} = Job) ->
    case shigoto:insert(Job) of
        {ok, _JobOrConflict} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(#{
                msg => ~"could not enqueue job", worker => Worker, args => Args, reason => Reason
            })
    end.
```

Unlike the comment notification, a failed enqueue here is logged, not returned. The request should succeed either way: a password reset answers the same whatever happens, and a new user is already stored by `nova_auth` (below) when the confirmation is enqueued. The arguments are only ids, so they are safe to log.

### The emails

`blog_emails` gets two builders:

```erlang
confirmation(#{email := To, username := Username}, Token) ->
    Vars = #{
        username => Username,
        url => account_link(~"/confirm", Token),
        days => nova_auth:config(blog_auth, confirm_validity_days)
    },
    Email = new(To, ~"Confirm your email address"),
    with_bodies(Email, blog_email_confirm_txt_dtl, blog_email_confirm_html_dtl, Vars).

password_reset(#{email := To, username := Username}, Token) ->
    Vars = #{
        username => Username,
        url => account_link(~"/password/reset", Token),
        hours => nova_auth:config(blog_auth, reset_validity_hours)
    },
    Email = new(To, ~"Reset your password"),
    with_bodies(Email, blog_email_reset_txt_dtl, blog_email_reset_html_dtl, Vars).
```

```erlang
account_link(Path, Token) ->
    Query = uri_string:compose_query([{~"token", Token}]),
    <<(base_url())/binary, Path/binary, "?", Query/binary>>.
```

The validity periods come from `nova_auth`'s merged config, so the email cannot promise three days when the config says one. `account_link/2` is where Part III's [base64 warning](../auth-sessions/authentication.md#api-tokens) matters: a token such as `ab+c/d=` becomes `token=ab%2Bc%2Fd%3D`. Pasted into a query string unencoded, the `+` would come back as a space and the link would never work.

`src/views/blog_email_reset_txt.dtl` (the confirmation template has the same shape):

```django
{% autoescape off %}Hi {{ username }},

Someone asked to reset the password of your Nova Blog account. To choose a new password, open this link:

{{ url }}

The link works once and expires in {{ hours }} hour{{ hours|pluralize }}. If you did not ask for a reset, ignore this email; your password has not changed.
{% endautoescape %}
```

### Accounts

`blog_accounts:register/1` from Part III enqueues the confirmation once `nova_auth` has stored the user:

```erlang
register(Params) ->
    case nova_auth_accounts:register(blog_auth, fun blog_user:registration_changeset/2, Params) of
        {ok, User} ->
            ok = blog_jobs:account_email(confirm, User),
            {ok, User};
        {error, _} = Error ->
            Error
    end.
```

Three more functions wrap the rest of `nova_auth`:

```erlang
confirm(Token) when is_binary(Token) ->
    nova_auth_confirm:confirm_user(blog_auth, Token);
confirm(_Token) ->
    {error, not_found}.

request_password_reset(Email) when is_binary(Email) ->
    maybe
        {ok, Normalized} ?= blog_user:normalize_email(Email),
        {ok, User} ?= blog_repo:get_by(blog_user, [{email, Normalized}]),
        blog_jobs:account_email(reset, User)
    else
        {error, invalid_encoding} -> ok;
        {error, not_found} -> ok
    end;
request_password_reset(_Email) ->
    ok.

reset_password(Token, Params) when is_binary(Token) ->
    nova_auth_reset:reset_password(blog_auth, Token, Params);
reset_password(_Token, _Params) ->
    {error, not_found}.
```

`request_password_reset/1` returns `ok` whether or not the address has an account, so its caller cannot leak the difference. The address goes through `blog_user:normalize_email/1`, the same function registration and login use, so `Alice@Example.com` finds Alice's account. The `maybe` block runs its steps in order and jumps to `else` at the first one that does not match: an address that is not valid UTF-8 and an address with no account both end in `ok`.

### The pages

A link in an email is opened with `GET`, and a `GET` must not change anything. Mail providers and corporate security scanners fetch the links in incoming mail to check them; if `GET /password/reset?token=...` used the token, the scanner would spend it before the user clicked. So each link opens a page with a form, and the form's `POST` does the work, with the CSRF token every form in the blog carries. `src/controllers/blog_account_controller.erl`:

```erlang
-module(blog_account_controller).
-include_lib("kura/include/kura.hrl").

-export([confirm_form/1, confirm/1, forgot_form/1, send_reset/1, reset_form/1, reset/1]).

-define(BAD_LINK, ~"That link is invalid or has expired.").

confirm_form(Req) ->
    no_referrer(blog_view:render(Req, blog_confirm, #{token => query_token(Req)})).

confirm(Req) ->
    case blog_accounts:confirm(param(~"token", Req)) of
        {ok, _User} ->
            ok = blog_flash:put(Req, info, ~"Thank you, your email address is confirmed.");
        {error, _} ->
            ok = blog_flash:put(Req, error, ?BAD_LINK)
    end,
    {redirect, "/posts"}.

forgot_form(Req) ->
    blog_view:render(Req, blog_password_forgot, #{form => blog_view:form([email], #{})}).

send_reset(Req) ->
    ok = blog_accounts:request_password_reset(param(~"email", Req)),
    ok = blog_flash:put(Req, info, ~"If that address has an account, a reset link is on its way."),
    {redirect, "/login"}.

reset_form(Req) ->
    Vars = #{token => query_token(Req), errors => #{}},
    no_referrer(blog_view:render(Req, blog_password_reset, Vars)).

reset(Req) ->
    Token = param(~"token", Req),
    case blog_accounts:reset_password(Token, maps:get(params, Req, #{})) of
        {ok, _User} ->
            ok = blog_flash:put(Req, info, ~"Your password has been changed. Please log in again."),
            {redirect, "/login"};
        {error, #kura_changeset{} = CS} ->
            Vars = #{token => Token, errors => blog_view:errors(CS)},
            no_referrer(blog_view:render(Req, blog_password_reset, Vars, 422));
        {error, _} ->
            ok = blog_flash:put(Req, error, ?BAD_LINK),
            {redirect, "/password/forgot"}
    end.

query_token(Req) ->
    proplists:get_value(~"token", cowboy_req:parse_qs(Req), ~"").

param(Name, Req) ->
    maps:get(Name, maps:get(params, Req, #{}), undefined).

no_referrer({ok, Page, Opts}) ->
    Headers = #{~"content-type" => ~"text/html", ~"referrer-policy" => ~"no-referrer"},
    {ok, Page, Opts#{headers => Headers}}.
```

- The two pages whose URL holds a token send `Referrer-Policy: no-referrer`, so the token is not passed on in the `Referer` header to anything the page links to or loads. The `headers` option replaces Nova's default headers, so `content-type` has to be given again.
- The token goes from the query string into a hidden field, and from there into the `POST` body.
- A new password that fails `blog_user:password_changeset/2` re-renders the form with the errors and 422. The token is not used up by a failed attempt.
- Every token error (unknown, used, expired, not valid base64) gets the same message.
- A successful reset deletes all the user's tokens, so every browser and API client logged in as that user is logged out. That is the point of a reset: if someone else knew the old password, they are out too.

The routes, in `src/blog_router.erl`. The pages share the HTML plugins (urlencoded body, CSRF). The `POST` that sends mail also gets the login rate limiter, so the form cannot be used to send a stream of email from one address:

```erlang
#{
    prefix => "",
    security => fun blog_security:current_user/1,
    plugins => html_plugins(),
    routes => [
        {"/confirm", fun blog_account_controller:confirm_form/1, #{methods => [get]}},
        {"/confirm", fun blog_account_controller:confirm/1, #{methods => [post]}},
        {"/password/forgot", fun blog_account_controller:forgot_form/1, #{methods => [get]}},
        {"/password/reset", fun blog_account_controller:reset_form/1, #{methods => [get]}},
        {"/password/reset", fun blog_account_controller:reset/1, #{methods => [post]}}
    ]
},
#{
    prefix => "",
    security => false,
    plugins => [
        {pre_request, blog_rate_limit, login_rate_limit()} | html_plugins()
    ],
    routes => [
        {"/password/forgot", fun blog_account_controller:send_reset/1, #{methods => [post]}}
    ]
},
```

The login page links to `/password/forgot`. The templates (`blog_confirm.dtl`, `blog_password_forgot.dtl`, `blog_password_reset.dtl`) are small forms like `blog_login.dtl`. The confirmation page in full:

```django
{% extends "blog_layout.dtl" %}
{% block title %}Confirm your email{% endblock %}
{% block content %}
<h1>Confirm your email</h1>
<form method="post" action="/confirm">
  <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
  <input type="hidden" name="token" value="{{ token }}">
  <button type="submit">Confirm my email address</button>
</form>
{% endblock %}
```

```admonish warning title="Account enumeration"
The "forgot your password?" form answers the same way for every address, so it cannot be used to find out who has an account. The response time is not quite the same: an address with an account costs one more `INSERT`. If that matters to you, enqueue a job for every request and let the worker look the address up; the price is that the job then stores the email address someone typed. Registration still says "has already been taken" for a known address, which a site that hides its user list must change too.
```

### Trying it

On a fresh dev database, Alice registers through the form at `/register`. Within a few seconds the logger adapter prints her confirmation email:

```
[notice] [2026-11-03T09:12:12.221233+00:00] bcc= cc= html_body="<p>Hi alice,</p>\
<p>Confirm your email address for the Nova Blog:</p>\
<p><a href=\"http://localhost:8080/confirm?token=KduLop1lpM%2Bb9nlWrJeLARILARAE%2BIS6ai9IMgmcXk4%3D\">Confirm my email address</a></p>\
<p>The link works once and expires in 3 days. If you did not create an account, ignore this email.</p>\
" subject="Confirm your email address" text_body="Hi alice,\
\
Confirm your email address for the Nova Blog by opening this link:\
\
http://localhost:8080/confirm?token=KduLop1lpM%2Bb9nlWrJeLARILARAE%2BIS6ai9IMgmcXk4%3D\
\
The link works once and expires in 3 days. If you did not create an account, ignore this email.\
\
" to="[<<\"alice@example.com\">>]" from="Nova Blog <noreply@blog.test>" msg=hikyaku_email_delivered  ()
```

The token has two `+` and a `=`, all encoded. Opening the link:

```
HTTP/1.1 200 OK
content-length: 1236
content-type: text/html
date: Tue, 03 Nov 2026 09:12:27 GMT
referrer-policy: no-referrer
server: Cowboy/Nova
vary: accept-encoding
```

The page's form holds the decoded token:

```html
<form method="post" action="/confirm">
  <input type="hidden" name="_csrf_token" value="gyqX2ZA9Z3qn6tlvDx3BSWa+0mZrTNpyZgaScuWzY28=">
  <input type="hidden" name="token" value="KduLop1lpM+b9nlWrJeLARILARAE+IS6ai9IMgmcXk4=">
  <button type="submit">Confirm my email address</button>
</form>
```

Submitting it redirects to `/posts` with "Thank you, your email address is confirmed.", and the database agrees:

```
 id |       email       |      confirmed_at      
----+-------------------+------------------------
  1 | alice@example.com | 2026-11-03 09:12:34+00
  2 | bob@example.com   | 
```

Bob, who has not confirmed yet, has forgotten his password. `POST /password/forgot` with his address and with an address nobody has both answer the same:

```
HTTP/1.1 302 Found
location: /login
```

and the login page shows "If that address has an account, a reset link is on its way." Only Bob gets an email:

```
[notice] [2026-11-03T09:12:47.221814+00:00] bcc= cc= html_body="<p>Hi bob,</p>\
<p>Someone asked to reset the password of your Nova Blog account.</p>\
<p><a href=\"http://localhost:8080/password/reset?token=%2BEwvQ%2FZOmz4MbbdG99%2FCrfyqaUUNguM2QW21E0TI84o%3D\">Choose a new password</a></p>\
<p>The link works once and expires in 1 hour. If you did not ask for a reset, ignore this email; your password has not changed.</p>\
" subject="Reset your password" text_body="Hi bob,\
...
```

On the page behind that link, a password that is too short comes back with 422 and "should be at least 12 character(s)"; a good one redirects to `/login`. Submitting the same form again redirects to `/password/forgot` with "That link is invalid or has expired."

```admonish warning title="The logger adapter logs live tokens"
The logger adapter writes the whole email to the log, links included, so in dev the log holds working confirmation and reset links. That is what makes it useful in development, and why it must never be the adapter in production: logs are shipped, kept and read by more people than mailboxes are.
```

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

In `inline` mode the worker runs inside `add_comment/3`'s transaction, before the comment is committed. It finds the comment only because the sandbox is shared, so every process uses the test's connection. Outside a sandbox the worker would not see the uncommitted comment and would send nothing, one more reason `inline` is a test-only mode.

**The real thing.** With no testing mode, jobs go to the database. The test posts a comment over HTTP, sees the job waiting, drains the queue and receives the email:

```erlang
api_comment_is_mailed_by_the_queue(Config) ->
    #{id := PostId} = ?config(post, Config),
    Token = blog_data_fixtures:token(?config(bob, Config)),
    Path = "/api/posts/" ++ integer_to_list(PostId) ++ "/comments",
    {201, #{~"id" := CommentId}} =
        blog_data_fixtures:request_as(Token, post, Path, #{body => ~"Lovely"}),
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

The account emails are tested end to end in `test/blog_email_accounts_SUITE.erl`: the suite registers through the form, drains the queue, pulls the link out of the email and follows it like a browser. The link is pulled out with a regular expression and the token read back with `uri_string`, so a token that was not encoded would fail here:

```erlang
registration_mails_a_confirmation_link(_Config) ->
    {200, _, Form, Jar} = blog_web:get(#{}, "/register"),
    {302, _, _, Jar1} = blog_web:post_form(Jar, "/register", [
        {"_csrf_token", blog_web:csrf_token(Form)},
        {"email", "carol@example.com"},
        {"username", "carol"},
        {"password", ?PASSWORD},
        {"password_confirmation", ?PASSWORD}
    ]),
    {ok, #{id := CarolId, confirmed_at := undefined}} =
        blog_repo:get_by(blog_user, [{email, ~"carol@example.com"}]),
    shigoto_testing:assert_enqueued(#{
        worker => blog_account_email_worker,
        args => #{~"email" => ~"confirm", ~"user_id" => CarolId}
    }),
    ok = shigoto:drain_queue(~"mailers"),
    #hikyaku_email{
        subject = ~"Confirm your email address",
        to = [#hikyaku_address{address = ~"carol@example.com"}],
        text_body = Text
    } = blog_jobs_fixtures:receive_email(),
    "/confirm?" ++ _ = Link = link_in(Text),
    {200, Headers, Page, Jar2} = blog_web:get(Jar1, Link),
    "no-referrer" = blog_web:header("referrer-policy", Headers),
    Fields = [{"_csrf_token", blog_web:csrf_token(Page)}, {"token", token_in(Link)}],
    {302, _, _, Jar3} = blog_web:post_form(Jar2, "/confirm", Fields),
    {ok, #{confirmed_at := {{_, _, _}, {_, _, _}}}} = blog_repo:get(blog_user, CarolId),
    {302, _, _, Jar4} = blog_web:post_form(Jar3, "/confirm", Fields),
    {200, _, Posts, _} = blog_web:get(Jar4, "/posts"),
    ?assertNotEqual(nomatch, binary:match(Posts, ~"That link is invalid or has expired.")).
```

`assert_enqueued/1` with the exact `args` map also proves the job holds no token. The other tests cover the reset flow (a rejected password, a used link, the old API token revoked), the same answer for an unknown address, the five-minute limit, and a deterministic check that `ab+c/d=` is encoded as `ab%2Bc%2Fd%3D`. `POST /register` and `POST /password/forgot` share the login rate limiter, so `init_per_testcase/2` calls `blog_web:reset_rate_limit/0`.

```
%%% blog_email_accounts_SUITE: ......
All 6 tests passed.
```

---

Background jobs carry more than email. Next, [Part IX](../jobs/background-jobs.md) looks at Shigoto properly: queues, retries, unique and scheduled jobs.

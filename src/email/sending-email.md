# Sending Email

A blog sends mail: a note to an author when someone comments, a confirmation link after registration, a password reset. [Hikyaku](https://hex.pm/packages/hikyaku) (飛脚, the runners who carried letters in Edo Japan) is the email library of the Nova ecosystem. It has three parts:

- **a builder** (`hikyaku_email`, `hikyaku_attachment`) that produces an email record,
- **a mailer behaviour** (`hikyaku_mailer`) for your own mailer module, which knows how to deliver,
- **adapters** that do the delivery: SMTP, three HTTP providers, a logger for development and a test adapter.

This chapter builds and delivers email. The next one wires mail into the blog's flows and sends it from background jobs instead of from the request.

## Adding Hikyaku

The blog already depends on it in `rebar.config`:

```erlang
{deps, [
    ...
    {hikyaku, "~> 0.3"},
    ...
]}.
```

and starts it with the application, in `src/blog.app.src`:

```erlang
{applications, [
    kernel,
    stdlib,
    nova,
    kura,
    kura_postgres,
    nova_auth,
    nova_json_schemas,
    arizona_nova,
    shigoto,
    hikyaku
]},
```

(`shigoto` is the job queue from the next chapter and Part IX.)

```admonish note title="Hex has 0.2.0"
The Hex release is hikyaku 0.2.0; GitHub is ahead. Everything in this chapter is checked against the 0.2.0 source in `_build/default/lib/hikyaku`. Where 0.2.0 has a problem that is fixed upstream, the chapter says so.
```

## A mailer module

A mailer is a module with the `hikyaku_mailer` behaviour. Its one callback, `config/0`, returns a map with the adapter and that adapter's settings. `hikyaku_mailer:deliver(Mailer, Email)` validates the email, calls `config/0` and hands the email to the adapter.

`src/mail/blog_mailer.erl`:

```erlang
-module(blog_mailer).
-behaviour(hikyaku_mailer).

-export([config/0, deliver/1, deliver_from_job/1]).

config() ->
    application:get_env(blog, mailer, #{adapter => hikyaku_adapter_logger}).

deliver(Email) ->
    hikyaku_mailer:deliver(?MODULE, Email).
```

(`deliver_from_job/1` comes in the next chapter.)

`config/0` reads the blog's application environment, so every environment can choose its own adapter without code changes. The development config logs mail instead of sending it, in `config/dev_sys.config.src`:

```erlang
{blog, [
    {json_lib, blog_json},
    {ops_token, ~"dev-ops-token"},
    {mailer, #{adapter => hikyaku_adapter_logger, level => notice}},
    {mail_from, {~"Nova Blog", ~"noreply@blog.test"}}
]},
```

and the test config uses the test adapter (`config/test_sys.config`):

```erlang
{blog, [
    {json_lib, blog_json},
    {ops_token, ~"test-ops-token"},
    {mailer, #{adapter => hikyaku_adapter_test}}
]},
```

`config/0` is called on every delivery, so a test can point the mailer somewhere else with `application:set_env/3` and the next email goes there.

## Building an email

Every builder function takes an email and returns a new one:

```erlang
1> E0 = hikyaku_email:new().
2> E1 = hikyaku_email:from(E0, {~"Nova Blog", ~"noreply@blog.test"}).
3> E2 = hikyaku_email:to(E1, ~"alice@example.com").
4> E3 = hikyaku_email:subject(E2, ~"Welcome!").
5> E4 = hikyaku_email:text_body(E3, ~"Thanks for joining the blog.").
6> blog_mailer:deliver(E4).
=NOTICE REPORT==== 3-Nov-2026::09:53:46.066628 ===
    msg: <<"hikyaku_email_delivered">>
    from: <<"Nova Blog <noreply@blog.test>">>
    to: [<<"alice@example.com">>]
    cc: []
    bcc: []
    subject: <<"Welcome!">>
    text_body: <<"Thanks for joining the blog.">>
    html_body: undefined
{ok,#{status => logged}}
```

That is a `rebar3 shell` session (output of steps 1 to 5 trimmed). The logger adapter printed the email and returned `{ok, #{status => logged}}`.

| Function | Purpose |
|---|---|
| `hikyaku_email:new/0` | An empty email |
| `from/2`, `reply_to/2` | Set the sender or the reply-to address |
| `to/2`, `cc/2`, `bcc/2` | Append one recipient |
| `put_to/2`, `put_cc/2`, `put_bcc/2` | Replace the whole list |
| `subject/2` | Set the subject |
| `text_body/2`, `html_body/2` | Set the plain text or HTML body |
| `attachment/2` | Append a `hikyaku_attachment` |
| `header/3` | Add a custom header (`Key` and `Value` are binaries); a reserved name such as `Reply-To` or a value with a line break raises |
| `provider_option/3` | Store an adapter-specific option (atom key) |

An address is a binary (`~"alice@example.com"`), a `{Name, Address}` tuple, or a `#hikyaku_address{}` record. All three are normalised to the record. Each must be one bare mailbox: `~"a@x.com, b@y.com"` or a name with a line break raises `{invalid_address, ...}` when you set it, so a value from a form cannot add recipients.

### Reading an email

The email is a record, `#hikyaku_email{}`, defined in `hikyaku/include/hikyaku.hrl`. `hikyaku_email` has a getter for each field (`subject/1`, `to/1`, `text_body/1`, `attachments/1`, ...), but addresses and attachments come back as `#hikyaku_address{}` and `#hikyaku_attachment{}` records, so code that looks inside them needs the header anyway. In the shell:

```erlang
8> rr(hikyaku_email).
[hikyaku_address,hikyaku_attachment,hikyaku_email]
9> E4.
#hikyaku_email{subject = <<"Welcome!">>,
               from = #hikyaku_address{name = <<"Nova Blog">>,
                                       address = <<"noreply@blog.test">>},
               to = [#hikyaku_address{name = undefined,
                                      address = <<"alice@example.com">>}],
               cc = [],bcc = [],reply_to = undefined,
               text_body = <<"Thanks for joining the blog.">>,
               html_body = undefined,attachments = [],headers = #{},
               provider_options = #{}}
```

In a module, `-include_lib("hikyaku/include/hikyaku.hrl").` gives you the same records. The tests later in this part match on them, which checks several fields in one pattern.

### Validation

`hikyaku_mailer:deliver/2` refuses an email without a sender or without any recipient, before the adapter is called:

```erlang
7> blog_mailer:deliver(hikyaku_email:to(hikyaku_email:new(), ~"alice@example.com")).
{error,{validation,<<"from is required">>}}
```

From `test/blog_email_SUITE.erl`:

```erlang
mailer_requires_sender_and_recipient(_Config) ->
    NoSender = hikyaku_email:to(hikyaku_email:new(), ~"alice@example.com"),
    {error, {validation, ~"from is required"}} = blog_mailer:deliver(NoSender),
    NoRecipient = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    {error, {validation, ~"at least one recipient (to, cc, or bcc) is required"}} =
        blog_mailer:deliver(NoRecipient).
```

## Bodies from templates

Email bodies hold user content: a comment, a post title, a username. In HTML that content must be escaped, or a comment containing `<script>` ends up in the author's inbox as markup. The blog already has a template engine that escapes by default: ErlyDTL, from [Part IV](../views-templates/templates.md). Email templates live next to the page templates in `src/views/` and compile to modules the same way (`blog_email_comment_html.dtl` becomes `blog_email_comment_html_dtl`).

`src/views/blog_email_comment_html.dtl`:

```html
<p>Hi {{ author }},</p>
<p><strong>{{ commenter }}</strong> left a comment on your post <em>{{ title }}</em>:</p>
<blockquote>{{ comment }}</blockquote>
<p><a href="{{ url }}">Read the discussion</a></p>
```

The plain text version must not be HTML-escaped (an `&` in a title should stay an `&`), so it turns autoescaping off. `src/views/blog_email_comment_txt.dtl`:

```
{% autoescape off %}Hi {{ author }},

{{ commenter }} left a comment on your post "{{ title }}":

{{ comment }}

Read the discussion: {{ url }}
{% endautoescape %}
```

The functions that build the blog's emails live in one module, `src/mail/blog_emails.erl`. They only build; they never deliver, which keeps them easy to test:

```erlang
-module(blog_emails).

-export([comment_notification/1, posts_export/2]).

comment_notification(#{post := Post, author := Commenter, body := Body}) ->
    #{title := Title, author := #{email := To, username := AuthorName}} = Post,
    #{username := CommenterName} = Commenter,
    Vars = #{
        author => AuthorName,
        commenter => CommenterName,
        title => Title,
        comment => Body,
        url => post_url(Post)
    },
    Email = new(To, <<CommenterName/binary, " commented on \"", Title/binary, "\"">>),
    with_bodies(Email, blog_email_comment_txt_dtl, blog_email_comment_html_dtl, Vars).

posts_export(#{email := To, username := Username}, Csv) ->
    E0 = new(To, ~"Your posts export"),
    E1 = hikyaku_email:text_body(
        E0, <<"Hi ", Username/binary, ",\r\n\r\nYour posts are attached as a CSV file.\r\n">>
    ),
    hikyaku_email:attachment(E1, hikyaku_attachment:from_data(~"posts.csv", Csv)).

new(To, Subject) ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, application:get_env(blog, mail_from, ~"noreply@blog.test")),
    E2 = hikyaku_email:to(E1, To),
    hikyaku_email:subject(E2, Subject).

with_bodies(Email, TextTemplate, HtmlTemplate, Vars) ->
    E1 = hikyaku_email:text_body(Email, render(TextTemplate, Vars)),
    hikyaku_email:html_body(E1, render(HtmlTemplate, Vars)).

render(Template, Vars) ->
    {ok, IoData} = Template:render(Vars),
    iolist_to_binary(IoData).

post_url(#{id := Id}) ->
    BaseUrl = application:get_env(blog, base_url, ~"http://localhost:8080"),
    <<BaseUrl/binary, "/api/posts/", (integer_to_binary(Id))/binary>>.
```

`comment_notification/1` takes a comment with its author and its post (and the post's author) preloaded, the shape `blog_repo:preload(blog_comment, Comment, [author, {post, [author]}])` returns. The sender comes from the `mail_from` setting, the link base from `base_url`.

The test feeds it a hostile comment and checks both bodies, from `test/blog_email_SUITE.erl`:

```erlang
comment_email_escapes_html(_Config) ->
    Alice = blog_data_fixtures:user(~"alice"),
    Bob = blog_data_fixtures:user(~"bob"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Tom & Jerry"}),
    {ok, Comment} = blog_posts:add_comment(Post, Bob, #{~"body" => ~"<script>alert(1)</script>"}),
    Loaded = blog_repo:preload(blog_comment, Comment, [author, {post, [author]}]),
    #hikyaku_email{subject = Subject, text_body = Text, html_body = Html} =
        blog_emails:comment_notification(Loaded),
    ~"bob commented on \"Tom & Jerry\"" = Subject,
    {_, _} = binary:match(Text, ~"<script>alert(1)</script>"),
    {_, _} = binary:match(Html, ~"&lt;script&gt;alert(1)&lt;/script&gt;"),
    {_, _} = binary:match(Html, ~"<em>Tom &amp; Jerry</em>"),
    nomatch = binary:match(Html, ~"<script>").
```

The subject is built by hand rather than from a template: it is a header, never rendered as HTML. A title containing a line break cannot start a new header over SMTP either; gen_smtp encodes it (`Subject: =?UTF-8?Q?Hi=0D=0ABcc=3A_...?=`).

## Attachments

`hikyaku_attachment` builds attachments:

| Function | Result |
|---|---|
| `from_data(Filename, Data)` | An attachment from a binary, content type guessed from the extension |
| `from_data(Filename, Data, Opts)` | Same, `Opts` may set `content_type` and `filename` |
| `from_path(Path)`, `from_path(Path, Opts)` | Read from a file at delivery time |
| `inline(Cid, Path)` | An inline part with a Content-ID, referenced from HTML as `<img src="cid:logo">` |

The blog's export email attaches a CSV it built in memory: `hikyaku_attachment:from_data(~"posts.csv", Csv)` in `posts_export/2` above. `.csv` maps to `text/csv`; unknown extensions become `application/octet-stream`.

An inline logo, as a variant (compiled in a scratch module, not part of the blog):

```erlang
welcome_with_logo(To) ->
    E0 = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    E1 = hikyaku_email:to(E0, To),
    E2 = hikyaku_email:html_body(E1, ~"<img src=\"cid:logo\" alt=\"Nova Blog\"><p>Welcome!</p>"),
    hikyaku_email:attachment(E2, hikyaku_attachment:inline(~"logo", ~"priv/assets/logo.png")).
```

Every adapter that delivers sends attachments with their filename. Over SMTP and SES the part goes out as `Content-Disposition: attachment; filename=posts.csv`, base64-encoded; `smtp_attachment_keeps_its_filename` in `test/blog_email_SUITE.erl` builds the MIME message the SMTP adapter would send and checks that. A content type you pass yourself must be a well-formed `type/subtype` (parameters such as `; charset=utf-8` are fine); anything else, for example a value with a line break, is refused before it can reach a header.

## Adapters

Each adapter reads its own keys from the map `config/0` returns. From the 0.3.0 source:

| Adapter | Delivers via | Keys |
|---|---|---|
| `hikyaku_adapter_logger` | OTP logger | `level` (default `notice`) |
| `hikyaku_adapter_test` | a message `{hikyaku_email, Email}` | `pid` (default: the calling process) |
| `hikyaku_adapter_smtp` | SMTP, through gen_smtp | `relay` (binary), `port` (default 587), `username`, `password`, `ssl` (`true`/`false`), `tls` (`always`, `if_available`, `never`), `tls_options`, `no_mx_lookups` (default `false`) |
| `hikyaku_adapter_mailgun` | Mailgun Messages API | `api_key`, `domain`, `base_url` (default `https://api.mailgun.net`; use `https://api.eu.mailgun.net` for EU accounts) |
| `hikyaku_adapter_sendgrid` | SendGrid v3 API | `api_key` |
| `hikyaku_adapter_ses` | Amazon SES v2 API, SigV4-signed | `access_key`, `secret_key`, `region` |

The HTTP adapters also accept `http_client`, a module with `post/3`, in place of the built-in `httpc` client. All HTTP adapters answer `{ok, #{status => sent, status_code => Code}}` for a 2xx, and `{error, #{status_code => Code, body => Body}}` otherwise.

The built-in HTTP client verifies the provider's certificate (`verify_peer` against `public_key:cacerts_get()`), and `hikyaku` already starts `inets` and `ssl`.

### SMTP and TLS

`tls` defaults to `always`: the adapter requires STARTTLS and verifies the relay's certificate against the operating system's trust store, with `relay` as the name it must be valid for. A relay that does not offer STARTTLS fails the delivery instead of receiving the password and the message in clear. Two cases default to `if_available`, STARTTLS when the relay offers it: `ssl => true` (implicit TLS, usually port 465, already encrypted from the first byte) and a loopback relay (`localhost`, `127.0.0.1`) without `username`, such as a local MTA.

For a relay on a private network that has no STARTTLS, say so explicitly:

```erlang
#{adapter => hikyaku_adapter_smtp, relay => ~"mailrelay.internal", port => 25, tls => if_available}
```

That is a downgrade: anyone on the path can strip STARTTLS and read the session, so use it only where you trust the network itself.

When `relay` is a domain, the adapter looks up its MX records and connects to those hosts, as any mail server would, falling back to `relay` itself. For a provider's submission host (`smtp.example.com`), set `no_mx_lookups => true` to connect to exactly the host you configured.

A production mailer config, as a variant (Mailgun's EU region; where the key comes from is covered in the [Configuration](../production/configuration.md) chapter):

```erlang
{blog, [
    {mailer, #{
        adapter => hikyaku_adapter_mailgun,
        api_key => ~"key-...",
        domain => ~"mg.blog.example",
        base_url => ~"https://api.eu.mailgun.net"
    }},
    {mail_from, {~"Nova Blog", ~"noreply@blog.example"}},
    {base_url, ~"https://blog.example"}
]}
```

## The test adapter

`hikyaku_adapter_test` sends `{hikyaku_email, Email}` to the `pid` in its config and returns `{ok, #{status => sent}}`. Without a `pid` it sends to the process that called `deliver`, which is only useful when that is your test process. The blog's tests set the pid explicitly with a fixture in `test/blog_jobs_fixtures.erl`:

```erlang
capture_mail() ->
    mailer(#{adapter => hikyaku_adapter_test, pid => self()}).

mailer(Config) ->
    application:set_env(blog, mailer, Config).

receive_email() ->
    receive
        {hikyaku_email, Email} -> Email
    after 2000 -> error(no_email_delivered)
    end.

refute_email() ->
    receive
        {hikyaku_email, Email} -> error({unexpected_email, Email})
    after 200 -> ok
    end.
```

and use it like this:

```erlang
test_adapter_sends_to_a_process(_Config) ->
    E0 = hikyaku_email:from(hikyaku_email:new(), ~"noreply@blog.test"),
    E1 = hikyaku_email:to(E0, ~"alice@example.com"),
    Email = hikyaku_email:text_body(hikyaku_email:subject(E1, ~"Hi"), ~"Hello Alice"),
    {ok, #{status := sent}} = blog_mailer:deliver(Email),
    Email = blog_jobs_fixtures:receive_email().
```

Setting the pid matters as soon as mail is sent from another process, which is exactly what happens next.

```shell
rebar3 ct --suite=test/blog_email_SUITE
```

```
%%% blog_email_SUITE: .......
All 7 tests passed.
```

---

Next: [Transactional Email](transactional-email.md), where the blog sends these emails from background jobs.

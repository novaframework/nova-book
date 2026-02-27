# Sending Email

[Hikyaku](https://github.com/Taure/hikyaku) is a composable email library for Erlang with pluggable adapters. It handles building and delivering emails without tying you to a specific provider.

## Adding Hikyaku

Add the dependency to `rebar.config`:

```erlang
{deps, [
    nova,
    {kura, "~> 1.0"},
    {hikyaku, "~> 0.1"}
]}.
```

Add `hikyaku` to your application dependencies in `src/blog.app.src`:

```erlang
{applications,
 [kernel,
  stdlib,
  nova,
  kura,
  hikyaku
 ]},
```

## Creating a mailer

Hikyaku uses a behaviour-based pattern. Define a mailer module that configures the delivery adapter:

```erlang
-module(blog_mailer).
-behaviour(hikyaku_mailer).
-export([config/0]).

config() ->
    #{adapter => hikyaku_adapter_logger}.
```

The `hikyaku_adapter_logger` prints emails to the console — perfect for development. We'll switch to a real adapter for production.

### Available adapters

| Adapter | Service | Config keys |
|---|---|---|
| `hikyaku_adapter_smtp` | Any SMTP server | `relay`, `port`, `username`, `password`, `tls` |
| `hikyaku_adapter_sendgrid` | SendGrid v3 API | `api_key` |
| `hikyaku_adapter_mailgun` | Mailgun | `api_key`, `domain` |
| `hikyaku_adapter_ses` | Amazon SES v2 | `access_key`, `secret_key`, `region` |
| `hikyaku_adapter_logger` | Console output | `level` |
| `hikyaku_adapter_test` | Test assertions | `pid` |

## Building an email

Hikyaku uses a builder API — each function takes an email record and returns a new one:

```erlang
E0 = hikyaku_email:new(),
E1 = hikyaku_email:from(E0, {<<"Blog">>, <<"noreply@myblog.com">>}),
E2 = hikyaku_email:to(E1, {<<"Alice">>, <<"alice@example.com">>}),
E3 = hikyaku_email:subject(E2, <<"Welcome to the Blog!">>),
E4 = hikyaku_email:text_body(E3, <<"Thanks for signing up, Alice.">>),
E5 = hikyaku_email:html_body(E4, <<"<h1>Welcome!</h1><p>Thanks for signing up.</p>">>),

{ok, _} = hikyaku_mailer:deliver(blog_mailer, E5).
```

### Builder functions

| Function | Purpose |
|---|---|
| `hikyaku_email:new/0` | Create a new email |
| `hikyaku_email:from/2` | Set the sender (`{Name, Address}` or `Address`) |
| `hikyaku_email:to/2` | Add a recipient |
| `hikyaku_email:cc/2` | Add a CC recipient |
| `hikyaku_email:bcc/2` | Add a BCC recipient |
| `hikyaku_email:reply_to/2` | Set the reply-to address |
| `hikyaku_email:subject/2` | Set the subject line |
| `hikyaku_email:text_body/2` | Set the plain text body |
| `hikyaku_email:html_body/2` | Set the HTML body |
| `hikyaku_email:header/3` | Add a custom header |
| `hikyaku_email:attachment/2` | Add an attachment |

## Creating email helper functions

Organize your emails in a dedicated module:

```erlang
-module(blog_emails).
-export([welcome/1, comment_notification/2]).

welcome(#{email := Email, username := Username}) ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, {<<"Nova Blog">>, <<"noreply@myblog.com">>}),
    E2 = hikyaku_email:to(E1, Email),
    E3 = hikyaku_email:subject(E2, <<"Welcome to Nova Blog!">>),
    E4 = hikyaku_email:text_body(E3,
        <<"Hi ", Username/binary, ",\n\n",
          "Thanks for joining Nova Blog.\n\n",
          "— The Blog Team">>),
    E5 = hikyaku_email:html_body(E4,
        <<"<h1>Welcome, ", Username/binary, "!</h1>",
          "<p>Thanks for joining Nova Blog.</p>">>),
    hikyaku_mailer:deliver(blog_mailer, E5).

comment_notification(Post, Comment) ->
    AuthorEmail = maps:get(email, maps:get(author, Post)),
    CommentAuthor = maps:get(username, maps:get(author, Comment)),
    PostTitle = maps:get(title, Post),

    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, {<<"Nova Blog">>, <<"noreply@myblog.com">>}),
    E2 = hikyaku_email:to(E1, AuthorEmail),
    E3 = hikyaku_email:subject(E2,
        <<CommentAuthor/binary, " commented on \"", PostTitle/binary, "\"">>),
    E4 = hikyaku_email:text_body(E3,
        <<CommentAuthor/binary, " left a comment on your post \"",
          PostTitle/binary, "\":\n\n",
          (maps:get(body, Comment))/binary>>),
    hikyaku_mailer:deliver(blog_mailer, E4).
```

## Attachments

```erlang
Attachment = hikyaku_attachment:from_data(CsvData, <<"export.csv">>),
E0 = hikyaku_email:new(),
E1 = hikyaku_email:from(E0, <<"noreply@myblog.com">>),
E2 = hikyaku_email:to(E1, <<"alice@example.com">>),
E3 = hikyaku_email:subject(E2, <<"Your export is ready">>),
E4 = hikyaku_email:text_body(E3, <<"See attached.">>),
E5 = hikyaku_email:attachment(E4, Attachment),

{ok, _} = hikyaku_mailer:deliver(blog_mailer, E5).
```

For inline images (e.g. in HTML emails):

```erlang
LogoAttachment = hikyaku_attachment:from_data(LogoData, <<"logo.png">>),
InlineAttachment = hikyaku_attachment:inline(LogoAttachment, <<"logo">>),

E0 = hikyaku_email:new(),
E1 = hikyaku_email:html_body(E0, <<"<h1>Hello</h1><img src=\"cid:logo\">">>),
E2 = hikyaku_email:attachment(E1, InlineAttachment),
hikyaku_mailer:deliver(blog_mailer, E2).
```

## Production adapter configuration

### SendGrid

```erlang
-module(blog_mailer).
-behaviour(hikyaku_mailer).
-export([config/0]).

config() ->
    #{adapter => hikyaku_adapter_sendgrid,
      api_key => application:get_env(blog, sendgrid_api_key, <<>>)}.
```

### Amazon SES

```erlang
config() ->
    #{adapter => hikyaku_adapter_ses,
      access_key => application:get_env(blog, aws_access_key, <<>>),
      secret_key => application:get_env(blog, aws_secret_key, <<>>),
      region => <<"us-east-1">>}.
```

### SMTP

```erlang
config() ->
    #{adapter => hikyaku_adapter_smtp,
      relay => <<"smtp.example.com">>,
      port => 587,
      username => application:get_env(blog, smtp_user, <<>>),
      password => application:get_env(blog, smtp_pass, <<>>),
      tls => always}.
```

---

Next, let's build [Transactional Email](transactional-email.md) flows — registration confirmation, password reset, and notifications.

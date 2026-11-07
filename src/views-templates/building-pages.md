# Building Pages

The blog has a JSON API and logins. This chapter gives it ordinary HTML pages that work without JavaScript: a post list, a post page, forms to write and edit posts, and the login and registration forms from [Authentication](../auth-sessions/authentication.md). They share a layout, a small view helper module, a few partials and one self-hosted stylesheet.

## The layout

Every page extends `src/views/blog_layout.dtl`:

```django
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{% block title %}Posts{% endblock %} - Nova Blog</title>
  <link rel="stylesheet" href="/assets/css/blog.css">
  <link rel="icon" href="/assets/favicon.ico">
</head>
<body>
  <header class="site-header">
    <a class="brand" href="/posts">Nova Blog</a>
    <nav>
      {% if current_user %}
        <a href="/posts/new">New post</a>
        {% if current_user.admin %}<a href="/admin/users">Users</a>{% endif %}
        <span class="who">{{ current_user.username }}</span>
        <form class="inline" method="post" action="/logout">
          <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
          <button type="submit">Log out</button>
        </form>
      {% else %}
        <a href="/login">Log in</a>
        <a href="/register">Register</a>
      {% endif %}
    </nav>
  </header>
  <main>
    {% include "partials/flash.dtl" %}
    {% block content %}{% endblock %}
  </main>
  <footer class="site-footer">Built with Nova</footer>
</body>
</html>
```

Logout is a form, not a link, because it is a `POST` with a CSRF token. Every page needs `current_user`, `flash` and `csrf_token`. The CSRF plugin provides the last one; a helper module provides the other two.

## blog_view

Controllers render through `src/blog_view.erl` instead of returning `{ok, Vars, Options}` themselves:

```erlang
-module(blog_view).
-include_lib("kura/include/kura.hrl").

-export([render/3, render/4, post/1, form/2, errors/1, forbidden/0]).

render(Req, View, Vars) ->
    render(Req, View, Vars, 200).

render(Req, View, Vars, Status) ->
    Page = Vars#{flash => blog_flash:take(Req), current_user => current_user(Req)},
    {ok, Page, #{view => View, status_code => Status}}.

current_user(#{auth_data := #{username := Username, role := Role}}) ->
    #{username => Username, admin => Role =:= admin};
current_user(_Req) ->
    false.
```

`render/4` adds the flash messages (taking them out of the session, so each shows once) and the current user. The template gets only what it shows: a username and an `admin` flag, never the actor with its `id`, and `false` rather than `undefined` for a visitor.

### Presenting records

Kura records hold atoms, tuples and `undefined`. Templates are happier with binaries, so `post/1` turns a post into exactly what the pages print:

```erlang
post(Post) ->
    Base = #{
        id => maps:get(id, Post),
        title => maps:get(title, Post),
        body => maps:get(body, Post),
        paragraphs => paragraphs(maps:get(body, Post)),
        status => atom_to_binary(maps:get(status, Post)),
        lock_version => maps:get(lock_version, Post),
        inserted_at => maps:get(inserted_at, Post)
    },
    case Post of
        #{author := #{username := Username}} -> Base#{author => Username};
        _ -> Base
    end.

paragraphs(Text) ->
    Lines = binary:replace(Text, ~"\r\n", ~"\n", [global]),
    [string:trim(P) || P <- binary:split(Lines, ~"\n\n", [global, trim_all])].
```

`paragraphs/1` splits the body on blank lines. Browsers submit textareas with `\r\n` line endings, hence the replace first. The template escapes each paragraph and wraps it in `<p>`, which is what the `linebreaks` filter would do if it worked with autoescaping ([ErlyDTL Templates](templates.md#escaping) explains why it does not).

### Forms

A form is filled either from a record (editing) or from the submitted params (after an error). The two use different keys: atoms in records, binaries in params. `form/2` reads either and always returns binaries:

```erlang
form(Fields, Source) ->
    #{Field => text(value(Field, Source)) || Field <- Fields}.

value(Field, Source) ->
    case Source of
        #{Field := Value} -> Value;
        #{} -> maps:get(atom_to_binary(Field), Source, ~"")
    end.

text(Value) when is_binary(Value) -> Value;
text(Value) when is_atom(Value), Value =/= undefined -> atom_to_binary(Value);
text(_Value) -> ~"".
```

`undefined` (an empty nullable column) becomes an empty string, and `draft` becomes `~"draft"`, which the status `<select>` compares against. No form field ever renders `undefined`, `null` or `nil`. Only the listed fields are copied, so the password a user typed is never sent back into the page.

`errors/1` turns a changeset into a map of field to messages, the same shape the JSON API returns:

```erlang
errors(#kura_changeset{} = CS) ->
    blog_api_json:errors(CS).
```

## Partials

Small templates included by others live in `src/views/partials/`. They compile to `flash_dtl`, `errors_dtl`, `post_card_dtl` and `post_form_dtl`.

```admonish warning title="No leading underscore"
Django projects often name partials `_flash.html`. Do not do that here: the scaffold's `.gitignore` contains `_*`, which ignores every file whose name starts with an underscore. Your partials would work on your machine and be missing from the repository. A `partials/` directory does the same job.
```

The flash partial, `src/views/partials/flash.dtl`:

```django
{% for message in flash %}
<p class="flash flash-{{ message.kind }}" role="status">{{ message.text }}</p>
{% endfor %}
```

`blog_flash:take/1` returns `[#{kind => ~"info", text => ~"Post created."}]`, so the class is `flash-info` or `flash-error`.

The field errors partial, `src/views/partials/errors.dtl`, is included once per field with the field's messages:

```django
{% if messages %}{% for message in messages %}<span class="error">{{ message }}</span>{% endfor %}{% endif %}
```

The `if` is not decoration: a field without errors gives `messages` the value `undefined`, and a `for` over a `with`-bound `undefined` crashes the render (see [Missing values](templates.md#missing-values)).

## The post list

`blog_posts_html_controller:index/1` lists published posts, ten to a page, with their authors:

```erlang
index(Req) ->
    Query = maps:from_list(cowboy_req:parse_qs(Req)),
    Params = #{~"status" => ~"published", ~"page" => maps:get(~"page", Query, ~"1")},
    case blog_posts:list_posts(Params) of
        {ok, #{entries := Posts, page := Page, total_pages := Pages}} ->
            WithAuthors = blog_repo:preload(blog_post, Posts, [author]),
            Vars = pager(Page, Pages, #{posts => [blog_view:post(P) || P <- WithAuthors]}),
            blog_view:render(Req, blog_post_index, Vars);
        {error, #kura_changeset{}} ->
            {status, 400}
    end.

pager(Page, Pages, Vars) ->
    Prev = [{prev_page, Page - 1} || Page > 1],
    Next = [{next_page, Page + 1} || Page < Pages],
    maps:merge(Vars, maps:from_list(Prev ++ Next)).
```

`status` is fixed to `published` whatever the query string says. `list_posts/1` from [CRUD & the Query Builder](../data-layer/crud.md) validates `page`, so `?page=abc` is a 400, not a crash. `pager/3` only adds `prev_page` and `next_page` when there is such a page, so the template can test for them.

`src/views/blog_post_index.dtl`:

```django
{% extends "blog_layout.dtl" %}
{% block content %}
<h1>Posts</h1>
{% for post in posts %}
  {% include "partials/post_card.dtl" %}
{% empty %}
  <p>No posts yet.</p>
{% endfor %}
<nav class="pager">
  {% if prev_page %}<a href="/posts?page={{ prev_page }}">Newer</a>{% endif %}
  {% if next_page %}<a href="/posts?page={{ next_page }}">Older</a>{% endif %}
</nav>
{% endblock %}
```

The page does not override `title`, so it gets the layout's default, "Posts". Each post is a card, `src/views/partials/post_card.dtl`:

```django
<article class="post-card">
  <h2><a href="/posts/{{ post.id }}">{{ post.title }}</a></h2>
  <p class="meta">by {{ post.author }} on {{ post.inserted_at|date:"j M Y" }}</p>
  <p>{{ post.body|truncatewords:30 }}</p>
</article>
```

The included card sees the loop variable `post`.

## The post page

`show/1` was covered in [Authorization](../auth-sessions/authorization.md#drafts): published posts for everyone, drafts for their author and admins, 404 for the rest. Its template, `src/views/blog_post_show.dtl`:

```django
{% extends "blog_layout.dtl" %}
{% block title %}{{ post.title }}{% endblock %}
{% block content %}
<article class="post">
  <h1>{{ post.title }}</h1>
  <p class="meta">
    by {{ post.author }} on {{ post.inserted_at|date:"j M Y" }}
    {% if post.status != "published" %}<span class="badge">{{ post.status }}</span>{% endif %}
  </p>
  {% for paragraph in post.paragraphs %}<p>{{ paragraph }}</p>{% endfor %}
</article>
{% if can_edit %}
<div class="actions">
  <a href="/posts/{{ post.id }}/edit">Edit</a>
  <form class="inline" method="post" action="/posts/{{ post.id }}/delete">
    <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
    <button type="submit" class="danger">Delete</button>
  </form>
</div>
{% endif %}
{% endblock %}
```

The title is escaped in both places it appears. The test posts a title that is a script tag and a body with an ampersand:

```erlang
post_page_escapes_and_formats(Config) ->
    #{id := Id} = blog_data_fixtures:post(?config(alice, Config), #{
        ~"title" => ~"<script>alert(1)</script>",
        ~"body" => ~"First paragraph.\n\nSecond & last.",
        ~"status" => ~"published"
    }),
    {ok, #{status := 200, body := Body}} = nova_test:get("/posts/" ++ integer_to_list(Id), Config),
    ?assertEqual(nomatch, binary:match(Body, ~"<script>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<h1>&lt;script&gt;alert(1)&lt;/script&gt;</h1>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<p>First paragraph.</p>")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<p>Second &amp; last.</p>")),
    assert_no_erlang_terms(Body).
```

`assert_no_erlang_terms/1` checks the page for `undefined`, `null` and `nil`, the usual signs of raw data reaching a template.

## Writing and editing posts

The new and edit pages share their fields in `src/views/partials/post_form.dtl`:

```django
<input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
<div class="field">
  <label for="title">Title</label>
  <input id="title" name="title" value="{{ form.title }}" required>
  {% include "partials/errors.dtl" with messages=errors.title %}
</div>
<div class="field">
  <label for="body">Body</label>
  <textarea id="body" name="body" rows="12" required>{{ form.body }}</textarea>
  {% include "partials/errors.dtl" with messages=errors.body %}
</div>
<div class="field">
  <label for="status">Status</label>
  <select id="status" name="status">
    <option value="draft"{% if form.status == "draft" %} selected{% endif %}>Draft</option>
    <option value="published"{% if form.status == "published" %} selected{% endif %}>Published</option>
  </select>
  {% include "partials/errors.dtl" with messages=errors.status %}
</div>
```

`src/views/blog_post_new.dtl` wraps it in a form:

```django
{% extends "blog_layout.dtl" %}
{% block title %}New post{% endblock %}
{% block content %}
<h1>New post</h1>
<form method="post" action="/posts">
  {% include "partials/post_form.dtl" %}
  <button type="submit">Create post</button>
</form>
{% endblock %}
```

and `src/views/blog_post_edit.dtl` adds the post's `lock_version`, so a save based on an old copy is caught by [optimistic locking](../data-layer/data-safety.md#optimistic-locking):

```django
{% extends "blog_layout.dtl" %}
{% block title %}Edit {{ post.title }}{% endblock %}
{% block content %}
<h1>Edit post</h1>
<form method="post" action="/posts/{{ post.id }}">
  <input type="hidden" name="lock_version" value="{{ post.lock_version }}">
  {% include "partials/post_form.dtl" %}
  <button type="submit">Save</button>
  <a href="/posts/{{ post.id }}">Cancel</a>
</form>
{% endblock %}
```

HTML forms only send `GET` and `POST`, so the update is `POST /posts/:id` and the delete is `POST /posts/:id/delete`. The controller actions:

```erlang
-define(FIELDS, [title, body, status]).

new(Req) ->
    Vars = #{form => blog_view:form(?FIELDS, #{status => draft}), errors => #{}},
    blog_view:render(Req, blog_post_new, Vars).

create(#{auth_data := Actor} = Req) ->
    Params = maps:get(params, Req, #{}),
    case blog_posts:create_post(Actor, Params) of
        {ok, #{id := Id}} ->
            ok = blog_flash:put(Req, info, ~"Post created."),
            {redirect, post_path(Id)};
        {error, #kura_changeset{} = CS} ->
            Vars = #{form => blog_view:form(?FIELDS, Params), errors => blog_view:errors(CS)},
            blog_view:render(Req, blog_post_new, Vars, 422)
    end.
```

```erlang
edit_form(Req, Post) ->
    Vars = #{post => blog_view:post(Post), form => blog_view:form(?FIELDS, Post), errors => #{}},
    blog_view:render(Req, blog_post_edit, Vars).

save(Req, #{id := Id} = Post) ->
    Params = maps:get(params, Req, #{}),
    case blog_posts:update_post(Post, Params) of
        {ok, _Updated} ->
            ok = blog_flash:put(Req, info, ~"Post updated."),
            {redirect, post_path(Id)};
        {error, stale} ->
            ok = blog_flash:put(
                Req, error, ~"Someone else changed this post. Reload and try again."
            ),
            {redirect, post_path(Id) ++ "/edit"};
        {error, #kura_changeset{} = CS} ->
            Vars = #{
                post => blog_view:post(Post),
                form => blog_view:form(?FIELDS, Params),
                errors => blog_view:errors(CS)
            },
            blog_view:render(Req, blog_post_edit, Vars, 422)
    end.

remove(Req, Post) ->
    {ok, _} = blog_posts:delete_post(Post),
    ok = blog_flash:put(Req, info, ~"Post deleted."),
    {redirect, "/posts"}.
```

`edit_form/2`, `save/2` and `remove/2` run inside `with_own_post/3` from [Authorization](../auth-sessions/authorization.md#resource-level-the-html-pages), so the post is known to be the user's. The pattern is the same everywhere:

- On success, set a flash and redirect (post, redirect, get), so reloading the next page does not submit the form again.
- On a validation error, render the form again with status 422, the submitted values and the errors.
- On a stale update, redirect to the edit form, which loads the current version, with a flash saying why.

`update_post/2` reads `lock_version` from the params, which is how the hidden field reaches the optimistic lock. A form sent back with errors shows the messages under each field:

```erlang
invalid_post_rerenders_form_with_errors(Config) ->
    Session = blog_data_fixtures:log_in(Config, ~"alice@example.com", ?PASSWORD),
    {ok, Form} = nova_test:get("/posts/new", Session),
    {ok, #{status := 422, body := Body}} = nova_test:post(
        "/posts",
        #{
            form => [
                {~"_csrf_token", nova_test:csrf_token(Form)},
                {~"title", ~"Hi"},
                {~"body", ~""},
                {~"status", ~"published"}
            ]
        },
        Session
    ),
    ?assertNotEqual(nomatch, binary:match(Body, ~"value=\"Hi\"")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"should be at least 3 character(s)")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"can&#039;t be blank")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"<option value=\"published\" selected>")),
    assert_no_erlang_terms(Body).
```

The apostrophe in "can't be blank" arrives as `&#039;`: the error message is escaped like any other value.

## Login and registration pages

The login and registration forms follow the same pattern. `src/views/blog_login.dtl`:

```django
{% extends "blog_layout.dtl" %}
{% block title %}Log in{% endblock %}
{% block content %}
<h1>Log in</h1>
{% if error %}<p class="flash flash-error" role="alert">{{ error }}</p>{% endif %}
<form method="post" action="/login">
  <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
  <div class="field">
    <label for="email">Email</label>
    <input id="email" name="email" type="email" value="{{ form.email }}" autocomplete="username" required>
  </div>
  <div class="field">
    <label for="password">Password</label>
    <input id="password" name="password" type="password" autocomplete="current-password" required>
  </div>
  <button type="submit">Log in</button>
</form>
<p>No account yet? <a href="/register">Register</a>.</p>
{% endblock %}
```

A failed login shows one message for the whole form, never which field was wrong. The password input has no `value`, so a password never travels back to the browser. The `autocomplete` attributes let password managers fill in the form. `src/views/blog_register.dtl` has a field per changeset field, each followed by its errors:

```django
<div class="field">
  <label for="email">Email</label>
  <input id="email" name="email" type="email" value="{{ form.email }}" autocomplete="email" required>
  {% include "partials/errors.dtl" with messages=errors.email %}
</div>
```

and the same for `username`, `full_name`, `password` and `password_confirmation`, with `autocomplete="new-password"` on the two password fields. The admin user list, `blog_admin_users.dtl`, is a plain table over `users`.

## The stylesheet

`priv/assets/css/blog.css` is about fifty lines: a system font stack, a narrow centred column, form fields, flash colours, the post cards. It is served by the static route already in the router:

```erlang
{"/assets/[...]", "assets"}
```

```admonish important title="Self-host your assets"
Do not link stylesheets, fonts or scripts from a CDN. Every visitor's browser would contact a third party, which tells it their IP address and what they read. Under the GDPR that is personal data you would have to justify sharing. A system font stack costs nothing to host. If you want a web font, put the files in `priv/assets/fonts` and serve them yourself.
```

The test makes sure the stylesheet is served and no page links outside the site:

```erlang
stylesheet_is_self_hosted(Config) ->
    {ok, Css} = nova_test:get("/assets/css/blog.css", Config),
    ?assertStatus(200, Css),
    ?assertHeader("content-type", "text/css", Css),
    ?assertNotEqual(nomatch, binary:match(nova_test:body(Css), ~".post-card")),
    {ok, #{status := 200, body := Page}} = nova_test:get("/posts", Config),
    ?assertEqual(nomatch, binary:match(Page, [~"https://", ~"http://"])).
```

## Trying it in a browser

Start the blog with `rebar3 nova serve` and open <http://localhost:8080/posts>:

1. Register; you land on the post list with "Welcome to the blog!" and your name in the header.
2. Click "New post", submit a two-letter title and an empty body, and see the errors under the fields with your title kept.
3. Fix them, choose "Published" and save. The post page shows "Post created." and the body split into paragraphs; reload and the flash is gone.
4. Log out, then open `/posts/new`: you are sent to the login page with "Please log in first."

In the browser's developer tools, the `session_id` cookie shows `HttpOnly`, `Secure` and `SameSite=Lax`, and the network tab shows no request leaving `localhost`. These steps were run in headless Chromium for this chapter; the CT suites cover the same ground on every build:

```shell
rebar3 ct --suite=test/blog_views_pages_SUITE,test/blog_views_templates_SUITE
```

---

The blog now has the pages a reader expects. Part V turns to the API side: [JSON APIs with Generators](../building-api/json-api.md).

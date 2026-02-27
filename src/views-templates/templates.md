# ErlyDTL Templates

Nova uses [ErlyDTL](https://github.com/erlydtl/erlydtl) for HTML templating — an Erlang implementation of [Django's template language](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html). Templates live in `src/views/` and are compiled to Erlang modules at build time.

## Template basics

ErlyDTL supports the same syntax as Django templates:

| Syntax | Purpose | Example |
|--------|---------|---------|
| `{{ var }}` | Output a variable | `{{ username }}` |
| `{% if cond %}...{% endif %}` | Conditional | `{% if error %}...{% endif %}` |
| `{% for x in list %}...{% endfor %}` | Loop | `{% for post in posts %}...{% endfor %}` |
| `{{ var\|filter }}` | Apply a filter | `{{ name\|upper }}` |
| `{{ var\|default:"n/a" }}` | Fallback value | `{{ bio\|default:"No bio" }}` |
| `{% extends "base.dtl" %}` | Inherit a layout | See below |
| `{% block name %}...{% endblock %}` | Override a block | See below |

See the [ErlyDTL documentation](https://github.com/erlydtl/erlydtl) for the full list of tags and filters.

## Creating a base layout

Most pages share the same outer HTML. Template inheritance lets you define a base layout once and override specific blocks in child templates.

Create `src/views/base.dtl`:

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>{% block title %}Blog{% endblock %}</title>
</head>
<body>
  <nav>
    {% if username %}
      <span>{{ username }}</span> | <a href="/logout">Logout</a>
    {% else %}
      <a href="/login">Login</a>
    {% endif %}
  </nav>
  <main>
    {% block content %}{% endblock %}
  </main>
</body>
</html>
```

Child templates use `{% extends "base.dtl" %}` and fill in the blocks they need. Anything outside a `{% block %}` tag in the child is ignored.

## Creating a login template

Create `src/views/login.dtl`:

```html
{% extends "base.dtl" %}

{% block title %}Login{% endblock %}

{% block content %}
<div>
  {% if error %}<p style="color:red">{{ error }}</p>{% endif %}
  <form action="/login" method="post">
    <input type="hidden" name="_csrf_token" value="{{ csrf_token }}" />
    <label for="username">Username:</label>
    <input type="text" id="username" name="username"><br>
    <label for="password">Password:</label>
    <input type="password" id="password" name="password"><br>
    <input type="submit" value="Submit">
  </form>
</div>
{% endblock %}
```

This form POSTs to `/login` with `username` and `password` fields. The URL-encoded body will be decoded by `nova_request_plugin` (which we configured in the [Plugins](../foundations/plugins.md) chapter).

The hidden `_csrf_token` field is required because we enabled `nova_csrf_plugin`. Nova automatically injects the `csrf_token` variable into every template — you just need to include it in the form. Without it, the POST request would be rejected with a 403 error.

## Adding a controller function

Our generated controller is in `src/controllers/blog_main_controller.erl`:

```erlang
-module(blog_main_controller).
-export([
         index/1,
         login/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

login(_Req) ->
    {ok, [], #{view => login}}.
```

The return tuple `{ok, [], #{view => login}}` tells Nova:
- `ok` — render a template
- `[]` — no template variables
- `#{view => login}` — use the `login` template (matches `login.dtl`)

## How template resolution works

When a controller returns `{ok, Variables}` (without a `view` option), Nova looks for a template named after the controller module. For `blog_main_controller:index/1`, it looks for `blog_main.dtl`.

When you specify `#{view => login}`, Nova uses `login.dtl` instead.

## Template options

The full return tuple is `{ok, Variables, Options}` where `Options` is a map that supports three keys:

| Option | Default | Description |
|--------|---------|-------------|
| `view` | derived from module name | Which template to render |
| `headers` | `#{<<"content-type">> => <<"text/html">>}` | Response headers |
| `status_code` | `200` | HTTP status code |

Some examples:

```erlang
%% Render login.dtl with default 200 status
{ok, [], #{view => login}}.

%% Render with a 422 status (useful for form validation errors)
{ok, [{error, <<"Invalid input">>}], #{view => login, status_code => 422}}.

%% Return plain text instead of HTML
{ok, [{data, Body}], #{headers => #{<<"content-type">> => <<"text/plain">>}}}.
```

```admonish tip
`{view, Variables}` and `{view, Variables, Options}` are aliases for `{ok, ...}` — they behave identically.
```

---

With templates in place, let's build complete pages in the next chapter: [Building Pages](building-pages.md).

# ErlyDTL Templates

Nova renders HTML with [ErlyDTL](https://github.com/erlydtl/erlydtl), an Erlang implementation of the Django template language. Every `.dtl` file is compiled to an Erlang module at build time, so a template is as fast as any other code and a syntax error stops the build instead of a request. This chapter covers how Nova finds and renders templates, the template language, and the traps worth knowing before [Building Pages](building-pages.md) uses it for the blog.

## How templates are compiled

The scaffold's `rebar.config` sets up the `rebar3_erlydtl_plugin` and runs it before the Erlang compiler:

```erlang
{erlydtl_opts, [
    {doc_root, "src/views"},
    {recursive, true},
    {libraries, [
        {nova_erlydtl_inventory, nova_erlydtl_inventory}
    ]},
    {default_libraries, [nova_erlydtl_inventory]}
]}.
```

```erlang
{provider_hooks, [
    {pre, [
        {compile, {erlydtl, compile}},
        ...
    ]}
]}.
```

Every `.dtl` file under `src/views`, in subdirectories too, becomes a module named after the file with `_dtl` appended: `src/views/blog_login.dtl` becomes `blog_login_dtl`. The directory is not part of the name, so `src/views/partials/flash.dtl` becomes `flash_dtl`, and two files with the same name in different directories collide. Prefix your template names or keep them unique.

A compiled template is an ordinary module. `render/1` takes the variables and returns `{ok, IoData}`, and `variables/0` lists what the template reads:

```erlang
1> post_card_dtl:variables().
[post]
```

That makes templates easy to test without HTTP, as the end of this chapter shows.

## Rendering from a controller

A controller renders a template by returning `ok` (or its alias `view`) and the variables:

| Return | Renders |
|--------|---------|
| `{ok, Vars}` | The controller's default template |
| `{ok, Vars, Options}` | As `Options` says |
| `{ok, Vars, Options, Req}` | The same, with a modified request (cookies, headers) |

The default template is the controller module with `_controller` replaced by `_dtl`: `blog_main_controller` renders `blog_main_dtl`, from `blog_main.dtl`. Options:

| Option | Default | Meaning |
|--------|---------|---------|
| `view` | the default template | Template name as an atom or string, without `_dtl`: `#{view => blog_login}` |
| `status_code` | `200` | The response status |
| `headers` | `#{~"content-type" => ~"text/html"}` | Response headers. A map here replaces the default, so include `content-type` yourself |

`Vars` can be a map or a proplist. If the template module does not exist, Nova logs an error and answers 404, so a typo in `view` looks like a missing page; check the log.

### The CSRF token

When the route runs `nova_csrf_plugin` (the blog's `html_plugins()` do, see [Plugins](../foundations/plugins.md)), the plugin puts the session's token in the request, and Nova adds it to the variables as `csrf_token`. Every form that posts to such a route needs it:

```html
<input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
```

On a route without the plugin there is no `csrf_token`, the field renders empty, and the next `POST` fails. If a form gets "Forbidden - CSRF token invalid", check that the `GET` route that rendered it runs the plugin too.

## The template language

### Variables

`{{ name }}` prints a variable. A dot reads a map key, a proplist key or a list element: `{{ post.title }}`, `{{ current_user.username }}`. Variables can be atoms, binaries, strings, integers and dates.

### Tags

```django
{% if current_user %}
  <span class="who">{{ current_user.username }}</span>
{% else %}
  <a href="/login">Log in</a>
{% endif %}

{% if post.status != "published" %}<span class="badge">{{ post.status }}</span>{% endif %}

{% for post in posts %}
  {% include "partials/post_card.dtl" %}
{% empty %}
  <p>No posts yet.</p>
{% endfor %}
```

- `{% if %}` is false for `undefined`, `false`, `0`, an empty list and an empty binary. `==` and `!=` compare a binary with a string literal as you would hope: `{% if status == "draft" %}` works when `status` is `~"draft"`.
- `{% for %}` loops over a list; `{% empty %}` renders when the list is empty. Inside the loop, `forloop.counter`, `forloop.first` and `forloop.last` are available.
- `{% include "file.dtl" %}` inserts another template, which sees the same variables. `{% include "partials/errors.dtl" with messages=errors.title %}` adds or renames variables for the included file. Paths are relative to `doc_root`.
- `{% extends "blog_layout.dtl" %}` and `{% block name %}...{% endblock %}` build pages from a layout: the layout defines blocks with default content, the page overrides them. Put `extends` at the very top of the page template.

### Filters

Filters transform a value with `|`:

| Filter | Example | Output |
|--------|---------|--------|
| `date` | `{{ at\|date:"j M Y" }}` with `{{2026, 11, 3}, {8, 5, 0}}` | `3 Nov 2026` |
| `truncatewords` | `{{ text\|truncatewords:2 }}` with `~"Hello world again"` | `Hello world ...` |
| `join` | `{{ list\|join:", " }}` with `[~"a", ~"b"]` | `a, b` |
| `default` | `{{ name\|default:"none" }}` | `none` when `name` is undefined, `null` or empty |
| `safe` | `{{ html\|safe }}` | `html` without escaping |

Kura returns `utc_datetime` fields as `{{Y, M, D}, {H, Mi, S}}` tuples, which is what `date` expects.

### The url tag

`nova_erlydtl_inventory` adds one tag, `url`, which prefixes a path with the prefix the application is mounted under:

```django
<a href="{% url "/posts" application="blog" %}">Posts</a>
```

The blog is mounted at the root, so this renders `<a href="/posts">Posts</a>`. The tag earns its keep when an application is included in another under a prefix; the blog's templates use plain paths.

## Escaping

Autoescaping is on. Every `{{ }}` output is HTML-escaped, so a post titled `<script>alert(1)</script>` is shown as text:

```erlang
~"<p>&lt;b&gt;Tom &amp; &quot;Jerry&quot;&lt;/b&gt;</p>" =
    render(~"<p>{{ name }}</p>", #{name => ~"<b>Tom & \"Jerry\"</b>"}),
~"<p><b>trusted</b></p>" = render(~"<p>{{ html|safe }}</p>", #{html => ~"<b>trusted</b>"}).
```

Use `safe` only on HTML your own code produced. Never on user input.

```admonish warning title="linebreaks and safe"
The `linebreaks` filter, which turns blank lines into `<p>` elements, is broken with autoescaping: its output is escaped again and the reader sees `&lt;p&gt;`. The tempting fix, `{{ body|escape|linebreaks|safe }}`, is worse: `escape` does nothing under autoescaping, so `safe` lets the user's HTML through unescaped. Split text into paragraphs in Erlang and loop over them instead; [Building Pages](building-pages.md#the-post-page) does.
```

## Missing values

What a missing or empty value prints depends on what it is:

```erlang
~"[][][null][nil][published]" =
    render(~"[{{ missing }}][{{ u }}][{{ n }}][{{ nil }}][{{ a }}]", #{
        u => undefined, n => null, nil => nil, a => published
    }),
```

A missing key and `undefined` print nothing. `null` (what OTP's `json` decodes JSON `null` to) and `nil` print as words. In an attribute, `value="null"` is a real value the browser submits back. The blog never hands raw data to a form; [Building Pages](building-pages.md#forms) converts every field to a binary first.

```admonish warning title="Looping over an undefined variable"
A `for` over a top-level variable that is `undefined` renders nothing. The same loop over a variable bound with `with` crashes the render with a `function_clause` in `erlydtl_runtime:to_list/2`:

    {% include "partials/errors.dtl" with messages=errors.title %}

crashes on a form without a title error if `errors.dtl` loops over `messages` directly. Guard the loop:

    {% if messages %}{% for message in messages %}...{% endfor %}{% endif %}
```

The template suite pins this behaviour down, so an ErlyDTL upgrade that changes it shows up as a failing test:

```erlang
looping_over_undefined_crashes(_Config) ->
    ~"" = render(~"{% for x in items %}{{ x }}{% endfor %}", #{items => undefined}),
    ~"" = render(~"{% for x in errors.title %}{{ x }}{% endfor %}", #{errors => #{}}),
    Bound =
        ~"{% with messages=errors.title %}{% for x in messages %}{{ x }}{% endfor %}{% endwith %}",
    ?assertError(function_clause, render(Bound, #{errors => #{}})),
    Guarded =
        ~"{% with messages=errors.title %}{% if messages %}{% for x in messages %}{{ x }}{% endfor %}{% endif %}{% endwith %}",
    ~"" = render(Guarded, #{errors => #{}}),
    ~"none" = render(~"{% for x in items %}{{ x }}{% empty %}none{% endfor %}", #{items => []}).
```

## Status codes and the status page

`{status, 404}` from a controller sends a 404 with an empty body; it does not render the 404 page from your router. To show your page, call its controller yourself, as the blog does for unknown posts:

```erlang
{error, not_found} ->
    blog_error_controller:not_found(Req)
```

## Testing templates

Templates compiled from strings are handy for trying out the language. `erlydtl:compile_template/3` with `{out_dir, false}` compiles in memory:

```erlang
render(Source, Vars) ->
    {ok, Module} = erlydtl:compile_template(Source, views_reference_dtl, [{out_dir, false}]),
    html(Module:render(Vars)).

html({ok, IoData}) ->
    iolist_to_binary(IoData).
```

The blog's own templates are tested by calling their modules with the variables a controller would pass:

```erlang
layout_adapts_to_the_user(_Config) ->
    Base = #{posts => [], flash => [], csrf_token => ~"t"},
    Anonymous = html(blog_post_index_dtl:render(Base#{current_user => false})),
    ?assertNotEqual(nomatch, binary:match(Anonymous, ~"href=\"/register\"")),
    Author = html(
        blog_post_index_dtl:render(Base#{current_user => #{username => ~"alice", admin => false}})
    ),
    ?assertNotEqual(nomatch, binary:match(Author, ~"action=\"/logout\"")),
    ?assertEqual(nomatch, binary:match(Author, ~"/admin/users")),
    Admin = html(
        blog_post_index_dtl:render(Base#{current_user => #{username => ~"carol", admin => true}})
    ),
    ?assertNotEqual(nomatch, binary:match(Admin, ~"href=\"/admin/users\"")),
    ?assertNotEqual(nomatch, binary:match(Anonymous, ~"No posts yet.")).
```

Run the template suite:

```shell
rebar3 ct --suite=test/blog_views_templates_SUITE
```

---

Next we use all of this for the blog's pages: [Building Pages](building-pages.md).

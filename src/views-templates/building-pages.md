# Building Pages

In the previous chapter we learned ErlyDTL template syntax. Now let's build complete pages — layouts with navigation, forms with error handling, and reusable partials.

## A proper base layout

Expand the base layout with navigation, flash messages, and a footer:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{% block title %}Blog{% endblock %} — Nova Blog</title>
  <link rel="stylesheet" href="/assets/css/style.css">
  {% block head %}{% endblock %}
</head>
<body>
  <header>
    <nav>
      <a href="/">Home</a>
      {% if auth_data %}
        <a href="/posts/new">New Post</a>
        <span>{{ auth_data.username }}</span>
        <a href="/logout">Logout</a>
      {% else %}
        <a href="/login">Login</a>
        <a href="/register">Register</a>
      {% endif %}
    </nav>
  </header>

  <main>
    {% if flash_info %}
      <div class="flash flash-info">{{ flash_info }}</div>
    {% endif %}
    {% if flash_error %}
      <div class="flash flash-error">{{ flash_error }}</div>
    {% endif %}

    {% block content %}{% endblock %}
  </main>

  <footer>
    <p>Built with Nova</p>
  </footer>
</body>
</html>
```

## Forms with validation errors

A post creation form that displays changeset errors:

```html
{% extends "base.dtl" %}
{% block title %}New Post{% endblock %}

{% block content %}
<h1>New Post</h1>
<form action="/posts" method="post">
  <input type="hidden" name="_csrf_token" value="{{ csrf_token }}" />

  <div class="field">
    <label for="title">Title</label>
    <input type="text" id="title" name="title" value="{{ form_title|default:"" }}">
    {% if errors.title %}
      <span class="error">{{ errors.title }}</span>
    {% endif %}
  </div>

  <div class="field">
    <label for="body">Body</label>
    <textarea id="body" name="body">{{ form_body|default:"" }}</textarea>
    {% if errors.body %}
      <span class="error">{{ errors.body }}</span>
    {% endif %}
  </div>

  <div class="field">
    <label for="status">Status</label>
    <select id="status" name="status">
      <option value="draft" {% if form_status == "draft" %}selected{% endif %}>Draft</option>
      <option value="published" {% if form_status == "published" %}selected{% endif %}>Published</option>
    </select>
  </div>

  <button type="submit">Create Post</button>
</form>
{% endblock %}
```

The controller re-renders the form with errors and the submitted values:

```erlang
create(#{params := Params, auth_data := #{id := UserId}} = _Req) ->
    Params1 = Params#{<<"user_id">> => UserId},
    CS = post:changeset(#{}, Params1),
    case blog_repo:insert(CS) of
        {ok, Post} ->
            {redirect, "/posts/" ++ integer_to_list(maps:get(id, Post))};
        {error, #kura_changeset{} = CS1} ->
            Errors = changeset_errors_to_json(CS1),
            {ok, [{errors, Errors},
                  {form_title, maps:get(<<"title">>, Params, <<>>)},
                  {form_body, maps:get(<<"body">>, Params, <<>>)},
                  {form_status, maps:get(<<"status">>, Params, <<"draft">>)}],
             #{view => new_post, status_code => 422}}
    end.
```

## Template includes (partials)

Extract reusable fragments with `{% include %}`:

`src/views/_post_card.dtl`:
```html
<article class="post-card">
  <h2><a href="/posts/{{ post.id }}">{{ post.title }}</a></h2>
  <p>by {{ post.author.username }} — {{ post.inserted_at }}</p>
  {% if post.tags %}
    <div class="tags">
      {% for tag in post.tags %}
        <span class="tag">{{ tag.name }}</span>
      {% endfor %}
    </div>
  {% endif %}
</article>
```

Use it in a listing page:
```html
{% extends "base.dtl" %}
{% block content %}
<h1>Posts</h1>
{% for post in posts %}
  {% include "_post_card.dtl" %}
{% endfor %}
{% endblock %}
```

## Flash messages

Flash messages show one-time notifications (e.g. "Post created successfully"). Store them in the session and clear after display.

A controller sets a flash before redirecting:

```erlang
create(#{params := Params} = Req) ->
    case blog_repo:insert(post:changeset(#{}, Params)) of
        {ok, Post} ->
            set_flash(Req, flash_info, <<"Post created!">>),
            {redirect, "/posts/" ++ integer_to_list(maps:get(id, Post))};
        {error, CS} ->
            %% re-render with errors (no flash needed)
            ...
    end.
```

The helpers that store and retrieve flash values from the session:

```erlang
%% Setting a flash message
set_flash(Req, Key, Message) ->
    nova_session:set(Req, Key, Message).

%% Reading and clearing flash messages
get_flash(Req, Key) ->
    case nova_session:get(Req, Key) of
        {ok, Message} ->
            nova_session:delete(Req, Key),
            Message;
        {error, _} ->
            undefined
    end.
```

The destination controller reads the flash and passes it to the template. The base layout (shown at the top of this chapter) renders `flash_info` and `flash_error` if present.

---

We now have a complete HTML frontend. Next, let's build [JSON APIs](../building-api/json-api.md) with code generators.

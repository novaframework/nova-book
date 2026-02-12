# Views

Nova uses [ErlyDTL](https://github.com/erlydtl/erlydtl) for HTML templating — an Erlang implementation of [Django's template language](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html). Templates live in `src/views/` and are compiled to Erlang modules at build time.

## Creating a login template

Let's build a login page. Create `src/views/login.dtl`:

```html
<html>
<body>
  <div>
    <form action="/login" method="post">
      <label for="username">Username:</label>
      <input type="text" id="username" name="username"><br>
      <label for="password">Password:</label>
      <input type="password" id="password" name="password"><br>
      <input type="submit" value="Submit">
    </form>
  </div>
</body>
</html>
```

This form POSTs to `/login` with `username` and `password` fields. The URL-encoded body will be decoded by `nova_request_plugin` (which we configured in the [Plugins](plugins.md) chapter).

## Adding a controller function

Our generated controller is in `src/controllers/my_first_nova_main_controller.erl`:

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.
```

The `index/1` function returns `{ok, Variables}` — this tells Nova to render the default template for this controller with the given variables. The variable `message` is available as `{{ message }}` in the template.

Let's add a `login/1` function:

```erlang
-module(my_first_nova_main_controller).
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

When a controller returns `{ok, Variables}` (without a `view` option), Nova looks for a template named after the controller module. For `my_first_nova_main_controller:index/1`, it looks for `my_first_nova_main.dtl`.

When you specify `#{view => login}`, Nova uses `login.dtl` instead.

## Viewing the page

Make sure the `/login` route exists in your router (we added it in the [Routing](routing.md) chapter):

```erlang
{"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}}
```

Start the server with `rebar3 nova serve` and visit `http://localhost:8080/login`. You should see the login form.

---

The form submits data but nothing handles it yet. In the next chapter we will add [authentication](authentication.md) to process the login.

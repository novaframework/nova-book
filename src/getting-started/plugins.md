# Plugins

Plugins are Nova's middleware system. They run code before and after your controller handles a request — useful for decoding request bodies, adding headers, logging, rate limiting, and more.

## How the plugin pipeline works

Every HTTP request flows through a pipeline:

1. **Pre-request plugins** run in order (lowest priority number first)
2. The **controller** handles the request
3. **Post-request plugins** run in order

A plugin module implements the `nova_plugin` behaviour and exports `pre_request/2`, `post_request/2`, and `plugin_info/0`.

Here is an example — the `nova_correlation_plugin` that ships with Nova:

```erlang
-module(nova_correlation_plugin).
-behaviour(nova_plugin).

-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

pre_request(Req0, Opts) ->
    CorrId = get_correlation_id(Req0, Opts),
    ok = update_logger_metadata(CorrId, Opts),
    Req1 = cowboy_req:set_resp_header(<<"X-Correlation-ID">>, CorrId, Req0),
    Req = Req1#{correlation_id => CorrId},
    {ok, Req}.

post_request(Req, _) ->
    {ok, Req}.

plugin_info() ->
   {<<"nova_correlation_plugin">>,
    <<"0.2.0">>,
    <<"Nova team <info@novaframework.org">>,
    <<"Add X-Correlation-ID headers to response">>,
    []}.
```

The `pre_request` callback picks up or generates a correlation ID and adds it to both the response headers and the request map. `post_request` is a no-op here.

## Configuring plugins

Plugins are configured in `sys.config` under the `nova` application key:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]}
]}
```

Each plugin entry is a tuple: `{Phase, Module, Options}` where Phase is `pre_request` or `post_request`.

`nova_request_plugin` is a built-in plugin that handles request body decoding. The options map controls what it decodes.

## Setting up for our login form

In the next chapters we will build a login form that sends URL-encoded data. To have Nova decode this automatically, update the plugin config in `dev_sys.config.src`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
]}
```

With this setting, form POST data is decoded and placed in the `params` key of the request map, ready for your controller to use.

```admonish tip
You can enable multiple decoders at once. We will add `decode_json_body => true` later when we build our [JSON API](../building-apis/json-apis.md).
```

## Built-in plugins

Nova ships with several plugins. See the [Nova documentation](https://hexdocs.pm/nova/plugins.html) for the full list.

For now, the key one is `nova_request_plugin` — it handles JSON body decoding, URL-encoded body decoding, and multipart uploads.

---

With plugins configured to decode form data, we can now build our first [view](views.md) — a login page.

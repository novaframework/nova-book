-module(blog_views_templates_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    variables_are_escaped/1,
    missing_values_render_as/1,
    looping_over_undefined_crashes/1,
    filters_format_values/1,
    linebreaks_is_escaped_twice/1,
    layout_adapts_to_the_user/1,
    flash_partial_renders_each_message/1,
    status_tuple_skips_the_status_page/1,
    every_template_is_a_module/1,
    url_tag_prefixes_the_app/1
]).

all() ->
    [
        variables_are_escaped,
        missing_values_render_as,
        looping_over_undefined_crashes,
        filters_format_values,
        linebreaks_is_escaped_twice,
        layout_adapts_to_the_user,
        flash_partial_renders_each_message,
        status_tuple_skips_the_status_page,
        every_template_is_a_module,
        url_tag_prefixes_the_app
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    ok = nova_router:add_routes(blog, #{
        prefix => "/views-reference",
        security => false,
        routes => [{"/gone", fun blog_views_reference_controller:gone/1, #{methods => [get]}}]
    }),
    Config.

end_per_suite(_Config) ->
    ok.

variables_are_escaped(_Config) ->
    ~"<p>&lt;b&gt;Tom &amp; &quot;Jerry&quot;&lt;/b&gt;</p>" =
        render(~"<p>{{ name }}</p>", #{name => ~"<b>Tom & \"Jerry\"</b>"}),
    ~"<p><b>trusted</b></p>" = render(~"<p>{{ html|safe }}</p>", #{html => ~"<b>trusted</b>"}).

missing_values_render_as(_Config) ->
    ~"[][][null][nil][published]" =
        render(~"[{{ missing }}][{{ u }}][{{ n }}][{{ nil }}][{{ a }}]", #{
            u => undefined, n => null, nil => nil, a => published
        }),
    ~"[none][none][none]" =
        render(
            ~"[{{ u|default:\"none\" }}][{{ n|default:\"none\" }}][{{ e|default:\"none\" }}]", #{
                u => undefined, n => null, e => ~""
            }
        ).

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

filters_format_values(_Config) ->
    ~"3 Nov 2026" = render(~"{{ at|date:\"j M Y\" }}", #{at => {{2026, 11, 3}, {8, 5, 0}}}),
    ~"Hello world ..." = render(~"{{ text|truncatewords:2 }}", #{text => ~"Hello world again"}),
    ~"a, b" = render(~"{{ list|join:\", \" }}", #{list => [~"a", ~"b"]}),
    ~"selected" =
        render(~"{% if status == \"draft\" %}selected{% endif %}", #{status => ~"draft"}).

linebreaks_is_escaped_twice(_Config) ->
    ~"&lt;p&gt;one&lt;/p&gt;&lt;p&gt;two&lt;/p&gt;" =
        render(~"{{ text|linebreaks }}", #{text => ~"one\n\ntwo"}),
    ~"<p><b>one</b></p>" = render(~"{{ text|escape|linebreaks|safe }}", #{text => ~"<b>one</b>"}).

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

flash_partial_renders_each_message(_Config) ->
    Html = html(
        flash_dtl:render(#{
            flash => [#{kind => ~"info", text => ~"Saved."}, #{kind => ~"error", text => ~"<oops>"}]
        })
    ),
    ?assertNotEqual(
        nomatch, binary:match(Html, ~"<p class=\"flash flash-info\" role=\"status\">Saved.</p>")
    ),
    ?assertNotEqual(nomatch, binary:match(Html, ~"&lt;oops&gt;")).

status_tuple_skips_the_status_page(_Config) ->
    {ok, {{_, 404, _}, _, Body}} = httpc:request(
        get,
        {blog_data_fixtures:url("/views-reference/gone"), [{"accept", "text/html"}]},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(<<>>, Body).

every_template_is_a_module(_Config) ->
    {module, blog_layout_dtl} = code:ensure_loaded(blog_layout_dtl),
    {module, post_card_dtl} = code:ensure_loaded(post_card_dtl),
    [post] = post_card_dtl:variables().

url_tag_prefixes_the_app(_Config) ->
    Options = [
        {out_dir, false},
        {libraries, [{nova_erlydtl_inventory, nova_erlydtl_inventory}]},
        {default_libraries, [nova_erlydtl_inventory]}
    ],
    {ok, Module} = erlydtl:compile_template(
        ~"<a href=\"{% url \"/posts\" application=\"blog\" %}\">Posts</a>",
        url_reference_dtl,
        Options
    ),
    ~"<a href=\"/posts\">Posts</a>" = html(Module:render(#{})).

render(Source, Vars) ->
    {ok, Module} = erlydtl:compile_template(Source, views_reference_dtl, [{out_dir, false}]),
    html(Module:render(Vars)).

html({ok, IoData}) ->
    iolist_to_binary(IoData).

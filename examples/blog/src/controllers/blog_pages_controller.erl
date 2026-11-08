-module(blog_pages_controller).

-export([
    about/1,
    about_moved/1,
    about_markdown/1,
    version/1
]).

about(_Req) ->
    {ok, [{title, ~"About this blog"}], #{view => blog_about}}.

about_moved(_Req) ->
    {redirect, "/about"}.

about_markdown(_Req) ->
    Path = filename:join(code:priv_dir(blog), "pages/about.md"),
    {sendfile, 200, #{}, {0, filelib:file_size(Path), Path}, ~"text/markdown; charset=utf-8"}.

version(_Req) ->
    {ok, Vsn} = application:get_key(blog, vsn),
    {text, ["blog ", Vsn, "\n"]}.

-module(blog_api_tags_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_list/1, test_show/1, test_create/1, test_update/1, test_delete/1]).
-export([
    list_is_sorted_by_name/1,
    show_includes_tagged_posts/1,
    show_hides_deleted_posts/1,
    show_unknown_tag_is_404/1,
    show_non_numeric_id_is_404/1,
    create_sets_location/1,
    create_duplicate_name_is_422/1,
    create_invalid_name_is_422/1,
    create_without_name_is_400/1,
    create_with_unknown_field_is_400/1,
    create_with_wrong_type_is_400/1,
    create_non_object_is_400/1,
    create_without_json_is_415/1,
    create_malformed_json_is_400/1,
    update_unknown_tag_is_404/1,
    update_invalid_name_is_422/1,
    delete_unknown_tag_is_404/1,
    delete_untags_posts/1,
    patch_is_405/1,
    writes_need_a_bearer_token/1,
    writes_need_the_admin_role/1,
    openapi_spec_lists_api_routes/1,
    generated_draft07_schema_is_rejected/1
]).

all() ->
    [
        test_list,
        test_show,
        test_create,
        test_update,
        test_delete,
        list_is_sorted_by_name,
        show_includes_tagged_posts,
        show_hides_deleted_posts,
        show_unknown_tag_is_404,
        show_non_numeric_id_is_404,
        create_sets_location,
        create_duplicate_name_is_422,
        create_invalid_name_is_422,
        create_without_name_is_400,
        create_with_unknown_field_is_400,
        create_with_wrong_type_is_400,
        create_non_object_is_400,
        create_without_json_is_415,
        create_malformed_json_is_400,
        update_unknown_tag_is_404,
        update_invalid_name_is_422,
        delete_unknown_tag_is_404,
        delete_untags_posts,
        patch_is_405,
        writes_need_a_bearer_token,
        writes_need_the_admin_role,
        openapi_spec_lists_api_routes,
        generated_draft07_schema_is_rejected
    ].

init_per_suite(Config) ->
    blog_data_fixtures:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(blog).

init_per_testcase(_Case, Config) ->
    blog_data_fixtures:checkout(),
    {ok, Tag} = blog_tags:create_tag(#{~"name" => ~"erlang"}),
    Token = blog_data_fixtures:token(blog_data_fixtures:admin(~"carol")),
    [{tag_path, "/api/tags/" ++ integer_to_list(maps:get(id, Tag))}, {token, Token} | Config].

end_per_testcase(_Case, _Config) ->
    blog_data_fixtures:checkin().

test_list(_Config) ->
    {200, #{~"tags" := [#{~"name" := ~"erlang"}]}} = blog_data_fixtures:request(get, "/api/tags").

test_show(Config) ->
    {200, #{~"name" := ~"erlang", ~"posts" := []}} =
        blog_data_fixtures:request(get, ?config(tag_path, Config)).

test_create(Config) ->
    {201, #{~"name" := ~"otp", ~"id" := Id}} =
        blog_data_fixtures:request_as(?config(token, Config), post, "/api/tags", #{name => ~"otp"}),
    true = is_integer(Id).

test_update(Config) ->
    {200, #{~"name" := ~"beam"}} =
        blog_data_fixtures:request_as(?config(token, Config), put, ?config(tag_path, Config), #{
            name => ~"beam"
        }).

test_delete(Config) ->
    {204, <<>>} = blog_data_fixtures:request_as(
        ?config(token, Config), delete, ?config(tag_path, Config)
    ),
    {404, _} = blog_data_fixtures:request(get, ?config(tag_path, Config)).

list_is_sorted_by_name(Config) ->
    {201, _} = blog_data_fixtures:request_as(?config(token, Config), post, "/api/tags", #{
        name => ~"nova"
    }),
    {201, _} = blog_data_fixtures:request_as(?config(token, Config), post, "/api/tags", #{
        name => ~"beam"
    }),
    {200, #{~"tags" := Tags}} = blog_data_fixtures:request(get, "/api/tags"),
    [~"beam", ~"erlang", ~"nova"] = [Name || #{~"name" := Name} <- Tags].

show_includes_tagged_posts(Config) ->
    Alice = blog_data_fixtures:user(~"alice"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Tagged post"}),
    {ok, _} = blog_posts:tag_post(Post, [~"erlang"]),
    {200, #{~"posts" := [#{~"title" := ~"Tagged post"}]}} =
        blog_data_fixtures:request(get, ?config(tag_path, Config)).

show_hides_deleted_posts(Config) ->
    Alice = blog_data_fixtures:user(~"alice"),
    Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Deleted post"}),
    {ok, Tagged} = blog_posts:tag_post(Post, [~"erlang"]),
    {ok, _} = blog_posts:delete_post(Tagged),
    {200, #{~"posts" := Posts}} = blog_data_fixtures:request(get, ?config(tag_path, Config)),
    [] = Posts.

show_unknown_tag_is_404(_Config) ->
    {404, #{~"error" := ~"not_found"}} = blog_data_fixtures:request(get, "/api/tags/999999").

show_non_numeric_id_is_404(_Config) ->
    {404, #{~"error" := ~"not_found"}} = blog_data_fixtures:request(get, "/api/tags/abc").

create_sets_location(Config) ->
    {201, Headers, Body} = raw(
        ?config(token, Config), post, "/api/tags", "application/json", ~"{\"name\":\"otp\"}"
    ),
    #{~"id" := Id} = json:decode(Body),
    Location = "/api/tags/" ++ integer_to_list(Id),
    Location = proplists:get_value("location", Headers).

create_duplicate_name_is_422(Config) ->
    {422, #{~"errors" := #{~"name" := [~"has already been taken"]}}} =
        blog_data_fixtures:request_as(?config(token, Config), post, "/api/tags", #{
            name => ~"erlang"
        }).

create_invalid_name_is_422(Config) ->
    {422, #{~"errors" := #{~"name" := [~"has invalid format"]}}} =
        blog_data_fixtures:request_as(?config(token, Config), post, "/api/tags", #{
            name => ~"Not Valid!"
        }).

create_without_name_is_400(Config) ->
    {400, _, <<>>} = raw(?config(token, Config), post, "/api/tags", "application/json", ~"{}").

create_with_unknown_field_is_400(Config) ->
    {400, _, <<>>} = raw(
        ?config(token, Config),
        post,
        "/api/tags",
        "application/json",
        ~"{\"name\":\"otp\",\"id\":7}"
    ).

create_with_wrong_type_is_400(Config) ->
    {400, _, <<>>} = raw(
        ?config(token, Config), post, "/api/tags", "application/json", ~"{\"name\":42}"
    ).

create_non_object_is_400(Config) ->
    {400, _, <<>>} = raw(
        ?config(token, Config), post, "/api/tags", "application/json", ~"[\"otp\"]"
    ).

create_without_json_is_415(Config) ->
    {415, _, Body} = raw(?config(token, Config), post, "/api/tags", "text/plain", ~"otp"),
    #{~"error" := ~"expected a JSON body"} = json:decode(Body).

create_malformed_json_is_400(Config) ->
    {400, _, <<>>} = raw(
        ?config(token, Config), post, "/api/tags", "application/json", ~"{\"name\":"
    ).

update_unknown_tag_is_404(Config) ->
    {404, #{~"error" := ~"not_found"}} =
        blog_data_fixtures:request_as(?config(token, Config), put, "/api/tags/999999", #{
            name => ~"otp"
        }).

update_invalid_name_is_422(Config) ->
    {422, #{~"errors" := #{~"name" := [~"should be at most 30 character(s)"]}}} =
        blog_data_fixtures:request_as(?config(token, Config), put, ?config(tag_path, Config), #{
            name => binary:copy(~"a", 31)
        }).

delete_unknown_tag_is_404(Config) ->
    {404, #{~"error" := ~"not_found"}} = blog_data_fixtures:request_as(
        ?config(token, Config), delete, "/api/tags/999999"
    ).

delete_untags_posts(Config) ->
    Alice = blog_data_fixtures:user(~"alice"),
    #{id := PostId} = Post = blog_data_fixtures:post(Alice, #{~"title" => ~"Tagged post"}),
    {ok, _} = blog_posts:tag_post(Post, [~"erlang"]),
    {204, <<>>} = blog_data_fixtures:request_as(
        ?config(token, Config), delete, ?config(tag_path, Config)
    ),
    {200, #{~"tags" := []}} =
        blog_data_fixtures:request_as(
            ?config(token, Config), get, "/api/posts/" ++ integer_to_list(PostId)
        ).

patch_is_405(Config) ->
    {405, Headers, <<>>} = raw(
        ?config(token, Config), patch, ?config(tag_path, Config), "application/json", ~"{}"
    ),
    "DELETE, GET, PUT" = proplists:get_value("allow", Headers).

writes_need_a_bearer_token(Config) ->
    {401, #{~"error" := ~"missing bearer token"}} =
        blog_data_fixtures:request(post, "/api/tags", #{name => ~"otp"}),
    {401, _} = blog_data_fixtures:request(put, ?config(tag_path, Config), #{name => ~"otp"}),
    {401, _} = blog_data_fixtures:request(delete, ?config(tag_path, Config)),
    {200, #{~"tags" := [_]}} = blog_data_fixtures:request(get, "/api/tags").

writes_need_the_admin_role(Config) ->
    Author = blog_data_fixtures:token(blog_data_fixtures:user(~"alice")),
    {403, #{~"error" := ~"admins only"}} =
        blog_data_fixtures:request_as(Author, post, "/api/tags", #{name => ~"otp"}),
    {403, _} = blog_data_fixtures:request_as(Author, delete, ?config(tag_path, Config)),
    {200, #{~"name" := ~"erlang"}} = blog_data_fixtures:request(get, ?config(tag_path, Config)).

openapi_spec_lists_api_routes(_Config) ->
    {200, #{~"openapi" := ~"3.0.3", ~"paths" := Paths}} =
        blog_data_fixtures:request(get, "/api/openapi.json"),
    #{~"post" := #{~"operationId" := ~"blog_tags_controller.create"}} = maps:get(
        ~"/api/tags", Paths
    ),
    #{~"get" := _, ~"put" := _, ~"delete" := _} = maps:get(~"/api/tags/{id}", Paths),
    [] = [Path || Path := _ <- Paths, not is_api_path(Path)].

generated_draft07_schema_is_rejected(_Config) ->
    Generated = json:decode(
        ~'{"$schema":"http://json-schema.org/draft-07/schema#","properties":{"id":{"type":"integer"},"name":{"type":"string"}},"required":["id","name"],"type":"object"}'
    ),
    {error, [{schema_invalid, _, {schema_unsupported, _}}]} =
        jesse:validate_with_schema(Generated, #{~"name" => ~"otp"}).

is_api_path(<<"/api/", _/binary>>) -> true;
is_api_path(_) -> false.

raw(Token, Method, Path, ContentType, Body) ->
    Auth = {"authorization", "Bearer " ++ binary_to_list(Token)},
    {ok, {{_, Status, _}, Headers, RespBody}} =
        httpc:request(
            Method,
            {blog_data_fixtures:url(Path), [Auth], ContentType, Body},
            [],
            [{body_format, binary}]
        ),
    {Status, Headers, RespBody}.

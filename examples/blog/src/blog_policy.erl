-module(blog_policy).

-export([can/3]).

can(undefined, _Action, _Resource) ->
    false;
can(Actor, Action, Resource) ->
    Extra = #{type => Action, record => Resource},
    lists:member(true, [Condition(Actor, Extra) || #{condition := Condition} <- policies(Action)]).

policies(create_post) ->
    [nova_auth_policy:allow_authenticated()];
policies(Action) when Action =:= update_post; Action =:= delete_post; Action =:= view_draft ->
    [nova_auth_policy:allow_role(admin), nova_auth_policy:allow_owner(user_id)];
policies(manage_users) ->
    [nova_auth_policy:allow_role(admin)];
policies(_Action) ->
    [nova_auth_policy:deny_all()].

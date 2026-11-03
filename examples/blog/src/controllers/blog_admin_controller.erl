-module(blog_admin_controller).

-export([users/1]).

users(Req) ->
    {ok, Users} = blog_repo:all(kura_query:order_by(kura_query:from(blog_user), [{id, asc}])),
    Rows = [
        #{username => Username, email => Email, role => atom_to_binary(Role)}
     || #{username := Username, email := Email, role := Role} <- Users
    ],
    blog_view:render(Req, blog_admin_users, #{users => Rows}).

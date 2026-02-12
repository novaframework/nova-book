-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

routes(_Environment) ->
  [
    %% Error handlers
    #{routes => [
        {404, fun my_first_nova_error_controller:not_found/1, #{}},
        {500, fun my_first_nova_error_controller:server_error/1, #{}}
     ]},

    %% Public routes
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/ws", my_first_nova_ws_handler, #{protocol => ws}},
                 {"/chat", my_first_nova_chat_handler, #{protocol => ws}},
                 {"/notifications", my_first_nova_notifications_handler, #{protocol => ws}}
                ]
    },

    %% Login POST (uses username/password auth)
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login_post/1, #{methods => [post]}}
                ]
    },

    %% Protected pages (uses session auth)
    #{prefix => "",
      security => fun my_first_nova_auth:session_auth/1,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                 {"/logout", fun my_first_nova_main_controller:logout/1, #{methods => [get]}}
                ]
    },

    %% HTML notes (with session auth)
    #{prefix => "/notes",
      security => fun my_first_nova_auth:session_auth/1,
      routes => [
                 {"/", fun my_first_nova_notes_controller:index/1, #{methods => [get]}},
                 {"/new", fun my_first_nova_notes_controller:new/1, #{methods => [get]}},
                 {"/", fun my_first_nova_notes_controller:create/1, #{methods => [post]}},
                 {"/:id/edit", fun my_first_nova_notes_controller:edit/1, #{methods => [get]}},
                 {"/:id", fun my_first_nova_notes_controller:update/1, #{methods => [post]}},
                 {"/:id/delete", fun my_first_nova_notes_controller:delete/1, #{methods => [post]}}
                ]
    },

    %% JSON API
    #{prefix => "/api",
      security => false,
      routes => [
                 {"/notes", fun my_first_nova_notes_api_controller:index/1, #{methods => [get]}},
                 {"/notes/:id", fun my_first_nova_notes_api_controller:show/1, #{methods => [get]}},
                 {"/notes", fun my_first_nova_notes_api_controller:create/1, #{methods => [post]}},
                 {"/notes/:id", fun my_first_nova_notes_api_controller:update/1, #{methods => [put]}},
                 {"/notes/:id", fun my_first_nova_notes_api_controller:delete/1, #{methods => [delete]}},
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
                 {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
                 {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}}
                ]
    }
  ].

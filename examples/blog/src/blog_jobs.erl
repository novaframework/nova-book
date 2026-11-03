-module(blog_jobs).

-include_lib("kernel/include/logger.hrl").

-export([schedule_maintenance/0, comment_added/1, schedule_publish/2, export_posts/1]).

schedule_maintenance() ->
    case blog_purge_worker:schedule_next() of
        {ok, _JobOrConflict} -> ok;
        {error, Reason} -> ?LOG_WARNING(#{msg => ~"could not schedule purge", reason => Reason})
    end.

comment_added(#{id := CommentId}) ->
    Job = #{worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}},
    case shigoto:insert(Job) of
        {ok, _JobOrConflict} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(#{
                msg => ~"could not enqueue comment notification",
                comment_id => CommentId,
                reason => Reason
            })
    end.

schedule_publish(#{id := PostId}, At) ->
    shigoto:insert(#{
        worker => blog_publish_worker,
        args => #{~"post_id" => PostId},
        scheduled_at => At
    }).

export_posts(#{id := UserId}) ->
    shigoto:insert(#{worker => blog_export_worker, args => #{~"user_id" => UserId}}).

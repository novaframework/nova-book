-module(blog_jobs).

-include_lib("kernel/include/logger.hrl").

-export([
    schedule_maintenance/0,
    comment_added/1,
    account_email/2,
    schedule_publish/2,
    export_posts/1
]).

schedule_maintenance() ->
    case blog_purge_worker:schedule_next() of
        {ok, _JobOrConflict} -> ok;
        {error, Reason} -> ?LOG_WARNING(#{msg => ~"could not schedule purge", reason => Reason})
    end.

comment_added(#{id := CommentId}) ->
    enqueue(#{worker => blog_comment_notification_worker, args => #{~"comment_id" => CommentId}}).

account_email(Kind, #{id := UserId}) when Kind =:= confirm; Kind =:= reset ->
    enqueue(#{
        worker => blog_account_email_worker,
        args => #{~"email" => atom_to_binary(Kind), ~"user_id" => UserId}
    }).

schedule_publish(#{id := PostId}, At) ->
    shigoto:insert(#{
        worker => blog_publish_worker,
        args => #{~"post_id" => PostId},
        scheduled_at => At
    }).

export_posts(#{id := UserId}) ->
    shigoto:insert(#{worker => blog_export_worker, args => #{~"user_id" => UserId}}).

enqueue(#{worker := Worker, args := Args} = Job) ->
    case shigoto:insert(Job) of
        {ok, _JobOrConflict} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(#{
                msg => ~"could not enqueue job", worker => Worker, args => Args, reason => Reason
            })
    end.

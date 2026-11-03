-module(blog_comment_notification_worker).
-behaviour(shigoto_worker).

-export([perform/1, queue/0, max_attempts/0, backoff/2, unique/0]).

queue() -> ~"mailers".

max_attempts() -> 5.

backoff(Attempt, _Reason) -> 30 * Attempt.

unique() -> #{keys => [worker, args], period => infinity}.

perform(#{~"comment_id" := CommentId}) ->
    case blog_repo:get(blog_comment, CommentId) of
        {ok, Comment} ->
            notify(blog_repo:preload(blog_comment, Comment, [author, {post, [author]}]));
        {error, not_found} ->
            ok
    end.

notify(#{user_id := AuthorId, post := #{user_id := AuthorId}}) ->
    ok;
notify(Comment) ->
    blog_mailer:deliver_from_job(blog_emails:comment_notification(Comment)).

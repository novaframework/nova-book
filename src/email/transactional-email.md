# Transactional Email

In the previous chapter we set up Hikyaku and built email helpers. Now let's wire emails into real application flows — registration confirmation, password reset, and comment notifications.

## Registration confirmation

When a user registers, send a confirmation email with a time-limited token:

```erlang
-module(blog_accounts).
-export([register_user/1, confirm_user/1]).

register_user(Params) ->
    CS = user:registration_changeset(#{}, Params),
    case blog_repo:insert(CS) of
        {ok, User} ->
            Token = generate_token(maps:get(id, User), <<"confirm">>, 24),
            blog_emails:confirmation(User, Token),
            {ok, User};
        {error, CS1} ->
            {error, CS1}
    end.

confirm_user(Token) ->
    case verify_token(Token, <<"confirm">>) of
        {ok, UserId} ->
            {ok, User} = blog_repo:get(user, UserId),
            CS = kura_changeset:cast(user, User,
                #{<<"confirmed_at">> => calendar:universal_time()},
                [confirmed_at]),
            blog_repo:update(CS);
        {error, _} ->
            {error, invalid_token}
    end.
```

The email helper:

```erlang
-module(blog_emails).
-export([welcome/1, confirmation/2, password_reset/2, comment_notification/2]).

confirmation(#{email := Email, username := Username}, Token) ->
    ConfirmUrl = <<"https://myblog.com/confirm?token=", Token/binary>>,
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, {<<"Nova Blog">>, <<"noreply@myblog.com">>}),
    E2 = hikyaku_email:to(E1, Email),
    E3 = hikyaku_email:subject(E2, <<"Confirm your email">>),
    E4 = hikyaku_email:text_body(E3,
        <<"Hi ", Username/binary, ",\n\n",
          "Click the link below to confirm your email:\n\n",
          ConfirmUrl/binary, "\n\n",
          "This link expires in 24 hours.">>),
    E5 = hikyaku_email:html_body(E4,
        <<"<h1>Confirm your email</h1>",
          "<p>Hi ", Username/binary, ",</p>",
          "<p><a href=\"", ConfirmUrl/binary, "\">Click here to confirm</a></p>",
          "<p>This link expires in 24 hours.</p>">>),
    hikyaku_mailer:deliver(blog_mailer, E5).
```

## Password reset

```erlang
request_password_reset(Email) ->
    case blog_repo:get_by(user, [{email, Email}]) of
        {ok, User} ->
            Token = generate_token(maps:get(id, User), <<"reset">>, 1),
            blog_emails:password_reset(User, Token),
            ok;
        {error, not_found} ->
            %% Don't reveal whether the email exists
            ok
    end.

reset_password(Token, NewPassword) ->
    case verify_token(Token, <<"reset">>) of
        {ok, UserId} ->
            {ok, User} = blog_repo:get(user, UserId),
            CS = user:password_changeset(User, #{<<"password">> => NewPassword}),
            blog_repo:update(CS);
        {error, _} ->
            {error, invalid_token}
    end.
```

```erlang
password_reset(#{email := Email, username := Username}, Token) ->
    ResetUrl = <<"https://myblog.com/reset-password?token=", Token/binary>>,
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:from(E0, {<<"Nova Blog">>, <<"noreply@myblog.com">>}),
    E2 = hikyaku_email:to(E1, Email),
    E3 = hikyaku_email:subject(E2, <<"Reset your password">>),
    E4 = hikyaku_email:text_body(E3,
        <<"Hi ", Username/binary, ",\n\n",
          "Click below to reset your password:\n\n",
          ResetUrl/binary, "\n\n",
          "This link expires in 1 hour.\n",
          "If you didn't request this, ignore this email.">>),
    hikyaku_mailer:deliver(blog_mailer, E4).
```

## Comment notifications

Notify post authors when someone comments, triggered from PubSub:

```erlang
-module(blog_notification_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    nova_pubsub:join(comments),
    {ok, #{}}.

handle_info({nova_pubsub, comments, _Sender, "comment_created", Comment}, State) ->
    PostId = maps:get(post_id, Comment),
    {ok, Post} = blog_repo:get(post, PostId),
    Post1 = blog_repo:preload(post, Post, [author]),
    Comment1 = blog_repo:preload(comment, Comment, [author]),

    %% Don't notify if the author commented on their own post
    PostAuthorId = maps:get(id, maps:get(author, Post1)),
    CommentAuthorId = maps:get(user_id, Comment1),
    case PostAuthorId =/= CommentAuthorId of
        true -> blog_emails:comment_notification(Post1, Comment1);
        false -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Req, _From, State) -> {reply, ok, State}.
```

Add this worker to your supervisor to start automatically.

## Testing email delivery

Use the test adapter to capture emails in tests. Configure `blog_mailer` to use the test adapter in your test environment, with `self()` as the receiving process:

```erlang
%% In test config or test setup
%% Override blog_mailer to use the test adapter:
config() ->
    #{adapter => hikyaku_adapter_test,
      pid => self()}.
```

```erlang
%% In your test
test_registration_sends_email(_Config) ->
    {ok, _User} = blog_accounts:register_user(#{
        <<"username">> => <<"testuser">>,
        <<"email">> => <<"test@example.com">>,
        <<"password">> => <<"password123">>
    }),
    receive
        {hikyaku_email, Email} ->
            <<"Confirm your email">> = hikyaku_email:get_subject(Email),
            ok
    after 1000 ->
        ct:fail("No email received")
    end.
```

## Token generation helpers

```erlang
generate_token(UserId, Purpose, ExpiryHours) ->
    Payload = #{user_id => UserId, purpose => Purpose,
                expires_at => erlang:system_time(second) + ExpiryHours * 3600},
    base64:encode(term_to_binary(Payload)).

verify_token(Token, ExpectedPurpose) ->
    try
        Payload = binary_to_term(base64:decode(Token)),
        #{user_id := UserId, purpose := Purpose, expires_at := ExpiresAt} = Payload,
        Now = erlang:system_time(second),
        case Purpose =:= ExpectedPurpose andalso ExpiresAt > Now of
            true -> {ok, UserId};
            false -> {error, expired}
        end
    catch _:_ ->
        {error, invalid}
    end.
```

```admonish warning
This is a simplified token implementation for illustration. In production, use cryptographically signed tokens (e.g. HMAC-SHA256) and store token hashes in the database for revocation.
```

---

With email integrated, let's ensure everything works with proper [Unit Testing](../testing/unit-testing.md).

-module( twitter_engine_client_utils ).

-export( [ register_user/3, init/1, stop/1, handle_cast/2, terminate/2, handle_call/3, subscribe_to_user/2, tweet_message/2, retweet_message/2, get_timeline_for_user/1, search_by_hashtags/2, search_by_mentions/2, login_user/3 ] ). 

% Using gen server
-behavior(gen_server).


% GEN SERVER HELPER METHODS
init( User ) ->
    io:fwrite("Starting!\n"),
    {ok, User}.
stop(Name) ->
    gen_server:cast(Name, stop).
terminate(_Reason, _LoopData) ->
    io:fwrite("Stoping ~p\n", [_LoopData]).

handle_call(Message, User, LoopData) ->
    io:fwrite("Recieved message ~p from ~p with loopdata ~p", [Message, User, LoopData]),
    {reply, "Hello", LoopData}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData};
handle_cast({recieve_tweet, Tweet}, LoopData) ->
    io:fwrite("~p Recieved Tweet:\n~p\n\n", [LoopData, Tweet]),
    {noreply, LoopData}.

% TWITTER ENGINE CLIENT FUNCTIONS:
register_user ( User, Username, Password ) ->
    io:fwrite("Registering User Client:~p\n", [ User ]),
    gen_server:start_link({ local, User }, ?MODULE, User, [] ),
     io:fwrite("Registering User Client:~p\n", [ User ]),
    twitter_engine_server_utils:register_user_update_table(Username,Password).
login_user ( User, Username, Password ) ->
    io:fwrite("Logging in User Client:~p\n", [ User ]),
    IsRegistered = twitter_engine_server_utils:is_user_registered(Username,Password),
    IsRegistered.
subscribe_to_user ( User, Follower ) ->
    % io:fwrite( "\nFollowing!!\n" ),
    gen_server:cast( twitter_engine_server_utils, { subscribe_user, User, Follower } ).

tweet_message( User, Message ) ->
    % { ok, Message } = io:read( "Enter Message: "),
    gen_server:cast( twitter_engine_server_utils, { tweet_message, User, Message } ).
retweet_message( User, I ) ->
    gen_server:cast( twitter_engine_server_utils, { retweet_message, User, I } ).

get_timeline_for_user( User ) ->
    AllTweets = gen_server:call(twitter_engine_server_utils, { tweet_list, User } ),
    io:fwrite("~p : Twitter Home:\n ~p", [ User, AllTweets ] ),
    ListTweets = [{element(1,X),element(2, X), element(3, X)}  || X <- AllTweets],
    ListTweets.

search_by_hashtags ( User, HashTag ) ->
    AllTweets = gen_server:call(twitter_engine_server_utils, { tweet_tags, HashTag } ),
    io:fwrite( "~p : HashTags ~p:\n ~p", [ User, HashTag, AllTweets ] ),
    ListTweets = [{element(1,X),element(2, X), element(3, X)}  || X <- AllTweets],
    ListTweets.
search_by_mentions ( User, Mention ) ->
    AllTweets = gen_server:call(twitter_engine_server_utils, { tweet_mentions, Mention }),
    io:fwrite( "~p : Mentions ~p:\n ~p", [ User, Mention, AllTweets ] ),
    ListTweets = [{element(1,X),element(2, X), element(3, X)}  || X <- AllTweets],
    ListTweets.
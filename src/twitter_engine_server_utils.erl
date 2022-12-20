-module( twitter_engine_server_utils ).

-import( lists,[ nth/2 ] ).
-export( [ intialize_gen_server_link/0, init/1, handle_cast/2, handle_call/3, stop/0, terminate/2, register_user_update_table/2, is_user_registered/2 ] ).

% Using gen server
-behavior(gen_server).

% GEN SERVER HELPER METHODS
% Setting named tables using ETS
init( _Args ) ->
    % User Tables
    ets:new( user_following_list, [ bag, named_table ] ),
    ets:new( user_followers_list, [ bag, named_table ] ),
    ets:new( user_hashtags, [ bag, named_table ] ),
    ets:new( user_mentions, [ bag, named_table ] ),

    % Manage Tweets Tables
    ets:new( tweet_uid, [ bag, named_table ] ),
    ets:new( tweet_uid_username, [ bag, named_table ] ),
    ets:new( retweet_uid, [ bag, named_table ] ),

    %Login table
    ets:new( user_table_list, [ bag, named_table, public ] ),

    {ok, self()}.

% Initialize the gen server 
intialize_gen_server_link( ) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call( { tweet_list, User }, _From, LoopData) ->
    AllFollowers = ets:lookup( user_followers_list, User),
    AllRetweets = get_all_tweets( retweet_uid, AllFollowers, length(AllFollowers), []),
    AllTweets = get_all_tweets( tweet_uid, AllFollowers, length(AllFollowers), AllRetweets),
    {reply, AllTweets, LoopData};
handle_call( { tweet_tags, Tag }, _From, LoopData) ->
    Tweets = ets:lookup( user_hashtags, Tag),
    {reply, Tweets, LoopData};
handle_call( { tweet_mentions, Mention }, _From, LoopData) ->
    Tweets = ets:lookup( user_mentions, Mention),
    {reply, Tweets, LoopData}.


handle_cast( { subscribe_user, User, Subscriber }, LoopData) ->
    io:fwrite( "\nFollowed Successfully!!\n" ),
    ets:insert( user_followers_list, {User, Subscriber}),
    ets:insert( user_followers_list, {Subscriber, User} ),
    io:fwrite("contents of the main ~p\n", [ets:lookup( user_followers_list, User)]),
    {noreply, LoopData};
handle_cast( { tweet_message, User, Message }, LoopData ) ->
    Uid = helper_total_length( tweet_uid ) + 1,
    Tweet = {User, Uid, Message, erlang:universaltime()},
    ets:insert( tweet_uid, Tweet),
    ets:insert( tweet_uid_username, {Uid, Message, User}),
    update_followers_list(User, Tweet),
    io:fwrite("\nTweeted!!\n"),
    io:fwrite("contents of the main ~p and size ~p\n", [ets:lookup( tweet_uid, User), helper_total_length( tweet_uid )]),
    HashTagList = ht_list( Message, "(?<=#)\\w+" ),
    AddHashTag = fun(E) -> io:fwrite( "\nTweet Contains Hashtag ~p \n", [ nth(1, E) ] ), ets:insert( user_hashtags, { nth(1, E), Uid, Message} ), true end,
    lists:all(AddHashTag, HashTagList),
    MentionList = mentions_list( Message, "(?<=@)\\w+" ) ,
    AddMention = fun(E) -> io:fwrite( "\nTweet Contains Mention ~p \n", [ nth(1, E) ] ), ets:insert( user_mentions, { nth(1, E), Uid, Message } ), true end,
    lists:all( AddMention, MentionList ),
    { noreply, LoopData };
handle_cast( { retweet_message, User, I }, LoopData ) ->
    [ { OrigTweetInd, OrigTweetStr, OrigUser } ] = ets:lookup( tweet_uid_username, I ),
    ReTweeetIndex = helper_total_length( retweet_uid ) + 1,
    Tweet = { User, ReTweeetIndex, OrigTweetStr, OrigUser, OrigTweetInd, erlang:universaltime( ) },
    ets:insert( retweet_uid, Tweet),
    update_followers_list( User, Tweet ),
    { noreply, LoopData };
handle_cast( stop, LoopData ) ->
    { stop, normal, LoopData }.


stop() ->
    gen_server:cast(?MODULE, stop).
terminate(_Reason, _LoopData) ->
    io:fwrite("Stopping:~p\n", [ _LoopData ]).

% TWITTER ENGINE HELPER METHODS

register_user_update_table(Username, Password) ->
    io:fwrite("Registering table:~p ~p\n", [ Username, Password ]),
    ets:insert( user_table_list, {Username,Password}),
    io:fwrite("contents of the main ~p and size ~p\n", [ets:lookup( user_table_list, Username), helper_total_length( user_table_list )]).

is_user_registered(Username, Password) ->
    io:fwrite("Checking Valid user table:~p ~p\n", [ Username, Password ]),
    UserVal = ets:lookup( user_table_list, Username),
    % if Pwd == Password
    % IsUserRegistered = Pwd == Password,
    [{_,Pwd}] = UserVal,
    io:fwrite("Checking Valid user table:~p ~p\n", [ Username, UserVal ]),
    io:fwrite("contents of the main ~p and size ~p\n", [ets:lookup( user_table_list, Username), helper_total_length( user_table_list )]),
    Pwd == Password.

% Send update to the followers list
update_followers_list(From, Tweet) ->
    FollowerList = ets:lookup( user_followers_list, From),
    io:fwrite("contents of the main ~p and size ~p\n", [ets:lookup( user_followers_list, From), helper_total_length( user_followers_list )]),
    send_msg_to_follower( From, Tweet, FollowerList, length(FollowerList) ).


% Tweet to all followers list, iterate over the followers list
send_msg_to_follower( _, _, _, 0 ) -> ok;
send_msg_to_follower( Send_from, Tweet_msg, User_followers, I ) ->
    io:fwrite("Send msg to follower:~p ~p ~p ~p\n", [ Send_from, Tweet_msg, User_followers, I ]),
    gen_server:cast( element(2, nth( I, User_followers ) ), { recieve_tweet, Tweet_msg } ),
    send_msg_to_follower( Send_from, Tweet_msg, User_followers, I-1 ).

ht_list( Message, Regex ) ->
    HashTagFound = re:run( Message, Regex, [ global, { capture, all, list } ] ),
    if HashTagFound /= nomatch ->
        { match, HashTagList } = HashTagFound,
        HashTagList;
    true ->
        HashTagList = [],
        HashTagList
    end.
mentions_list( Message, Regex ) ->
    MentionsFound = re:run( Message, Regex, [global, { capture, all, list } ] ),
    if MentionsFound /= nomatch ->
        { match, MentionList } = MentionsFound,
        MentionList;
    true ->
        MentionList = [],
        MentionList
    end.


get_all_tweets(_, _, 0, TweetsList) -> TweetsList;
get_all_tweets(Table, FollowingList, Index, TweetsList) ->
    User = element(2, nth(Index, FollowingList)),
    UserTweets = ets:lookup(Table, User),
    NewTweetsList = lists:append(TweetsList, UserTweets),
    get_all_tweets(Table, FollowingList, Index-1, NewTweetsList).




% HELPER FUNCTIONS
% Return total for the input Uid
helper_total_length( Uid ) ->
    Info = ets:info( Uid ),
    if (Info == undefined) ->
        0;
    true ->
        element(2, nth(10, Info))
    end.

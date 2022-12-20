-module(tweet_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-import( lists,[ nth/2 ] ).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 6000000}}.

websocket_init(State) ->
	% (<<"Hello!">>),
    % twitter_engine_server_utils:intialize_gen_server_link(),
	{[{text, <<"Hello Tweet Handler!">>}], State}.

websocket_handle({text, Msg}, State) ->
    RecievedObj = binary_to_list(Msg),
    io:fwrite("Tweeting Msg:~p\n", [RecievedObj]),

    [Username, TweetMsg] = string:tokens(RecievedObj, "|"),
    % TweetMsg=binary_to_list(ParsedObj[1]),
    User = list_to_atom(Username),
    io:fwrite("User:~p\n", [User]),
    io:fwrite("Tweet:~p\n", [TweetMsg]),

    twitter_engine_client_utils:tweet_message(User,TweetMsg),
    AllTweets = twitter_engine_client_utils:get_timeline_for_user(User),
    Results = lists:flatten(io_lib:format("~p", [AllTweets])),
    io:fwrite("Results:~p\n", [Results]),
    ResultsBinary = list_to_binary(Results),
    {[{text, << "Tweet Sent|",ResultsBinary/binary >>}], State};

websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	% erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{[{text, << "Info ", Msg/binary >>}], State};

websocket_info(_Info, State) ->
	{[], State}.

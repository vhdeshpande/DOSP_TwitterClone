-module(search_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 60000000}}.

websocket_init(State) ->
	% (<<"Hello!">>),
    % twitter_engine_server_utils:intialize_gen_server_link(),
	{[{text, <<"">>}], State}.

websocket_handle({text, Msg}, State) ->
    RecievedObj = binary_to_list(Msg),
    [UserName, Search, SearchBy] = string:tokens(RecievedObj, "|"),
    User = list_to_atom(UserName),
    io:fwrite("Search Tweet:~p\n", [Search]),
    io:fwrite("Search By:~p\n", [SearchBy]),
    % ResultsBinary= "",
    if (SearchBy == "HashTags") ->
        search_by_hashtags(User, Search, State);
    true->
       search_by_mentions(User, Search, State)
    end;
	% {[{text, << "Tweets Found ", ResultsBinary >>}], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	% erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{[{text, << "Info ", Msg/binary >>}], State};

websocket_info(_Info, State) ->
	{[], State}.

search_by_hashtags(User, SearchStr, State) ->
    AllTweets = twitter_engine_client_utils:search_by_hashtags(User,SearchStr),
    io:fwrite("All Tweets:~p\n", [AllTweets]),
    Results = lists:flatten(io_lib:format("~p", [AllTweets])),
    io:fwrite("Results:~p\n", [Results]),
    ResultsBinary = list_to_binary(Results),
    {[{text, << ResultsBinary/binary >>}], State}.

search_by_mentions(User, SearchStr, State) ->
    AllTweets = twitter_engine_client_utils:search_by_mentions(User,SearchStr),
    io:fwrite("All Tweets:~p\n", [AllTweets]),
    Results = lists:flatten(io_lib:format("~p", [AllTweets])),
    io:fwrite("Results:~p\n", [Results]),
    ResultsBinary = list_to_binary(Results),
    {[{text, << ResultsBinary/binary >>}], State}.


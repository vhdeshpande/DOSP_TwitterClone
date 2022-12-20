-module(login_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 60000000}}.

websocket_init(State) ->
    twitter_engine_server_utils:intialize_gen_server_link(),
	{[{text, <<"Hello Login Handler!">>}], State}.

websocket_handle({text, Msg}, State) ->
    MsgList=binary_to_list(Msg),
    [Username, Password] = string:tokens(MsgList, "|"),
    User = list_to_atom(Username),
    IsRegistered = twitter_engine_client_utils:login_user(User,Username, Password),
    if IsRegistered == true ->
        AllTweets = twitter_engine_client_utils:get_timeline_for_user(User),
        Results = lists:flatten(io_lib:format("~p", [AllTweets])),
        io:fwrite("Results:~p\n", [Results]),
        ResultsBinary = list_to_binary(Results),
        {[{text, << "User Logged In|",ResultsBinary/binary >>}], State};
    true ->
        {[{text, << "Invalid username and password|",Msg/binary >>}], State}
    end;
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{[{text, << "Info ", Msg/binary >>}], State};

websocket_info(_Info, State) ->
	{[], State}.

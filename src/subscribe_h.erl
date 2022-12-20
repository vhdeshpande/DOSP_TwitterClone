-module(subscribe_h).

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
    [User, UserToFollow] = string:tokens(RecievedObj, "|"),
    UserAtom = list_to_atom(User),
    UserToFollowAtom = list_to_atom(UserToFollow),
    io:fwrite("Following User:~p\n", [UserToFollow]),
    twitter_engine_client_utils:subscribe_to_user(UserAtom,UserToFollowAtom),
	{[{text, << "Followed User ", Msg/binary >>}], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	% erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{[{text, << "Info ", Msg/binary >>}], State};

websocket_info(_Info, State) ->
	{[], State}.

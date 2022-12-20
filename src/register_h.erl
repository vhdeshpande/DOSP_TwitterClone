-module(register_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 60000000}}.

websocket_init(State) ->
    twitter_engine_server_utils:intialize_gen_server_link(),
	{[{text, <<"">>}], State}.

websocket_handle({text, Msg}, State) ->
    MsgList=binary_to_list(Msg),
    [Username, Password] = string:tokens(MsgList, "|"),
    User = list_to_atom(Username),
    io:fwrite("Registering User:~p\n", [User]),
    twitter_engine_client_utils:register_user(User,Username, Password),
	{[{text, << "Registered User Successfully, Login to continue|", Msg/binary >>}], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{[{text, << "Info ", Msg/binary >>}], State};

websocket_info(_Info, State) ->
	{[], State}.

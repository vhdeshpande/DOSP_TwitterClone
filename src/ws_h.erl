-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 60000000}}.

websocket_init(State) ->
	% (<<"Hello!">>),
	{[{text, <<"">>}], State}.

websocket_handle({text, Msg}, State) ->
	io:fwrite("~p Recieved Tweet:\n~p\n\n", [Msg, State]),
	{[{text, << "That's what she said! ", Msg/binary >>}], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	% erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{[{text, << "Info ", Msg/binary >>}], State};

websocket_info(_Info, State) ->
	{[], State}.

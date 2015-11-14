%% A Minimal IRC with plain Erlang
%%
%% Functional Interface/User Interface:
%% connect(Nickname)
%% 	One user at a time can connect to the IRC Server from each Erlang node. If the Name already exist it will reject the connection with a suitable error message.
%%
%% disconnect()
%%      Disconnects the current connected user at the current node.
%%
%% nickname(Nickname)
%%     Allows a connected user to change his nick. If the user is not already connected, an error will be sent by the server.
%%
%% list()
%%      Once connected it will list the available channels that the user can join.
%%
%% join(Channel)
%%      Joins a Channel/Chat Room. If the room does not exists, it will be created by the server.
%%
%% leave(Channel)
%%     Leave the specified Channel, if no Channel was specified leave the current channel, that's it, the last joined Channel.
%%
%% names(Channel)
%%     Shows the names of all users in a Channel.
%%
%% whois(Nickname)
%%    Shows information about the specified Nickname.
%%
%% message(Nickname, Message)
%%    Sends a private message to a User. If the user does not exists the server will send an error message.
%%
%% motd()
%%     Returns the Message of the Day from the Server.
%%
%% The server process will be registered by the name "ircd"

-module(irc).
-export([start_server/0, server_loop/1, client/1, client_loop/1,connect/1,list/0,join/1]).

%% Change this to specify the node where the server runs.
%% TODO: This can be set in a different way at runtime using a config file or by command line argument when converting this to an Application.
-define(SERVER_NODE, 'server@vagrant-ubuntu-trusty-64').
-define(SERVER_INSTANCE_NAME, irc_server).
-define(CLIENT_INSTANCE_NAME, irc_client).

-record(irc_channel, {name, topic="", users = []}).
-record(server_state, {users = [], channels = []}).

%% @doc Start the server process and register it with known name.
start_server() ->
	error_logger:info_msg("Server > Creating server process under name: ~s~n", [?SERVER_INSTANCE_NAME]),
	register(?SERVER_INSTANCE_NAME, spawn(irc, server_loop, [#server_state{}])).

server_loop(ServerState) ->
	receive
		{From, connect, Nickname} ->
			error_logger:info_msg("Server > Received a connect message from ~s", [Nickname]),
		       	UpdatedServerState = server_handle_connect(From, ServerState, Nickname);
		{From, list} ->
			error_logger:info_msg("Server > Received a list message~n"),
			UpdatedServerState = server_handle_list(From, ServerState);
		{From, join, ChannelName} ->
			error_logger:info_msg("Server > Received a join(~s) message", [ChannelName]),
			UpdatedServerState = server_handle_join(From, ServerState, ChannelName)
	end,
	server_loop(UpdatedServerState).

server_tell(To, Message) ->
	To ! {?SERVER_INSTANCE_NAME, Message}.

%% @doc get users from server_state 
server_state_users(#server_state{users = Users}) -> Users.

%% @doc set users on server_state
server_state_users(ServerState, UpdatedUsers) -> ServerState#server_state{users = UpdatedUsers}.

%% @doc check if a user exists
server_state_has_user(ServerState, {_Sender, Nickname}) -> lists:keymember(Nickname, 2, server_state_users(ServerState)).

%% @doc get channels from server_state
server_state_channels(#server_state{channels = Channels}) -> Channels.

%% @doc set channels to server_state
server_state_channels(ServerState, UpdatedChannels) -> ServerState#server_state{channels = UpdatedChannels}.

%% @doc add user to server_state if the user is not already registered. This is business logic. 
may_add_user_to_server_state(ServerState, User) -> 
	case server_state_has_user(ServerState, User) of
	      	true -> {error, user_already_exists};
		false ->
			UserList = server_state_users(ServerState),
	 		UpdatedUsers = [User | UserList],
			{ok, server_state_users(ServerState, UpdatedUsers)}
	end.

%% @doc Handle a connect message
server_handle_connect(Sender, ServerState, Nickname) ->
	case may_add_user_to_server_state(ServerState, {Sender, Nickname}) of
		{ok, UpdatedServerState} ->
			server_tell(Sender, connected),
			UpdatedServerState;
		{error, user_already_exists} ->
			server_tell(Sender, {stop, user_exists_at_other_node}),
			ServerState
	end.

server_handle_list(Sender, ServerState = #server_state{channels=ChannelList}) ->
	server_tell(Sender, {list_response, ChannelList}),
	ServerState.

server_handle_join(Sender, ServerState = #server_state{channels=ChannelList}, ChannelName) ->
	UpdatedChannelList = [ChannelName | ChannelList],
	UpdatedServerState = ServerState#server_state{channels=UpdatedChannelList},
	server_tell(Sender, {join_response, ok}),
	UpdatedServerState.

connect(Nickname) ->
	case whereis(?CLIENT_INSTANCE_NAME) of
		undefined ->
			error_logger:info_msg("Client > Creating client process under name: ~s~n", [?CLIENT_INSTANCE_NAME]),
		       	register(?CLIENT_INSTANCE_NAME, spawn(irc, client, [Nickname]));
		_ -> 
			error_logger:error_msg("Client > Client process under name ~s already exists.~n", [?CLIENT_INSTANCE_NAME]),
		   	already_connected
	end.

list() ->
	{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), list},
	receive
		{?SERVER_INSTANCE_NAME, {list_response, ChannelList}} ->
			error_logger:info_msg("Client > Received list of channels from server ~p~n", [ChannelList])
	end.


join(ChannelName) ->
	error_logger:info_msg("Client > Sending join(~s) command to server", [ChannelName]),
	{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), join, ChannelName},
	await_result().

client(Nickname) ->
	error_logger:info_msg("Client > Sending connect command to server using Nickname ~s", [Nickname]),
	{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), connect, Nickname},
	await_result(),
	client_loop(Nickname).

await_result() ->
	receive
		{?SERVER_INSTANCE_NAME, {stop, Why}} ->
			error_logger:error_msg("Client > Server says to stop. Exiting because of ~p~n", [Why]),
			exit(normal);
		{?SERVER_INSTANCE_NAME, What} ->
			error_logger:info_msg("Client > Received ~p from server~n", [What])
	end.

client_loop(Nickname) ->
	client_loop(Nickname).


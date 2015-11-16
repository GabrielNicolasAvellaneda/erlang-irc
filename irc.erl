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


%% Separation of concerns: Client logic, Server handling, Server business rules, State model management.

-module(irc).
-export([start_server/0, server_loop/1, client/1, client_loop/1,connect/1,list/0,join/1]).

-include_lib("eunit/include/eunit.hrl").

%% Change this to specify the node where the server runs.
%% TODO: This can be set in a different way at runtime using a config file or by command line argument when converting this to an Application.
-define(SERVER_NODE, 'server@vagrant-ubuntu-trusty-64').
-define(SERVER_INSTANCE_NAME, irc_server).
-define(CLIENT_INSTANCE_NAME, irc_client).

-record(channel, {name, topic="", users = []}).
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


%% @doc Create a new server_state. Encapsulates server_state record creation.
server_state_create() ->
	#server_state{}.

server_state_create_channel(ChannelName) ->
	server_state_create_channel(ChannelName, []).

server_state_create_channel(ChannelName, Users) ->
	#channel{name=ChannelName, users=Users}.

%% @doc get users from server_state 
-spec server_state_get_users(#server_state{})->list(tuple()).
server_state_get_users(#server_state{users = Users}) -> Users.

%% @doc set users on server_state
server_state_set_users(ServerState, UpdatedUsers) -> ServerState#server_state{users = UpdatedUsers}.

%% @doc add a user to the server_state
-spec server_state_add_user(#server_state{}, tuple()) -> #server_state{}.
server_state_add_user(ServerState, User) ->
	UserList = server_state_get_users(ServerState),
	UpdatedUsers = [User | UserList],
	server_state_set_users(ServerState, UpdatedUsers).

%% @doc check if a user exists
server_state_has_user(ServerState, {_Sender, Nickname}) -> lists:keymember(Nickname, 2, server_state_get_users(ServerState)).

%% @doc get a user from the server_state record.
server_state_get_user_by_sender(ServerState, Sender) -> lists:keyfind(Sender, 1, server_state_get_users(ServerState)). 

%% TODO: Optimize this to get the first match.
%% TODO: How to specify types that returns records
channels_find_by_name(Name, Channels) ->
	Result = lists:filter(fun (X) -> X#channel.name =:= Name end, Channels),
	if
		length(Result) > 0 -> hd(Result);
		true -> false
	end.

channel_get_name(Channel) -> Channel#channel.name.

%% @doc get channels from server_state
server_state_get_channels(#server_state{channels = Channels}) -> Channels.

%% @doc set channels to server_state
server_state_set_channels(ServerState, UpdatedChannels) -> ServerState#server_state{channels = UpdatedChannels}.

server_state_add_channel(ServerState, Channel) ->
	Channels = server_state_get_channels(ServerState),
	UpdatedChannels = [Channel | Channels],
	server_state_set_channels(ServerState, UpdatedChannels).

channel_users(#channel{users=Users}) -> Users.

channel_users(Channel = #channel{}, UpdatedUsers) -> Channel#channel{users=UpdatedUsers}.

%% @doc add user to server_state if the user is not already registered. This is business logic. 
%% TODO: Get a better name, like may_connect_user?
may_add_user_to_server_state(ServerState, User) -> 
	case server_state_has_user(ServerState, User) of
	      	true -> {error, user_already_exists};
		false -> {ok, server_state_add_user(ServerState, User)}
	end.

server_state_channels_find_by_name(ServerState, ChannelName) ->
	ok.

server_state_channels_add(Channel) ->
	ok.

%% @doc List the all the channel names in the server
server_list_channel_names(ServerState) ->
	Channels = server_state_get_channels(ServerState),
	lists:map(fun channel_get_name/1, Channels).

%% Separate logic into Server Message Handling, Data Structure(Model), Business Rules.

%% TODO: Refactor this to be more readable.
add_channel_and_user_to_server_state(ServerState, ChannelName, User) ->
	Channels = server_state_get_channels(ServerState),
	Channel = channels_find_by_name(ChannelName, Channels),
	if
		Channel == false ->
			NewChannel = #channel{name=ChannelName, users=[User]},
			UpdatedChannels = [NewChannel | Channels],
			server_state_set_channels(ServerState, UpdatedChannels);
		true ->
			%% TODO: Do a unit test of this.
			OtherChannels = Channels -- [Channel],
			UserList = channel_users(Channel),
			UpdatedUsers = [User | UserList],
			UpdatedChannel = channel_users(Channel, UpdatedUsers),
			UpdatedChannels = [UpdatedChannel | OtherChannels],
			server_state_set_channels(ServerState, UpdatedChannels)
	end.

%% @doc Handle a connect message
server_handle_connect(Sender, ServerState, Nickname) ->
	%% TODO: Encapsulate user structure?
	%% TODO: Create different function for handling ok and error situations to remove the case?
	case may_add_user_to_server_state(ServerState, {Sender, Nickname}) of
		{ok, UpdatedServerState} ->
			server_tell(Sender, connected),
			UpdatedServerState;
		{error, user_already_exists} ->
			server_tell(Sender, {stop, user_exists_at_other_node}),
			ServerState
	end.

%% TODO: Just returns the list of Channels without users.
server_handle_list(Sender, ServerState) ->
	ChannelNames = server_list_channel_names(ServerState),
	server_tell(Sender, {list_response, ChannelNames}),
	ServerState.

server_handle_join(Sender, ServerState = #server_state{channels=ChannelList}, ChannelName) ->
	User = server_state_get_user_by_sender(ServerState, Sender),
	UpdatedServerState = add_channel_and_user_to_server_state(ServerState, ChannelName, User), 
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

%% Tests for Server state model manipulation.

should_create_a_new_server_state_test() ->
	ServerState = server_state_create(),
	?assert(ServerState =:= #server_state{}).

should_get_empty_users_for_a_newly_created_server_state_test() ->
	ServerState = server_state_create(),
	?assert(server_state_get_users(ServerState) =:= []).

should_set_users_to_server_state_test() ->
	ServerState = server_state_create(), 
	%% TODO: Define a User type and think about a value that can be used for testing in replacement for a User Pid (?maybe an atom?).
	AllUsers = [{user_pid, 'Mike'}, {user_pid, 'Jonhhy'}],
	UpdatedServerState = server_state_set_users(ServerState, AllUsers),
	?assert(server_state_get_users(UpdatedServerState) =:= AllUsers).

should_add_user_to_server_state_test() ->
	ServerState = server_state_create(),
	NewUser = {user_pid, "Emma"},
	UpdatedServerState = server_state_add_user(ServerState, NewUser),
	?assert(server_state_get_users(UpdatedServerState) =:= [NewUser]).

should_get_empty_channels_for_a_newly_created_server_state_test() ->
	ServerState = server_state_create(),
	?assert(server_state_get_channels(ServerState) =:= []).

should_set_channels_to_server_state_test() ->
	ServerState = server_state_create(),
	Channel = server_state_create_channel("MyChannel"),
	AllChannels = [Channel],
	UpdatedServerState = server_state_set_channels(ServerState, AllChannels),
	?assert(server_state_get_channels(UpdatedServerState) =:= AllChannels).

should_add_channel_to_server_state_test() ->
	ServerState = server_state_create(),
	Channel = server_state_create_channel("MyChannel"),
	UpdatedServerState = server_state_add_channel(ServerState, Channel),
	?assert(server_state_get_channels(UpdatedServerState) =:= [Channel]).

%% Tests for Server business rules.
should_only_add_user_if_user_does_not_already_exists_test() ->
	User = {user_pid, 'Vincent'},
	ServerState = server_state_add_user(server_state_create(), User),
	?assert({error, user_already_exists} =:= may_add_user_to_server_state(ServerState, User)),
	?assert(length(server_state_get_users(ServerState)) =:= 1),
	NewUser = {new_user_pid, 'Sandra'},
	{ok, UpdatedServerState} = may_add_user_to_server_state(ServerState, NewUser),
	?assert(length(server_state_get_users(UpdatedServerState)) =:= 2). 

should_get_list_channels_names_test() ->
	ServerState = server_state_create(),
	Channel1 = server_state_create_channel("Channel 1"),
	Channel2 = server_state_create_channel("Channel 2"),	
	%% TODO: Use a set for managing channels? So when comparing lists we don't need take in account the ordering of the items?
	UpdatedServerState = server_state_add_channel(server_state_add_channel(ServerState, Channel1), Channel2),
	?assert(server_list_channel_names(UpdatedServerState) =:= ["Channel 2", "Channel 1"]). 

should_only_add_channel_if_channel_does_not_already_exists_test() ->
	%% TODO: Implement this.
	ok.

should_only_join_user_to_channel_if_user_does_not_already_joined_that_channel_test() ->
	%% TODO: Implement this.
	ok.

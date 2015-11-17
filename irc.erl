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
-export([start_server/0, server_loop/1, client/1, client_loop/1, connect/1, list/0, join/1, names/1, whois/1]).

-include_lib("eunit/include/eunit.hrl").

%% Change this to specify the node where the server runs.
%% TODO: This can be set in a different way at runtime using a config file or by command line argument when converting this to an Application.
-define(SERVER_NODE, 'server@vagrant-ubuntu-trusty-64').
-define(SERVER_INSTANCE_NAME, irc_server).
-define(CLIENT_INSTANCE_NAME, irc_client).

-record(user, {pid, name}).
-record(channel, {name, users = [] :: [user()]}).
-record(server_state, {users = [] :: [user()], channels = [] :: [channel()]}).
-record(whois_info, {user :: user(), channels = [] :: [channel()]}).

-opaque server_state() :: #server_state{}.
-opaque channel() :: #channel{}.
-opaque user() :: #user{}.
-opaque whois_info() :: #whois_info{}.

-export_type([server_state/0, channel/0, user/0, whois_info/0]).

%% @doc Start the server process and register it with known name.
start_server() ->
	Pid = spawn(irc, server_loop, [server_state_create()]),
	register(?SERVER_INSTANCE_NAME, Pid),
	error_logger:info_msg("Server > Created server process under name: ~p and pid: ~p~n", [?SERVER_INSTANCE_NAME, Pid]).

-spec server_loop(server_state()) -> any().
server_loop(ServerState) ->
	receive
		{From, connect, Nickname} ->
			error_logger:info_msg("Server > Received a connect message from ~s", [Nickname]),
		       	UpdatedServerState = server_handle_connect(From, ServerState, Nickname);
		{From, list} ->
			error_logger:info_msg("Server > Received a list message~n"),
			UpdatedServerState = server_handle_list(From, ServerState);
		{From, join, ChannelName} ->
			error_logger:info_msg("Server > Received a join(~s) message from(~p)", [ChannelName, From]),
			UpdatedServerState = server_handle_join(From, ServerState, ChannelName);
		{From, names, ChannelName} ->
			error_logger:info_msg("Server > Received a names(~s) message", [ChannelName]),
			UpdatedServerState = server_handle_names(From, ServerState, ChannelName);
		{From, whois, Nickname} ->
			error_logger:info_msg("Server > Received whois(~s) message", [Nickname]),
			UpdatedServerState = server_handle_whois(From, ServerState, Nickname)
	end,
	server_loop(UpdatedServerState).
-spec server_tell(identifier(), any()) -> any().
server_tell(To, Message) ->
	To ! {?SERVER_INSTANCE_NAME, Message}.


%% @doc Create a new server_state. Encapsulates server_state record creation.
-spec server_state_create() -> server_state().
server_state_create() -> #server_state{}.

-spec server_state_create_user(any(), string()) -> user().
server_state_create_user(Pid, Nickname) -> #user{pid=Pid, name=Nickname}.

-spec user_get_name(user()) -> string().
user_get_name(User) -> User#user.name.

-spec server_state_create_channel(string()) -> channel().
server_state_create_channel(ChannelName) ->
	server_state_create_channel(ChannelName, []).

-spec server_state_create_channel(string(), [user()]) -> channel().
server_state_create_channel(ChannelName, Users) ->
	#channel{name=ChannelName, users=Users}.

-spec server_state_create_whois_info(user(), [channel()]) -> whois_info().
server_state_create_whois_info(User, Channels) -> #whois_info{user=User, channels=Channels}.

-spec whois_info_get_user(whois_info()) -> user().
whois_info_get_user(WhoisInfo) -> WhoisInfo#whois_info.user.

-spec whois_info_get_channels(whois_info()) -> [channel()].
whois_info_get_channels(WhoisInfo) -> WhoisInfo#whois_info.channels.

%% @doc get users from server_state 
-spec server_state_get_users(server_state()) -> [user()].
server_state_get_users(#server_state{users = Users}) -> Users.

%% @doc set users on server_state
-spec server_state_set_users(server_state(), [user()]) -> server_state().
server_state_set_users(ServerState, UpdatedUsers) -> ServerState#server_state{users = UpdatedUsers}.

%% @doc add a user to the server_state
-spec server_state_add_user(server_state(), user()) -> server_state().
server_state_add_user(ServerState, User) ->
	UserList = server_state_get_users(ServerState),
	UpdatedUsers = [User | UserList],
	server_state_set_users(ServerState, UpdatedUsers).

%% @doc check if a user exists
-spec server_state_has_user(server_state(), user()) -> boolean().
server_state_has_user(ServerState, User) -> 
	Nickname = user_get_name(User),
	lists:any(fun (U) -> user_get_name(U) =:= Nickname end, server_state_get_users(ServerState)).

-spec find(fun( (_) -> boolean()), [any()]) -> any() | 'false'. 
find(_Pred, []) -> false; 
find(Pred, [H|Tail]) ->
	Result = Pred(H),
	case Result of
		true -> H;
		false -> find(Pred, Tail)
	end.

%% @doc get a user from the server_state record.
-spec server_state_find_user_by_sender(any(), server_state()) -> user() | 'false'. 
server_state_find_user_by_sender(Sender, ServerState) ->
	find(fun (X) -> X#user.pid == Sender end, server_state_get_users(ServerState)).

-spec user_name_match_predicate(string()) -> fun( (user()) -> boolean()).
user_name_match_predicate(Nickname) -> fun (User) -> user_get_name(User) =:= Nickname end.

-spec server_state_find_user_by_name(string(), server_state()) -> user() | 'false'.
server_state_find_user_by_name(Nickname, ServerState) ->
	find(user_name_match_predicate(Nickname), server_state_get_users(ServerState)).

-spec channels_find_by_name(string(), [channel()]) -> 'false' | channel(). 
channels_find_by_name(Name, Channels) ->
	Result = lists:filter(fun (X) -> X#channel.name =:= Name end, Channels),
	if
		length(Result) > 0 -> hd(Result);
		true -> false
	end.

-spec user_exists_in_channel(string(), channel()) -> boolean().
user_exists_in_channel(Nickname, Channel) ->
	lists:any(user_name_match_predicate(Nickname), channel_get_users(Channel)).

channels_find_all_by_user_name(Nickname, Channels) ->
	lists:filter(fun (Chan) -> user_exists_in_channel(Nickname, Chan) end, Channels).

-spec channel_get_name(channel()) -> string(). 
channel_get_name(Channel) -> Channel#channel.name.

%% @doc get channels from server_state
-spec server_state_get_channels(server_state()) -> [channel()].
server_state_get_channels(#server_state{channels = Channels}) -> Channels.

%% @doc set channels to server_state
-spec server_state_set_channels(server_state(), [channel()]) -> server_state().
server_state_set_channels(ServerState, UpdatedChannels) -> ServerState#server_state{channels = UpdatedChannels}.

-spec server_state_add_channel(server_state(), channel()) -> server_state().
server_state_add_channel(ServerState, Channel) ->
	Channels = server_state_get_channels(ServerState),
	UpdatedChannels = [Channel | Channels],
	server_state_set_channels(ServerState, UpdatedChannels).

-spec channel_get_users(channel()) -> [user()].
channel_get_users(#channel{users=Users}) -> Users.

-spec channel_set_users(channel(), [user()]) -> channel().
channel_set_users(Channel = #channel{}, UpdatedUsers) -> Channel#channel{users=UpdatedUsers}.

%% @doc add user to server_state if the user is not already registered. This is business logic. 
%% TODO: Get a better name, like may_connect_user?
-spec may_add_user_to_server_state(server_state(), user()) -> {ok, server_state()} | {error, user_alredy_exists}.
may_add_user_to_server_state(ServerState, User) -> 
	case server_state_has_user(ServerState, User) of
		false -> {ok, server_state_add_user(ServerState, User)};
	      	true -> {error, user_already_exists}
	end.

-spec may_list_names_in_channel(string(), server_state()) -> [string()].
may_list_names_in_channel(ChannelName, ServerState) ->
	%% TODO: Return false if the channel does not exists.
	Channel = server_state_channels_find_by_name(ChannelName, ServerState),
	Users = channel_get_users(Channel),
	lists:map(fun (U) -> user_get_name(U) end, Users).

-spec server_state_channels_find_by_name(string(), server_state()) -> channel().
server_state_channels_find_by_name(ChannelName, ServerState) ->
	channels_find_by_name(ChannelName, server_state_get_channels(ServerState)).

-spec server_state_channels_add(channel(), server_state()) -> server_state().
server_state_channels_add(Channel, ServerState) ->
	Channels = server_state_get_channels(ServerState),
	UpdatedChannels = channels_add(Channel, Channels),
	server_state_set_channels(ServerState, UpdatedChannels).

%% @doc List the all the channel names in the server
-spec server_list_channel_names(server_state()) -> [string()].
server_list_channel_names(ServerState) ->
	Channels = server_state_get_channels(ServerState),
	lists:map(fun channel_get_name/1, Channels).

-spec channels_remove(channel(), [channel()]) -> [channel()].
channels_remove(Channel, Channels) -> Channels -- [Channel].

-spec channels_add(channel(), [channel()]) -> [channel()].
channels_add(Channel, Channels) -> [Channel | Channels ].

%% TODO: Reverse parameters.
-spec channel_add_user(channel(), user()) -> channel().
channel_add_user(Channel, User) ->
	UserList = channel_get_users(Channel),
	UpdatedUsers = [User | UserList],
	channel_set_users(Channel, UpdatedUsers).

%% Separate logic into Server Message Handling, Data Structure(Model), Business Rules.

%% TODO: Refactor this to be more readable.
-spec add_channel_and_user_to_server_state(server_state(), string(), user()) -> server_state(). 
add_channel_and_user_to_server_state(ServerState, ChannelName, User) ->
	Channels = server_state_get_channels(ServerState),
	case channels_find_by_name(ChannelName, Channels) of
		false ->
			NewChannel = server_state_create_channel(ChannelName, [User]),
			server_state_add_channel(ServerState, NewChannel);
		Channel ->
			OtherChannels = channels_remove(Channel, Channels),
			UpdatedChannel = channel_add_user(Channel, User),
			UpdatedChannels = channels_add(UpdatedChannel, OtherChannels),
			server_state_set_channels(ServerState, UpdatedChannels)
	end.

-spec may_get_user_whois_info(server_state(), string()) -> whois_info().
may_get_user_whois_info(ServerState, Nickname) ->
	Channels = channels_find_all_by_user_name(Nickname, server_state_get_channels(ServerState)),
	User = server_state_find_user_by_name(Nickname, ServerState),
	server_state_create_whois_info(User, Channels).

server_handle_whois(Sender, ServerState, Nickname) ->
	WhoisInfo = may_get_user_whois_info(ServerState, Nickname),
	% TODO: Handle situation when the user does not exists.
	server_tell(Sender, {whois_response, WhoisInfo}),
	ServerState.

%% @doc Handle a connect message
server_handle_connect(Sender, ServerState, Nickname) ->
	%% TODO: Create different function for handling ok and error situations to remove the case?
	case may_add_user_to_server_state(ServerState, server_state_create_user(Sender, Nickname)) of
		{ok, UpdatedServerState} ->
			server_tell(Sender, connected),
			UpdatedServerState;
		{error, user_already_exists} ->
			server_tell(Sender, {stop, user_exists_at_other_node}),
			ServerState
	end.

server_handle_list(Sender, ServerState) ->
	ChannelNames = server_list_channel_names(ServerState),
	server_tell(Sender, {list_response, ChannelNames}),
	ServerState.

server_handle_names(Sender, ServerState, ChannelName) ->
	NamesInChannel = may_list_names_in_channel(ChannelName, ServerState),	
	server_tell(Sender, {names_response, NamesInChannel}),
	ServerState.

server_handle_join(Sender, ServerState= #server_state{}, ChannelName) ->
	% TODO: Do not join the user two times at the same channel.
	User = server_state_find_user_by_sender(Sender, ServerState),
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

-spec names(string()) -> any().
names(ChannelName) ->
	send_if_connected({names, ChannelName}).

send_if_connected(Message) ->
	case whereis(?CLIENT_INSTANCE_NAME) of
		undefined ->
			not_logged_on;
		_ -> ?CLIENT_INSTANCE_NAME ! Message
	end.

join(ChannelName) ->
	send_if_connected({join, ChannelName}).

whois(Nickname) ->
	send_if_connected({whois, Nickname}).

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

%% @doc client_loop forwards messages to the server
client_loop(Nickname) ->
	receive
		{join, ChannelName} ->
			error_logger:info_msg("Client > Sending join(~s) command to server", [ChannelName]),
			{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), join, ChannelName},
			await_result();
		{names, ChannelName} ->
			{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), names, ChannelName},
			await_result();
		{whois, Nickname} ->
			{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), whois, Nickname},
			await_result()
	end,
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
	AllUsers = [server_state_create_user(user_pid, "Mike"), server_state_create_user(user_pid, "Jonhhy")],
	UpdatedServerState = server_state_set_users(ServerState, AllUsers),
	?assert(server_state_get_users(UpdatedServerState) =:= AllUsers).

should_add_user_to_server_state_test() ->
	ServerState = server_state_create(),
	NewUser = server_state_create_user(user_pid, "Emma"),
	UpdatedServerState = server_state_add_user(ServerState, NewUser),
	?assert(server_state_get_users(UpdatedServerState) =:= [NewUser]).

should_check_for_existing_user_on_server_state_test() ->
	ServerState = server_state_create(),
	NewUser = server_state_create_user(user_pid, "Bryan"),
	UpdatedServerState = server_state_add_user(ServerState, NewUser),
	?assert(server_state_has_user(UpdatedServerState, NewUser) =:= true).

should_find_user_by_nickname_test() ->
	ServerState = server_state_create(),
	User = server_state_create_user(user_pid, "John"),
	UpdatedServerState = server_state_add_user(ServerState, User),
	?assert(server_state_find_user_by_name("John", UpdatedServerState) =:= User).

should_find_user_by_sender_test() ->
	ServerState = server_state_create(),
	User = server_state_create_user(unique_user_identifier, "Rita"),
	UpdatedServerState = server_state_add_user(ServerState, User),
	?assert(server_state_find_user_by_sender(unique_user_identifier, UpdatedServerState) =:= User).

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

should_find_channel_by_name_test() ->
	ServerState = server_state_create(),
	Channel = server_state_create_channel("MyChannel"),
	UpdatedServerState = server_state_add_channel(ServerState, Channel),
	?assert(server_state_channels_find_by_name("MyChannel", UpdatedServerState) =:= Channel).

%% Tests for Server business rules.
should_only_add_user_if_user_does_not_already_exists_test() ->
	User = server_state_create_user(user_pid, "Vincent"),
	ServerState = server_state_add_user(server_state_create(), User),
	?assert({error, user_already_exists} =:= may_add_user_to_server_state(ServerState, User)),
	?assert(length(server_state_get_users(ServerState)) =:= 1),
	NewUser = server_state_create_user(new_user_pid, "Sandra"),
	{ok, UpdatedServerState} = may_add_user_to_server_state(ServerState, NewUser),
	?assert(length(server_state_get_users(UpdatedServerState)) =:= 2). 

should_get_list_of_channels_names_test() ->
	ServerState = server_state_create(),
	Channel1 = server_state_create_channel("Channel 1"),
	Channel2 = server_state_create_channel("Channel 2"),	
	%% TODO: Use a set for managing channels? So when comparing lists we don't need to take in account the ordering of the items?
	UpdatedServerState = server_state_add_channel(server_state_add_channel(ServerState, Channel1), Channel2),
	?assert(server_list_channel_names(UpdatedServerState) =:= ["Channel 2", "Channel 1"]). 

should_get_list_of_names_of_users_in_channel_test() ->
	User = server_state_create_user(user_pid, "Gabriel"),
	ChannelName = "Channel 1",
	Channel1 = server_state_create_channel(ChannelName, [User]),
	ServerState = server_state_add_channel(server_state_create(), Channel1),
	?assert(may_list_names_in_channel(ChannelName, ServerState) =:= ["Gabriel"]).

should_get_user_whois_info_from_server_state_test() ->
	ServerState = server_state_create(),
	User = server_state_create_user(user_pid, "Homer"),
	Channel1 = server_state_create_channel("Some_Channel", [User]),
	Channel2 = server_state_create_channel("Other_Channel", [User]),
	UpdatedServerState = server_state_add_user(ServerState, User),
	AllChannels = [Channel1, Channel2],
	FinalServerState = server_state_set_channels(UpdatedServerState, AllChannels),
	WhoisInfo = may_get_user_whois_info(FinalServerState, "Homer"),
	?assert(whois_info_get_user(WhoisInfo) =:= User),
	?assert(whois_info_get_channels(WhoisInfo) =:= AllChannels).

should_only_add_channel_if_channel_does_not_already_exists_test() ->
	%% TODO: Implement this.
	ok.

should_only_join_user_to_channel_if_user_does_not_already_joined_that_channel_test() ->
	%% TODO: Implement this.
	ok.

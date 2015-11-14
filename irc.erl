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
-export([start_server/0, server_loop/1, client/1, client_loop/1,connect/1]).

%% Change this to specify the node where the server runs.
%% TODO: This can be set in a different way at runtime using a config file or by command line argument when converting this to an Application.
-define(SERVER_NODE, 'server@vagrant-ubuntu-trusty-64').
-define(SERVER_INSTANCE_NAME, irc_server).
-define(CLIENT_INSTANCE_NAME, irc_client).

%% @doc Start the server process and register it with known name.
start_server() ->
	error_logger:info_msg("Server > Creating server process under name: ~s~n", [?SERVER_INSTANCE_NAME]),
	register(?SERVER_INSTANCE_NAME, spawn(irc, server_loop, [[]])).

server_loop(User_List) ->
	receive
		{From, connect, Nickname} ->
			error_logger:info_msg("Server > Received a connect message from ~s", [Nickname]),
		       	Updated_User_List = server_handle_connect(From, User_List, Nickname)
	end,
	server_loop(Updated_User_List).

server_handle_connect(From, User_List, Nickname) ->
	From ! { ?SERVER_INSTANCE_NAME, connected}, 
	[Nickname | User_List].

connect(Nickname) ->
	case whereis(?CLIENT_INSTANCE_NAME) of
		undefined ->
			error_logger:info_msg("Client > Creating client process under name: ~s~n", [?CLIENT_INSTANCE_NAME]),
		       	register(?CLIENT_INSTANCE_NAME, spawn(irc, client, [Nickname]));
		_ -> 
			error_logger:error_msg("Client > Client process under name ~s already exists.~n", [?CLIENT_INSTANCE_NAME]),
		   	already_connected
	end.

client(Nickname) ->
	error_logger:info_msg("Client > Sending connect command to server using Nickname ~s", [Nickname]),
	{?SERVER_INSTANCE_NAME, ?SERVER_NODE} ! {self(), connect, Nickname},
	await_result(),
	client_loop(Nickname).

await_result() ->
	receive
		{?SERVER_INSTANCE_NAME, stop, Why} ->
			error_logger:error_msg("Client > Server says to stop. Exiting because of ~p~n", [Why]),
			exit(normal);
		{?SERVER_INSTANCE_NAME, What} ->
			error_logger:info_msg("Client > Received ~p from server~n", [What])
	end.

client_loop(Nickname) ->
	client_loop(Nickname).


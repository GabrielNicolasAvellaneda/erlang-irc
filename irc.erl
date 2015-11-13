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
-export([start_server/0, server_loop/1]).

%% Change this to specify the node where the server runs.
%% TODO: This can be set in a different way at runtime using a config file or by command line argument when converting this to an Application.
-define(SERVER_NODE, 'server@vagrant-unbuntu-trusty-64').
-define(SERVER_INSTANCE_NAME, irc_server).

%% @doc Start the server process and register it with known name.
start_server() ->
	error_logger:info_msg("Creating server process under name: ~s~n", [?SERVER_INSTANCE_NAME]),
	register(?SERVER_INSTANCE_NAME, spawn(irc, server_loop, [[]])).

server_loop(User_List) ->
	server_loop(User_List).



%% A Minimal IRC with plain Erlang
%%
%% User Interface:
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
-export([server_start/0]).

server_start() ->
	ok.


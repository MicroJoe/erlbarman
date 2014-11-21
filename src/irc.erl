-module(irc).

-compile(export_all).

% Wrap an element inside of a list if it is not already inside one.
%
% This is used in this module because we do not know if user if passing
% arguments as lists or as a single entity but all our functions are based on
% lists.
%
% The simplest thing to do is to wrap element into a list in order to let the
% user give non-list-wrapped arguments to this module's functions.
wrap(Elt) ->
  case Elt of
    [_] -> Elt;
    _ -> [Elt]
  end.

% Send an IRC command on the socket
command(Sock, Cmd) ->
  gen_tcp:send(Sock, lists:append([Cmd, [<<13, 10>>]])).

% Send a PRIVMSG IRC command on the socket
%
% Arguments:
%   Dest -> Target of the PRIVMSG (user or #channel)
%   Message -> The message to send
%
privmsg(Sock, Dest, Message) ->
  LDest = wrap(Dest),
  Cmd = lists:append([[<<"PRIVMSG ">>], LDest, [<<" :">>], Message]),
  command(Sock, Cmd).

% CTCP command
%
% A CTCP command is a PRIVMSG surrounded with 0x01 ASCII bytes.
%
ctcp(Sock, Dest, Cmd) ->
  privmsg(Sock, Dest, lists:append([[<<1>>], Cmd, [<<1>>]])).

% ACTION CTCP command
action(Sock, Dest, Msg) ->
  ctcp(Sock, Dest, lists:append([[<<"ACTION ">>], Msg])).

-module(barman).
-behavior(application).

-compile(export_all).

% -export([start/2, stop/1]).

start(_Type, _Args) ->
  client().

stop(_State) ->
  ok.

% Server params
-define(SERVER_HOST, "irc.smoothirc.net").
-define(SERVER_PORT, 6667).

% Bot params
-define(NICK, <<"erlbot">>).
-define(REALNAME, <<"Erlang Barman Bot by MicroJoe">>).
-define(QUIT, ?REALNAME).

client() ->
  % Connect to the server
  {ok, Sock} = gen_tcp:connect(?SERVER_HOST, ?SERVER_PORT,
                               [binary, {active, false}, {packet, 0}]),

  % Send authentification stuff
  ok = command(Sock, [<<"NICK ">>, ?NICK]),
  ok = command(Sock, [<<"USER ">>, ?NICK, <<" 0 * :">>, ?REALNAME]),

  % Spawn read, prompt and write loops
  spawn(?MODULE, loop_recv, [Sock]),
  spawn(?MODULE, loop_prompt, [self(), Sock]),

  % We wait the stop message from anyone
  receive
    stop ->
      io:format("Goodbye~n"),
      gen_tcp:close(Sock),
      true
  end.

% Send an IRC command to the socket
command(Sock, Cmd) ->
  gen_tcp:send(Sock, lists:append([Cmd, [<<13, 10>>]])).

privmsg(Sock, Message) ->
  gen_tcp:send(Sock, lists:append([[<<"PRIVMSG #erlbot :">>], Message, [<<13, 10>>]])).

% CTCP stuff
ctcp(Sock, Cmd) ->
  privmsg(Sock, lists:append([[<<1>>], Cmd, [<<1>>]])).

action(Sock, Msg) ->
  ctcp(Sock, lists:append([[<<"ACTION ">>], Msg])).

% Basic prompt in order to send raw IRC commands and to quit the program
loop_prompt(Pid, Sock) ->
  Input = io:get_line("irc> "),
  Cmd = list_to_binary(Input),
  case Cmd of
    <<"exit\n">> ->
      Pid ! stop,
      command(Sock, [<<"QUIT ">>, ?QUIT]),
      closed;
    <<"action\n">> ->
      action(Sock, [<<"sert un jus.">>]),
      loop_prompt(Pid, Sock);
    _ ->
      command(Sock, [Cmd]),
      loop_prompt(Pid, Sock)
  end.

% Writing loop
loop_write(Sock) ->
  receive
    stop -> true
  end,
  loop_write(Sock).

% Reading loop
loop_recv(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Packet} ->
      io:format("~ts", [Packet]),
      loop_recv(Sock);
    {error, closed} ->
      closed
  end.

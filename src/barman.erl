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
-define(CHAN, <<"#erlbot">>).

-define(EOL, <<13, 10>>).


client() ->
  % Connect to the server
  {ok, Sock} = gen_tcp:connect(?SERVER_HOST, ?SERVER_PORT,
                               [binary, {active, false}, {packet, 0}]),

  % Send authentification stuff
  ok = irc:command(Sock, [<<"NICK ">>, ?NICK]),
  ok = irc:command(Sock, [<<"USER ">>, ?NICK, <<" 0 * :">>, ?REALNAME]),

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

% Basic prompt in order to send raw IRC commands and to quit the program
loop_prompt(Pid, Sock) ->
  Input = io:get_line("irc> "),
  Cmd = list_to_binary(Input),
  case Cmd of
    <<"exit\n">> ->
      Pid ! stop,
      irc:command(Sock, [<<"QUIT ">>, ?QUIT]),
      closed;
    <<"action\n">> ->
      irc:action(Sock, ?CHAN, [<<"sert un jus ">>, jus:choose_fruit(), <<".">>]),
      loop_prompt(Pid, Sock);
    _ ->
      irc:command(Sock, [Cmd]),
      loop_prompt(Pid, Sock)
  end.

handle_recv(Sock, Line) ->
    case binary:split(Line, <<" ">>, [global, trim]) of
        [_Host, <<"MODE">>, _Nick, <<"+x">>] ->
            io:format("Identified~n"),
            io:format("Joining ~s...~n", [?CHAN]),
            irc:command(Sock, [<<"JOIN ">>, ?CHAN]);
        [<<"PING">>, From] ->
            io:format("Pong back to ~s~n", [From]),
            irc:command(Sock, [<<"PONG">>]),
            pong;
        [Host, <<"PRIVMSG">>, Chan, <<":!jus">>] ->
            action_jus(Sock, Chan, irc:nick_from_host(Host));
        [_Host, <<"PRIVMSG">>, Chan, <<":!jus">> | Recipients] ->
            lists:map(fun(Dest) -> action_jus(Sock, Chan, Dest) end, Recipients);
        _ ->
            {not_handled, Line}
    end.

action_jus(Sock, Chan, Dest) ->
    io:format("Sert un jus à ~s sur ~s~n", [Dest, Chan]),
    irc:action(Sock, Chan, jus:jus(Dest)).

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
      % io:format("~ts", [Packet]),
      % We split the whole packet in lines
      Lines = binary:split(Packet, ?EOL, [global, trim]),
      % For each line in the packet we handle it with dedicated function
      lists:map(fun(L) -> handle_recv(Sock, L) end, Lines),
      loop_recv(Sock);
    {error, closed} ->
      closed
  end.

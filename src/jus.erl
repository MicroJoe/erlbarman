-module(jus).
-compile(export_all).

% TODO: Handle unicode

-define(CHOICES, [<<"d’orange">>, <<"de citron">>]).

readlines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  binary:split(Data, [<<"\n">>], [global]).

choose(List) ->
  lists:nth(random:uniform(length(List)), List).

choose_fruit() ->
  choose(readlines("data/jus.txt")).
  % choose(?CHOICES).

jus(Dest) ->
  [<<"sert un jus ">>, choose_fruit(), <<" à ">>, Dest, <<".">>].

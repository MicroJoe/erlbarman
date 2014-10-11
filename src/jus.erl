-module(jus).
-compile(export_all).

% TODO: Handle unicode

-define(CHOICES, [<<"d'orange">>, <<"de citron">>]).

choose(List) ->
    lists:nth(random:uniform(length(List)), List).

choose_fruit() ->
    choose(?CHOICES).

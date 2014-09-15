# Erlang Barman IRC bot

A small IRC bot written in Erlang, still WIP.

## Description

This is a small IRC bot attempt written in Erlang, which may be used someday in
order to replace the Python version of a bot I wrote in Python using Twisted.

## Running

Since I did not really found an easy to use build system for Erlang I usually
start the bot using the Erlang REPL:

    :::console
    $ cd src/
    $ erl
    > c(barman).
    > barman:client().
    irc>

In order to exit the bot you have to type `exit` and in order to quit the
Erlang interactive shell you have to call `init:stop().` and it will go away
after a short time.

## License

This program is brought to you under WTFPL. For further informations please
read the provided COPYING file.

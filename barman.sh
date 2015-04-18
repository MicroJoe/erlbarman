#!/bin/sh

# Run the barman client function and then exit the Erlang VM when closed
erl -pa ebin/ +fnu +pc unicode -noshell \
	-run barman client \
	-run init stop

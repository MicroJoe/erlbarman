all:
	mkdir -p ebin
	erl -make

shell:
	erl -pa ebin/ +pc unicode

.PHONY: shell

all:
	mkdir -p ebin
	erl -make

shell:
	erl -pa ebin/

.PHONY: shell

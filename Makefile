BUILDDIR=ebin/
ERLRUN=erl -pa $(BUILDDIR) +pc unicode

.PHONY: shell run

all:
	mkdir -p $(BUILDDIR)
	erl -make

# Start an interactive Erlang shell
shell:
	$(ERLRUN)


clean:
	rm -rf $(BUILDDIR)

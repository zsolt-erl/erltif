all: compile

compile:
	./rebar compile

clean:
	./rebar clean

.PHONY: all compile clean


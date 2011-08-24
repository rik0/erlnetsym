all: compile

compile:
	rebar compile

run: compile
	./run

clean:
	rebar clean


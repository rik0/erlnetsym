.PHONY: tags compile clean all run

all: compile tags

compile: tags
	rebar compile

run: compile
	./run

clean:
	rebar clean

tags:
	ctags -e -R -f TAGS .
## ctags -R .





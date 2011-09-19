.PHONY: tags compile clean all run doc

all: compile tags doc

compile: tags
	rebar compile

run: compile
	./run

clean:
	rebar clean

tags:
	ctags -e -R -f TAGS .
## ctags -R .

doc:
	rebar doc





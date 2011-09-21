.PHONY: tags compile clean all run doc

all: compile tags doc

compile: ensy

ensy: tags
	(cd lib/ensy; rebar compile)

run: compile
	./run.sh

clean:
	rebar clean

tags:
	ctags -e -R -f TAGS .
## ctags -R .

doc: ensy_doc

ensy_doc:
	(cd lib/ensy; rebar doc)





.PHONY: tags compile clean all run doc

all: build tags doc

build: compile release

release:
	pbin/build_release.erl

compile: ensy

ensy: tags
	(cd lib/ensy; rebar compile)

clean:
	rebar clean

tags:
	ctags -e -R -f TAGS .
## ctags -R .

doc: ensy_doc

ensy_doc:
	(cd lib/ensy; rebar doc)





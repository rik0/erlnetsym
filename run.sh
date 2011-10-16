#!/bin/sh

USAGE="usage: $0 config_name [--steps num] [--init opts...]"
CONFIG=$1


if  [ $# -eq 0 ]; then
	echo $USAGE
fi
	

shift # remove the config name
erl -boot ./releases/erlnetsym -config config/"$CONFIG" -noshell -detached


#while [ $# -gt 0 ]
#do
#    case "$1" in
#        -v) vflag=on;;
#		-f) filename="$2"; shift;;
#		-*) echo >&2 usage: $0 [-v] [-f file] [file ...]"
#	    	exit 1;;
#		*)  break;;	# terminate while loop
#    esac
#    shift
#done
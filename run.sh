#!/bin/sh

ACTIVATOR=$1;

shift;

echo $ACTIVATOR
echo $@

erl \
	-s application start ensy permanent \
    -init_debug \
	-smp enable \
	-pa lib/ensy/ebin  \
	-noinput \
    -boot start_sasl \
	-ensy activator_stub ensy_preferential_attachment \
    1
	#-detached
	#-sasl sasl_error_logger '{file, "ensy.log"}' \
	# -ensy activator_stub ensy_preferential_attachment
	#-detached
		#	-sasl errlog_type all \
	#-boot start_sasl \

	

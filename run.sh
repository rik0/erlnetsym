#!/bin/sh

ACTIVATOR=$1;

shift;

echo $ACTIVATOR
echo $@

erl \
	-smp enable \
	-pa ebin  \
	-noinput \
	-ensy activator_stub ensy_preferential_attachment \
	-run application start ensy
	#-detached
	#-sasl sasl_error_logger '{file, "ensy.log"}' \
	# -ensy activator_stub ensy_preferential_attachment
	#-detached
		#	-sasl errlog_type all \
	#-boot start_sasl \

	
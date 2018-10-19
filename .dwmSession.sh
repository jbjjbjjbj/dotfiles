#!/bin/bash
PIDFILE=/home/julian/.dwmSession.pid

kill `cat $PIDFILE`

if [ "$1" = "-e" ]; then
	exit
fi

echo $$ > $PIDFILE

while true; do

	BATT=$( acpi -b | sed 's/.*[charging|unknown], \([0-9]*\)%.*/\1/gi' )
	TIME=$(/bin/date +"%H:%M")

	xsetroot -name "$TIME $BATT%"
	sleep 10
done

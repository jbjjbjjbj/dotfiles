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
	IP=$(ip -4 a | grep "inet " | sed "s:inet \(.*\)/.*:\1:" | tr -d " " | tr "\n" " ")

	xsetroot -name "$IP $TIME $BATT%"
	sleep 10
done

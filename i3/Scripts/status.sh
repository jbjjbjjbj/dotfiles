PIDFILE=/tmp/stat.pid
STATUSFILE=/tmp/dsbMain

kill `cat $PIDFILE`
if [ $? -eq 0 ]; then
	echo "Shutting down"
	rm $STATUSFILE
	exit
fi

echo $$ > $PIDFILE

while true; do

	BATT=$( acpi -b | sed 's/.*[charging|unknown], \([0-9]*\)%.*/\1/gi' )
	TIME=$(/bin/date +"%H:%M")

	echo "$IP $TIME $BATT%" > $STATUSFILE
	sleep 10
done

PIDFILE=/tmp/statRam.pid
STATUSFILE=/tmp/dsbRfree

kill `cat $PIDFILE`
if [ $? -eq 0 ]; then
	echo "Shutting down"
	rm $STATUSFILE
	exit
fi

echo "Starting up"

echo $$ > $PIDFILE

while true; do
	echo " $(free -h | grep Mem: | awk '{print $3}') " > $STATUSFILE
	sleep 2
done


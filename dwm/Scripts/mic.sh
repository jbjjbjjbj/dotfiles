PIDFILE=/tmp/statMic.pid
STATUSFILE=/tmp/dsbAMic

kill `cat $PIDFILE`
if [ $? -eq 0 ]; then
	echo "Shutting down"
	rm $STATUSFILE
	exit
fi

echo $$ > $PIDFILE

while true; do

	MIC=$(amixer get Capture | grep "Front Left:" | sed 's/.*\[\(.*\)\].*/\1/')

	echo "$MIC " > $STATUSFILE
	sleep 1
done

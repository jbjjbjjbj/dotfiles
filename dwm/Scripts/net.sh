PIDFILE=/tmp/statNet.pid
STATUSFILE=/tmp/dsbANet

kill `cat $PIDFILE`
if [ $? -eq 0 ]; then
	echo "Shutting down"
	rm $STATUSFILE
	exit
fi

echo $$ > $PIDFILE

while true; do

	IP=$( ip -4 a | grep "inet " | sed "s:inet \(.*\)/.*:\1:" | tr -d " " | grep -v "0.1" | tr "\n" " ")

	echo "$IP " > $STATUSFILE
	sleep 10
done

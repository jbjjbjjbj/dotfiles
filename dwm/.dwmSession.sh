!/bin/bash
PIDFILE=/home/julian/.dwmSession.pid

kill `cat $PIDFILE`

if [ "$1" = "-e" ]; then
	exit
fi

echo $$ > $PIDFILE

for file in /home/julian/Scripts/enMenuScripts/*
do
	sh $file &
done

while true; do
	xsetroot -name "$(cat /tmp/dsb* | tr -d '\n')"
	sleep 1
	inotifywait -q -e modify /tmp/dsb*
done

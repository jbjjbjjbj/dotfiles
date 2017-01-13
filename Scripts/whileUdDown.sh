C=1
RED=$(curl -Ls -o /dev/null -w %{url_effective} http://uddataplus.dk)
echo "$C  $RED"
COM="https://www.uddata.dk/uddataplus-nede/"
while [ "$RED" == "$COM" ]
do
	C=$((C+1))
	RED=$(curl -Ls -o /dev/null -w %{url_effective} http://uddataplus.dk)
	echo "$C  $RED"
	sleep 30
done
notify-send 'Yo!' 'Uddata virker måske igen' --icon=dialog-information

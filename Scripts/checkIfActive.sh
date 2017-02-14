if pgrep "$1" > /dev/null; then
	echo Active
else
	echo Inactive
fi

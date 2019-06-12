elements="ram.sh status.sh mic.sh"

if [ $1 = "dmenu" ]; then
	sh $(echo $elements | tr " " "\n" | dmenu -i -p ":")
fi


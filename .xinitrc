setxkbmap -layout us -variant altgr-intl
xcape -e "Control_L=Escape"
feh --bg-fill /home/julian/Pictures/up1.*
export _JAVA_AWT_WM_NONREPARENTING=1
export TERM=st

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)
export SSH_AUTH_SOCK

xmodmap .Xmodmap

nm-applet &
xscreensaver &
xss-lock -- xscreensaver-command --lock &
dunst &

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi

while true; do 
	dwm
done &


exec sh /home/julian/.dwmSession.sh

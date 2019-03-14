setxkbmap -layout us -variant altgr-intl
feh --bg-fill /home/julian/Pictures/defaultWall.*
export _JAVA_AWT_WM_NONREPARENTING=1
export TERM=st

dbus-update-activation-environment --systemd DISPLAY

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)
export SSH_AUTH_SOCK

xmodmap .Xmodmap
xcape -e "Control_R=Escape"

nm-applet &
xscreensaver &
xss-lock -- xscreensaver-command --lock &
dunst &
xcompmgr &

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

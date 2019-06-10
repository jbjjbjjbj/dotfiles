setxkbmap -layout us -variant altgr-intl
export _JAVA_AWT_WM_NONREPARENTING=1
feh --bg-tile /home/julian/Pictures/defaultWall.*
export TERM=st

dbus-update-activation-environment --systemd DISPLAY

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

xmodmap .Xmodmap
xcape -e "Control_R=Escape"

nm-applet &
xscreensaver &
xss-lock -- xscreensaver-command --lock &
xcompmgr &

while true; do 
	dwm
done &

play ~/Winsounds/login.wav -q &

exec sh /home/julian/.dwmSession.sh

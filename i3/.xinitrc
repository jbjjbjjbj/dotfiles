setxkbmap -layout us -variant altgr-intl
export _JAVA_AWT_WM_NONREPARENTING=1
feh --bg-tile /home/julian/Pictures/defaultWall.*
export TERM=st
export TERMINAL=st

dbus-update-activation-environment --systemd DISPLAY

export SSH_AUTH_SOCK=${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)

xmodmap .Xmodmap
xcape -e "Control_R=Escape"
dunst &

nm-applet &
xscreensaver &
xss-lock -- xscreensaver-command --lock &
xcompmgr &

play ~/Winsounds/login.wav -q &

exec i3

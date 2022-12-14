
export LOPTS="panel composer gnome-keyring"
# Launch other stuff
source ~/.xprofile
source ~/Scripts/xlaunchrc

sxhkd &
exec bspwm

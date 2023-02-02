
export LOPTS="panel composer gnome-keyring jpinput"
# Launch other stuff
source ~/.xprofile
source ~/Scripts/xlaunchrc

sxhkd &
exec bspwm

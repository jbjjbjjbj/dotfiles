
export LOPTS="panel composer gnome-keyring jpinput"
export WALLCONFIG="$HOME/Pictures/Rand/walls/config.yaml"
# Launch other stuff
source ~/.xprofile
source ~/Scripts/xlaunchrc

sxhkd &
exec bspwm

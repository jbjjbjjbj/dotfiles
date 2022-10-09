
export LOPTS="screensaver composer gnome-keyring"
# Launch other stuff
source ~/.xprofile
source ~/Scripts/xlaunchrc

#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 2 --height 18 --transparent true --tint 0x000000 --alpha 0 &
exec i3

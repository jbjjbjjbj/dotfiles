
export LOPTS="panel wallpaper composer gnome-keyring jpinput"
export WALLCONFIG="$HOME/Pictures/rand/walls/config.yaml"
# Launch other stuff
source ~/.xprofile
source ~/Scripts/xlaunchrc

# sxhkd &
exec xmonad 2>&1 | tee /tmp/vm.log

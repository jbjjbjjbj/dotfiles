
tomnt=$( cat /etc/fstab | grep mnt | sed "s:.* \(/mnt/[^ ]*\) .*:\1:g" | dmenu )

mount $tomnt 
st -e ranger $tomnt
sync
umount $tomnt

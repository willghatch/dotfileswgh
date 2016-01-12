# blink icon when there are new messages

dir=$HOME/.config/gajim
mkdir -p $dir
cat $dir/config | grep -v remote_control > $dir/config.new
echo "remote_control=True" >>$dir/config.new
mv $dir/config.new $dir/config

# any after will only work while gajim is running, after remote_control is enabled

gajim-remote prefs_put trayicon_blink=False


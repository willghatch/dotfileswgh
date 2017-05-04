# run the REAL xinitrc with dbus-launch
export LOGIN_TTY=$(tty)
exec dbus-launch --exit-with-session $DOTFILESWGH/xinitrc-post-dbus.sh


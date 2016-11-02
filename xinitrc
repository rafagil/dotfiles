xrdb -merge ~/.Xresources

autorandr --change

xsetroot -cursor_name left_ptr -solid '#002b36'

if [ -x /usr/bin/ssh-agent ] ; then
  eval `ssh-agent -s`
  fi

  setxkbmap -option caps:escape
  xmodmap ~/.Xmodmap
  xset r rate 250 50

  xautolock -time 10 -locker "i3lock -c 000000" &

  dunst &
  udiskie &

  exec xmonad

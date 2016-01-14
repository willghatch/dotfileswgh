#!/usr/bin/env bash

$DOTFILESWGH/external/misc/kbd-state-mon/kbd-state-mon -w | while read line
do
    echo "kbd_state_widget:set_markup(\"<span color='#6fff3f'>$line</span>\")" | awesome-client
done


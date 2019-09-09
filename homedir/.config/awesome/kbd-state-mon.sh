#!/usr/bin/env bash

ksm=$(which kbd-state-mon)
ksm=${ksm:-$DOTFILESWGH/external/misc/kbd-state-mon/kbd-state-mon}

$ksm -w | while read line
do
    echo "kbd_state_widget:set_markup(\"<span color='#6fff3f'>$line</span>\")" | awesome-client
done


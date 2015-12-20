#!/usr/bin/env bash

$DOTFILESWGH/external/misc/kbd-state-mon/kbd-state-mon -w | while read line
do
    echo "kbd_state_widget:set_text(\"$line\")" | awesome-client
done


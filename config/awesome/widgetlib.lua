local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")

local math = require("math")
mytextclock = awful.widget.textclock()


kbd_state_widget = wibox.widget.textbox()
kbd_state_widget:set_text("")

get_unread_count = function()
    local io = { popen = io.popen }
    local instr = io.popen("unread-hatchmail-count all")
    local incr = function(v, n)
       n = n or 0
       n = tonumber(n)
       n = math.floor(n)
       return v + n
    end
    local inboxy = 0
    local spammy = 0
    local mach_school = 0
    local racket = 0
    local lists = 0
    local top_feeds = 0
    local rfeeds = 0

    for line in instr:lines() do
       inboxy = incr(inboxy, line:match("INBOX (%d+)"))
       spammy = incr(spammy, line:match("maybe.spam (%d+)"))
       spammy = incr(spammy, line:match("Spam (%d+)"))
       mach_school = incr(mach_school, line:match("school.lists (%d+)"))
       mach_school = incr(mach_school, line:match("machines (%d+)"))
       racket = incr(racket, line:match("racket (%d+)"))
       lists = incr(lists, line:match("misc.lists (%d+)"))
       top_feeds = incr(top_feeds, line:match("^feeds (%d+)"))
       rfeeds = incr(rfeeds, line:match("many.feeds (%d+)"))
    end
    local green = "<span color='green'>"
    local cc = "<span color='#99ff66'>"
    local ecc = "</span>"
    instr:close()
    return green ..
       "IN ".. cc .. inboxy .. ecc .." "..
       "MS ".. cc .. mach_school .. ecc .." "..
       "R ".. cc .. racket .. ecc .." "..
       "L ".. cc .. lists .. ecc .." "..
       "F ".. cc .. top_feeds .. ecc .." "..
       "mF ".. cc .. rfeeds .. ecc .." "..
       "S ".. cc .. spammy .. ecc .." "..
       ecc .. " "
end
set_email_widget = function()
   email_widget:set_markup('<span color="green">'.. get_unread_count() .."</span>")
end
email_widget = wibox.widget.textbox()
-- initialize email widget, then let it run every so often
set_email_widget()
email_timer = timer({ timeout = 20 })
email_timer:connect_signal("timeout", set_email_widget)
email_timer:start()

set_ipv6_widget = function()
   local ip6str = ""
   local io = { popen = io.popen }
   local s = io.popen("ip address show scope global")
   for line in s:lines() do
      if line:match("inet6.*global") then
         ip6str = "｢IPv6｣"
      end
   end
   ipv6_widget:set_markup('<span color="#913191">'.. ip6str ..'</span>')
end
ipv6_widget = wibox.widget.textbox()
set_ipv6_widget()
ipv6_timer = timer({ timeout = 5 })
ipv6_timer:connect_signal("timeout", set_ipv6_widget)
ipv6_timer:start()

-- create some vicious widgets
-- Initialize widget
-- mpdwidget = wibox.widget.textbox()
-- Register widget
--vicious.register(mpdwidget, vicious.widgets.mpd,
--                 function (mpdwidget, args)
--                    local ret = '|<span color="#2275FF"> MPD: '
--                    local fin = "   </span>|   "
--                    if args["{state}"] == "Stop" then
--                       return ret.." - "..fin
--                    else
--                       return ret..args["{Artist}"]..' - '..args["{Album}"]..' - '.. args["{Title}"]..fin
--                    end
--                 end, 3, {port=6637})

-- Initialize widget
memTextWidget = wibox.widget.textbox()
-- Register widget
vicious.register(memTextWidget, vicious.widgets.mem, "RAM: $1% ")

-- Initialize widget
memwidget = awful.widget.progressbar()
-- Progressbar properties
memwidget:set_width(8)
memwidget:set_height(10)
memwidget:set_vertical(true)
memwidget:set_background_color("#494B4F")
memwidget:set_border_color(nil)
memwidget:set_color("#003599")
--memwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 }, stops = { {0, "#AECF96"}, {0.5, "#88A175"},
--                         {1, "#FF5656"}}})
-- Register widget
vicious.register(memwidget, vicious.widgets.mem, "$1", 13)

rotmemwidget = wibox.container.rotate(memwidget, "east")

-- Initialize widget
cpuTextWidget = wibox.widget.textbox()
-- Register widget
vicious.register(cpuTextWidget, vicious.widgets.cpu, " | CPU: $1% | ")

-- this CPU widget is full of crap.
---- Initialize widget
--cpuwidget = awful.widget.graph()
---- Graph properties
--cpuwidget:set_width(50)
--cpuwidget:set_background_color("#494B4F")
--cpuwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 }, stops = { {0, "#FF5656"}, {0.5, "#88A175"},
--                         {1, "#AECF96" }}})
---- Register widget
--vicious.register(cpuwidget, vicious.widgets.cpu, "$1")

--volume widget
volumewidget = wibox.widget.textbox()
volumewidget:set_align("right")
-- wrapper to make mute status more visible...
volume_widget_wrapper = function(...)
   ret = vicious.widgets.volume(...)
   mutestr = ""
   if ret[2] == "♩" then
      mutestr = "MUTE"
   end
   return {ret[1], mutestr}
end
vicious.register(volumewidget, volume_widget_wrapper, "<span color='#00ffff'> Vol: $1%</span> <span color='#99FF99'>$2 </span>| ", 1, "Master")


-- battery widget
batteryTextWidget = wibox.widget.textbox()
vicious.register(batteryTextWidget, vicious.widgets.bat, "<span color='orange'> Bat: $2% $1</span>", 30, "BAT0")

batteryWidget = awful.widget.progressbar()
batteryWidget:set_width(8)
batteryWidget:set_height(10)
batteryWidget:set_vertical(true)
batteryWidget:set_background_color("#494B4F")
batteryWidget:set_border_color(nil)
batteryWidget:set_color("#f05030")
--batteryWidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 }, stops = { {0, "#AECF96"}, {0.5, "#88A175"},
--                         {1, "#FF5656"}}})
vicious.register(batteryWidget, vicious.widgets.bat, "$2", 30, "BAT0")
rotbatteryWidget = wibox.container.rotate(batteryWidget, "east")


return {
   email = email_widget,
   ipv6 = ipv6_widget,
   kbdstate = kbd_state_widget,
   cput = cpuTextWidget,
   memt = memTextWidget,
   mem = rotmemwidget,
   batt_t = batteryTextWidget,
   batt = rotbatteryWidget,
   vol = volumewidget
}

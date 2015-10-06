-- vicious widget registration:
-- vicious.register(widget, wtype, format, interval, warg)

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- create some vicious widgets
-- Initialize widget
mpdwidget = wibox.widget.textbox()
-- Register widget
vicious.register(mpdwidget, vicious.widgets.mpd,
                 function (mpdwidget, args)
                    local ret = "MPD: "
                    local fin = "   |   "
                    if args["{state}"] == "Stop" then
                       return ret.." - "..fin
                    else
                       return ret..args["{Artist}"]..' - '..args["{Album}"]..' - '.. args["{Title}"]..fin
                    end
                 end, 10, {port=6637})

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

-- Initialize widget
cpuTextWidget = wibox.widget.textbox()
-- Register widget
vicious.register(cpuTextWidget, vicious.widgets.cpu, "CPU: $1% | ")

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
vicious.register(volumewidget, vicious.widgets.volume, "Vol: $1% $2 | ", 1, "Master")


-- battery widget
batteryTextWidget = wibox.widget.textbox()
vicious.register(batteryTextWidget, vicious.widgets.bat, " Bat: $2% $1", 30, "BAT0")

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

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() then
               awful.tag.viewonly(c:tags()[1])
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, function ()
         if instance then
            instance:hide()
            instance = nil
         else
            instance = awful.menu.clients({ width=250 })
         end
   end),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
         if client.focus then client.focus:raise() end
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
         if client.focus then client.focus:raise() end
end))

for s = 1, screen.count() do
   -- Create a promptbox for each screen
   mypromptbox[s] = awful.widget.prompt()
   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   mylayoutbox[s] = awful.widget.layoutbox(s)
   mylayoutbox[s]:buttons(awful.util.table.join(
                             awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

   -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s })

   -- Widgets that are aligned to the left
   local left_layout = wibox.layout.fixed.horizontal()
   left_layout:add(mylauncher)
   left_layout:add(mytaglist[s])
   left_layout:add(mypromptbox[s])

   -- Widgets that are aligned to the right
   local right_layout = wibox.layout.fixed.horizontal()
   right_layout:add(mpdwidget)
   right_layout:add(volumewidget)
   right_layout:add(cpuTextWidget)
   --right_layout:add(cpuwidget)
   right_layout:add(memTextWidget)
   right_layout:add(memwidget)
   right_layout:add(batteryTextWidget)
   right_layout:add(batteryWidget)
   if s == 1 then right_layout:add(wibox.widget.systray()) end
   right_layout:add(mytextclock)
   right_layout:add(mylayoutbox[s])

   -- Now bring it all together (with the tasklist in the middle)
   local layout = wibox.layout.align.horizontal()
   layout:set_left(left_layout)
   layout:set_middle(mytasklist[s])
   layout:set_right(right_layout)

   mywibox[s]:set_widget(layout)
end
-- }}}

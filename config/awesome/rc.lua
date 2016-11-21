-- Standard awesome library
-- Load as globals so awesome-client can access them
gears = require("gears")
awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- so awesome-client will work
require("awful.remote")
-- Widget and layout library
wibox = require("wibox")
-- Theme handling library
beautiful = require("beautiful")
-- Notification library
-- don't load it, or it will fight with dunst, which is a better notification system
--naughty = require("naughty")
menubar = require("menubar")
vicious = require("vicious")
-- Requires awesome version 3.5. !!!!!!!!!

function table_shallow_copy(t)
   local u = { }
   for k, v in pairs(t) do u[k] = v end
   return setmetatable(u, getmetatable(t))
end
function table_concat(t1,t2)
   local t3 = table_shallow_copy(t1)
   for i = 1, #t2 do
      t3[#t1+i] = t2[i]
   end
   return t3
end

theme_env_var="WGH_THEME_DARK_OR_LIGHT"
init_theme_state = os.getenv(theme_env_var) or "dark"

globalstate = {
   kbdstate = "normal",
   theme_ld = init_theme_state
}


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
--   naughty.notify({ preset = naughty.config.presets.critical,
--                    title = "Oops, there were errors during startup!",
--                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- Make sure we don't go into an endless error loop
                             if in_error then return end
                             in_error = true

                             ------ TODO use notify-send here
--                             naughty.notify({ preset = naughty.config.presets.critical,
--                                              title = "Oops, an error happened!",
--                                              text = err })
                             in_error = false
   end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/home/wgh/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
--terminal = "xterm"
--terminal = "xterm -fg white -bg black"
terminal = {"vlaunch", "terminal"}
terminal2 = {"vlaunch", "terminal2"}
terminal3 = {"vlaunch", "terminal3"}
editor = os.getenv("EDITOR") or "nano"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
superkey = "Mod4"
hyperkey = "Mod2"
modkey = superkey

-- which modifier states will be ignored by bindings?
--awful.key.ignore_modifiers = {"Lock", "Mod2"}
awful.key.ignore_modifiers = {"Lock"}

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
   {
      awful.layout.suit.tile,
      awful.layout.suit.tile.bottom,
      awful.layout.suit.floating,

      ---- I almost like fair as a better default than tile, because it will tile 4 windows by giving
      ---- each a quadrant.  But you can't resize the left and right sides, which is something I often
      ---- do on the tile layout.  And you can achieve the 4 quadrant layout in tile by changing the
      ---- number of windows in the master area.
      --awful.layout.suit.fair,

      --awful.layout.suit.max,
      --awful.layout.suit.tile.left,
      --awful.layout.suit.spiral.dwindle,
      --awful.layout.suit.tile.top,
      --awful.layout.suit.fair.horizontal,
      --awful.layout.suit.spiral,
      -- max.fullscreen covers the status bar, which I think I always want for everything but full screen video
      --awful.layout.suit.max.fullscreen,
      -- magnifier is silly.  It's the one with one window essentially floating over the rest tiled in a weird way
      --awful.layout.suit.magnifier
   }
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
   for s = 1, screen.count() do
      gears.wallpaper.maximized(beautiful.wallpaper, s, true)
   end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
   -- Each screen has its own tag table.
   tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                             { "open terminal", terminal }
}
                       })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ widgets
require("widgets")
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

enterKeyState = function(stateName, globalkeys, clientkeys)
   globalstate.kbdstate = stateName
   root.keys(globalkeys)
      -- client.iterate(filter, start, s) gives an iterator to cycle through clients
   local nullFilter = function(c) return true end
   local clients = awful.client.iterate(nullFilter)
   for c in clients do
      c:keys(clientkeys)
   end
   mainRule.properties.keys = clientkeys
end

getCurrentClient = function()
   return awful.client.next(0)
end

enterNormalState = function()
   enterKeyState("normal", globalKeysNormal, clientKeysNormal)
   theme.border_focus = theme.border_focus__normal
   getCurrentClient().border_color = theme.border_focus__normal
end
enterLockedState = function()
   enterKeyState("locked", globalKeysLocked, clientKeysLocked)
   theme.border_focus = theme.border_focus__locked
   getCurrentClient().border_color = theme.border_focus__locked
end

toggleKeyState = function()
   if globalstate.kbdstate == "normal" then
      enterLockedState()
   else
      enterNormalState()
   end
end

mkspawn = function(cmd)
   return function()
      local l_or_d = "dark"
      if globalstate.theme_ld == "light" then
         -- l_or_d should only ever be exactly "light" or "dark"
         l_or_d = "light"
      end
      local cmdwrapper = {l_or_d}
      local full_command = table_concat(cmdwrapper, cmd)
      awful.util.spawn(full_command)
   end
end

-- {{{ Key bindings
genGlobalKeys = function(modkey)
   local globalkeys = awful.util.table.join(
      awful.key({ modkey,           }, "b",   awful.tag.viewprev       ),
      awful.key({ modkey,           }, "w",  awful.tag.viewnext       ),
      --awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

      awful.key({ modkey,           }, "j",
         function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
      end),
      awful.key({ modkey,           }, "k",
         function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
      end),
      --awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

      awful.key({ modkey,           }, "m", toggleKeyState),
      awful.key({ modkey,           }, "e", toggleKeyState),

      -- Layout manipulation
      awful.key({ modkey, "Mod1"   }, "j", function () awful.client.swap.byidx(  1)    end),
      awful.key({ modkey, "Mod1"   }, "k", function () awful.client.swap.byidx( -1)    end),
      awful.key({ modkey,           }, "n", function () awful.screen.focus_relative( 1) end),
      awful.key({ modkey,           }, "p", function () awful.screen.focus_relative(-1) end),
      --awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
      awful.key({ modkey,           }, "Tab",
         function ()
            awful.client.focus.history.previous()
            if client.focus then
               client.focus:raise()
            end
      end),

      -- Standard program
      awful.key({ modkey, "Control" }, "r", awesome.restart),
      awful.key({ modkey, "Shift", "Control"  }, "q", awesome.quit),

      awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
      awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
      awful.key({ modkey, "Mod1"   }, "h",     function () awful.tag.incnmaster( 1)      end),
      awful.key({ modkey, "Mod1"   }, "l",     function () awful.tag.incnmaster(-1)      end),
      awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
      awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
      awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
      awful.key({ modkey, "Mod1"   }, "space", function () awful.layout.inc(layouts, -1) end),

      awful.key({ modkey, "Control" }, "n", awful.client.restore),

      -- Prompt
      --awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

      awful.key({ modkey, "Mod1" }, "x",
         function ()
            awful.prompt.run({ prompt = "Run Lua code: " },
               mypromptbox[mouse.screen].widget,
               awful.util.eval, nil,
               awful.util.getdir("cache") .. "/history_eval")
      end),
      -- mENUBAr
      --awful.key({ modkey }, "p", function() menubar.show() end)

      -- Launch Programs
      awful.key({ modkey,           }, "r",     mkspawn({"bashrun"})),
      awful.key({ modkey,           }, "Return", mkspawn(terminal)),
      awful.key({ modkey, "Control" }, "Return", mkspawn(terminal2)),
      awful.key({ modkey, "Mod1"    }, "Return", mkspawn(terminal3)),

      --awful.key({ modkey, "Mod1"    }, "b", function () awful.util.spawn("dwb") end),
      awful.key({ modkey,           }, "q", mkspawn({"vlaunch", "lockscreen"})),
      awful.key({ modkey,    "Mod1" }, "a", mkspawn({"arandr"})),

      awful.key({ modkey,           }, "u", mkspawn({"vlaunch", "unicode"})),
      awful.key({                   }, "XF86DOS", mkspawn({"vlaunch", "unicode"})),
      ----- Set keys on level 3 and level 4 shift for top row
      awful.key({ hyperkey,         }, "Tab", mkspawn({"set-xkb-key-from-primary 12 3"})),
      awful.key({ hyperkey,         }, "=", mkspawn({"set-xkb-key-from-primary", "13", "3"})),
      awful.key({ hyperkey,         }, ";", mkspawn({"set-xkb-key-from-primary", "14", "3"})),
      awful.key({ hyperkey,         }, ":", mkspawn({"set-xkb-key-from-primary", "15", "3"})),
      awful.key({ hyperkey,         }, "\\", mkspawn({"set-xkb-key-from-primary", "16", "3"})),
      awful.key({ hyperkey,         }, "/", mkspawn({"set-xkb-key-from-primary", "17", "3"})),
      awful.key({ hyperkey,         }, "(", mkspawn({"set-xkb-key-from-primary", "18", "3"})),
      awful.key({ hyperkey,         }, ")", mkspawn({"set-xkb-key-from-primary", "19", "3"})),
      awful.key({ hyperkey, "Mod1"  }, "Tab", mkspawn({"set-xkb-key-from-primary", "12", "4"})),
      awful.key({ hyperkey, "Mod1"  }, "=", mkspawn({"set-xkb-key-from-primary", "13", "4"})),
      awful.key({ hyperkey, "Mod1"  }, ";", mkspawn({"set-xkb-key-from-primary", "14", "4"})),
      awful.key({ hyperkey, "Mod1"  }, ":", mkspawn({"set-xkb-key-from-primary", "15", "4"})),
      awful.key({ hyperkey, "Mod1"  }, "\\", mkspawn({"set-xkb-key-from-primary", "16", "4"})),
      awful.key({ hyperkey, "Mod1"  }, "/", mkspawn({"set-xkb-key-from-primary", "17", "4"})),
      awful.key({ hyperkey, "Mod1"  }, "(", mkspawn({"set-xkb-key-from-primary", "18", "4"})),
      awful.key({ hyperkey, "Mod1"  }, ")", mkspawn({"set-xkb-key-from-primary", "19", "4"})),
      --
      awful.key({ hyperkey,         }, "s", mkspawn({"vlaunch", "screenshot"})),
      awful.key({ hyperkey,         }, "g", mkspawn({"gajim-remote", "show_next_pending_event"})),
      awful.key({ hyperkey,         }, "m", mkspawn({"vlaunch", "volmute"})),
      awful.key({ hyperkey,         }, "u", mkspawn({"vlaunch", "volup"})),
      awful.key({ hyperkey,         }, "d", mkspawn({"vlaunch", "voldown"}))
   ) -- initial globalkeys ends here

-- I don't want these keys...
--   -- Compute the maximum number of digit we need, limited to 9
--   keynumber = 0
--   for s = 1, screen.count() do
--      keynumber = math.min(9, math.max(#tags[s], keynumber))
--   end
--
--   -- Bind all key numbers to tags.
--   -- Be careful: we use keycodes to make it works on any keyboard layout.
--   -- This should map on the top row of your keyboard, usually 1 to 9.
--   for i = 1, keynumber do
--      globalkeys = awful.util.table.join(globalkeys,
--                                         awful.key({ modkey }, "#" .. i + 9,
--                                            function ()
--                                               local screen = mouse.screen
--                                               if tags[screen][i] then
--                                                  awful.tag.viewonly(tags[screen][i])
--                                               end
--                                         end),
--                                         awful.key({ modkey, "Control" }, "#" .. i + 9,
--                                            function ()
--                                               local screen = mouse.screen
--                                               if tags[screen][i] then
--                                                  awful.tag.viewtoggle(tags[screen][i])
--                                               end
--                                         end),
--                                         awful.key({ modkey, "Shift" }, "#" .. i + 9,
--                                            function ()
--                                               if client.focus and tags[client.focus.screen][i] then
--                                                  awful.client.movetotag(tags[client.focus.screen][i])
--                                               end
--                                         end),
--                                         awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
--                                            function ()
--                                               if client.focus and tags[client.focus.screen][i] then
--                                                  awful.client.toggletag(tags[client.focus.screen][i])
--                                               end
--      end))
--   end
   return globalkeys
end -- genGlobalKeys ends here
globalKeysNormal = genGlobalKeys(modkey)
globalKeysLocked = genGlobalKeys("None")
root.keys(globalKeysNormal)

tagMaxId = 9
tagMinId = 1

genClientKeys = function(modkey)
   local clientkeys = awful.util.table.join(
      awful.key({ modkey,           }, "c",      function (c) c:kill()                         end),
      --awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
      awful.key({ modkey, "Mod1"   }, "p",      function(c) awful.client.movetoscreen(c, awful.screen.getbycoord(mouse.coords().x, mouse.coords().y) -1) end) ,
      awful.key({ modkey, "Mod1"   }, "n",      awful.client.movetoscreen                        ),
      awful.key({ modkey, "Mod1"   }, "b",
         -- Move forward one tag
         function (c)
            local curidx = awful.tag.getidx()
            if curidx == tagMinId then
               awful.client.movetotag(tags[client.focus.screen][tagMaxId])
            else
               awful.client.movetotag(tags[client.focus.screen][curidx - 1])
            end
            awful.tag.viewprev()
      end),
      awful.key({ modkey, "Mod1"   }, "w",
         -- move backwards one tag
         function (c)
            local curidx = awful.tag.getidx()
            if curidx == tagMaxId then
               awful.client.movetotag(tags[client.focus.screen][tagMinId])
            else
               awful.client.movetotag(tags[client.focus.screen][curidx + 1])
            end
            awful.tag.viewnext()
      end),
      --awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
      --    awful.key({ modkey,           }, "n",
      --        function (c)
      --            -- The client currently has the input focus, so it cannot be
      --            -- minimized, since minimized clients can't have the focus.
      --            c.minimized = true
      --        end),
      awful.key({ modkey, "Mod1"    }, "f",  awful.client.floating.toggle                     ),
      awful.key({ modkey, "Control" }, "f",  function (c) c.fullscreen = not c.fullscreen  end), --covers top bar
      awful.key({ modkey,      }, "f", -- full screen, doesn't cover top bar
         function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
      end)
   )
   return clientkeys
end
clientKeysNormal = genClientKeys(modkey)
clientKeysLocked = genClientKeys("None")


clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))


-- {{{ Rules
mainRule = { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    keys = clientKeysNormal,
                    buttons = clientbuttons} }
awful.rules.rules = {
   -- All clients will match this rule.
   mainRule,
   { rule_any = { name = {"bashrun", }},
     properties = { floating = true } },
   --    { rule_any = { class = { "MPlayer", "pinentry", "gimp", "bashrun" }},
   --      properties = { floating = true } },
   --{ rule = { class = "bashrun" },
   --  properties = { floating = true } },
   -- Set Firefox to always map on tags number 2 of screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
                         -- Enable sloppy focus
                         c:connect_signal("mouse::enter", function(c)
                                             if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                                             and awful.client.focus.filter(c) then
                                                client.focus = c
                                             end
                         end)

                         if not startup then
                            -- Set the windows at the slave,
                            -- i.e. put it at the end of others instead of setting it master.
                            -- awful.client.setslave(c)

                            -- Put windows in a smart way, only if they does not set an initial position.
                            if not c.size_hints.user_position and not c.size_hints.program_position then
                               awful.placement.no_overlap(c)
                               awful.placement.no_offscreen(c)
                            end
                         end

                         local titlebars_enabled = false
                         if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
                            -- Widgets that are aligned to the left
                            local left_layout = wibox.layout.fixed.horizontal()
                            left_layout:add(awful.titlebar.widget.iconwidget(c))

                            -- Widgets that are aligned to the right
                            local right_layout = wibox.layout.fixed.horizontal()
                            right_layout:add(awful.titlebar.widget.floatingbutton(c))
                            right_layout:add(awful.titlebar.widget.maximizedbutton(c))
                            right_layout:add(awful.titlebar.widget.stickybutton(c))
                            right_layout:add(awful.titlebar.widget.ontopbutton(c))
                            right_layout:add(awful.titlebar.widget.closebutton(c))

                            -- The title goes in the middle
                            local title = awful.titlebar.widget.titlewidget(c)
                            title:buttons(awful.util.table.join(
                                             awful.button({ }, 1, function()
                                                   client.focus = c
                                                   c:raise()
                                                   awful.mouse.client.move(c)
                                             end),
                                             awful.button({ }, 3, function()
                                                   client.focus = c
                                                   c:raise()
                                                   awful.mouse.client.resize(c)
                                             end)
                            ))

                            -- Now bring it all together
                            local layout = wibox.layout.align.horizontal()
                            layout:set_left(left_layout)
                            layout:set_right(right_layout)
                            layout:set_middle(title)

                            awful.titlebar(c):set_widget(layout)
                         end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Send mouse to corner so that it's not on the edge of two windows causing contention
mouse.coords({x = 10, y = 10})


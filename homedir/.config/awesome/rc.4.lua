-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
-- don't load it, or it will fight with dunst, which is a better notification system
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local socket = require("socket")


dotfilesdir = os.getenv("DOTFILESWGH")
local darkTheme = require("theme")
local lightTheme = require("light-theme")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end

function nnotify(str)
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Note:",
                    text = str})
end

-- }}}

-- {{{ convenience functions
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
-- }}}

-- {{{ global state stuff
mkspawn = function(cmd)
   return function()
      awful.spawn(cmd)
   end
end

-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
init_theme_state = "dark"
globalstate = {
   kbdstate = "normal",
   theme_ld = init_theme_state
}

beautiful.init(darkTheme)

lightdark_update_awesome = function(ld_stat)
   -- TODO - toggling this a bunch of times with theme init and top bar update slowed things way down, so I'm just going to give up for now on theming awesome as well as everything else for now.  Another idea might be restarting awesome, which makes things fast again, but which resets some state (mostly which workspace is active).
   if ld_stat == "light\n" or ld_stat == "light" then
      --beautiful.init(lightTheme)
      globalstate.theme_ld = "light"
   else
      --beautiful.init(darkTheme)
      globalstate.theme_ld = "dark"
   end

   -- reset widgets
   --awful.screen.connect_for_each_screen(topBarSetup)

   -- this one widget would be harder to get to work nicer, so let's just force it to update when switching color...
   --set_email_widget()
end
-- set theme once for startup based on lightdark-status
--awful.spawn.easy_async({"lightdark-status"}, function(stdout, stderr, reason, exit_code)
--      lightdark_update_awesome(stdout)
--end)

toggle_lightdark = function()
   awful.spawn.easy_async({"lightdark-status", "toggle"}, function(stdout, stderr, reason, exit_code)
         lightdark_update_awesome(stdout)
  end)
end


-- This is used later as the default terminal and editor to run.
terminal = {"vlaunch", "terminal"}
terminal2 = {"vlaunch", "terminal2"}
terminal3 = {"vlaunch", "terminal3"}
terminal4 = {"xterm"}
--terminal = "xterm"
editor = os.getenv("EDITOR") or "nano"
--editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
supkey = "Mod4"
modkey = supkey
hypkey = "Mod2"
altkey = "Mod1"
ctlkey = "Control"
sh1key = "Shift"
sh2key = "Mod5"
sh4key = "Mod3"

-- By default, the awful.key.ignore_modifiers table includes "Mod2", which is my hyper key.
-- It also includes "Lock", but really I think I'm happy to just empty it out.
--awful.key.ignore_modifiers = { "Lock" }
-- Just setting it like this doesn't seem to work, so let's mutate it instead.
for k,v in pairs(awful.key.ignore_modifiers) do
 table.remove(awful.key.ignore_modifiers, k)
end

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating,

--    awful.layout.suit.floating,
--    awful.layout.suit.tile,
--    awful.layout.suit.tile.left,
--    awful.layout.suit.tile.bottom,
--    awful.layout.suit.tile.top,
--    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
--    awful.layout.suit.max,
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.magnifier,
--    awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   --{ "manual", terminal .. " -e man awesome" },
   --{ "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", function() awful.spawn({"xterm"}) end},
                                    { "qw", function() awful.spawn({"qw"}) end},
                                    { "hkk", function() awful.spawn({"hkk"}) end},
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()
local mywidgets = require("widgetlib")

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

local registeredWidgets = {}
topBarSetup = (function(s)
    -- Remove old toolbar, if any.
    if s.mywibox then s.mywibox:remove() end
    --if s.taglist then s.taglist:remove() end

    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    -- not next(table) checks if it's empty...
    if not next(s.tags) then
       awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])
    end

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })
    local vicious = require("vicious")

    if registeredWidgets.mem then vicious.unregister(registeredWidgets.mem, false) end
    if registeredWidgets.batt_t then vicious.unregister(registeredWidgets.batt_t, false) end
    if registeredWidgets.batt then vicious.unregister(registeredWidgets.batt, false) end
    if registeredWidgets.vol then vicious.unregister(registeredWidgets.vol, false) end
    registeredWidgets.mem = mywidgets.mem()
    registeredWidgets.batt_t = mywidgets.batt_t()
    registeredWidgets.batt = mywidgets.batt()
    registeredWidgets.vol = mywidgets.vol()


    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mywidgets.email,
            mywidgets.ipv6,
            mywidgets.kbdstate,
            mywidgets.cput,
            mywidgets.memt,
            mywidgets.mem(),
            mywidgets.batt_t(),
            mywidgets.batt(),
            mywidgets.vol(),

            mykeyboardlayout,
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end)
awful.screen.connect_for_each_screen(topBarSetup)
-- }}}

function toggleBar(to)
   for s in screen do
      newval = to
      if newval == nil then
         newval = (not s.mywibox.visible)
      end
      s.mywibox.visible = newval
   end
   return
end


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

function mkNonRep(f)
   -- When I press hyper+<key> I get not one key event but TWO...
   -- So this function is for wrapping callbacks so that they can't be
   -- pressed twice within a certain small time so that the action
   -- is performed once and not twice.
   local repeatDelay = 100
   function innerMkNonRep(f)
      local lastCall = 0
      return function()
         local cur_ms = socket.gettime()*1000
         if lastCall + repeatDelay > cur_ms then
            return
         else
            lastCall = cur_ms
            return f()
         end
      end
   end
   return innerMkNonRep(f)
end

-- List of XF86 keys:
---------------------
--XF86AddFavorite
--XF86ApplicationLeft
--XF86ApplicationRight
--XF86AudioForward
--XF86AudioLowerVolume
--XF86AudioMedia
--XF86AudioMicMute
--XF86AudioMute
--XF86AudioNext
--XF86AudioPause
--XF86AudioPlay
--XF86AudioPrev
--XF86AudioRaiseVolume
--XF86AudioRecord
--XF86AudioRewind
--XF86AudioStop
--XF86Away
--XF86Back
--XF86BackForward
--XF86Battery
--XF86Bluetooth
--XF86Book
--XF86BrightnessAdjust
--XF86Calculator
--XF86Calendar
--XF86CD
--XF86Clear
--XF86_ClearGrab
--XF86Close
--XF86Community
--XF86Config
--XF86ContrastAdjust
--XF86Copy
--XF86Cut
--XF86Display
--XF86Documents
--XF86DOS
--XF86Eject
--XF86Excel
--XF86Explorer
--XF86Favorites
--XF86Finance
--XF86Forward
--XF86Game
--XF86Go
--XF86Hibernate
--XF86History
--XF86HomePage
--XF86HotLinks
--XF86iTouch
--XF86KbdBrightnessDown
--XF86KbdBrightnessUp
--XF86KbdLightOnOff
--XF86Keyboard
--XF86Launch0
--XF86Launch1
--XF86Launch2
--XF86Launch3
--XF86Launch4
--XF86Launch5
--XF86Launch6
--XF86Launch7
--XF86Launch8
--XF86Launch9
--XF86LaunchA
--XF86LaunchB
--XF86LaunchC
--XF86LaunchD
--XF86LaunchE
--XF86LaunchF
--XF86LightBulb
--XF86LogGrabInfo
--XF86LogOff
--XF86LogWindowTree
--XF86Mail
--XF86MailForward
--XF86Market
--XF86Meeting
--XF86MenuKB
--XF86MenuPB
--XF86Messenger
--XF86ModeLock
--XF86MonBrightnessDown
--XF86MonBrightnessUp
--XF86Music
--XF86MyComputer
--XF86New
--XF86News
--XF86_Next_VMode
--XF86OfficeHome
--XF86Open
--XF86OpenURL
--XF86Paste
--XF86Phone
--XF86Pictures
--XF86PowerOff
--XF86_Prev_VMode
--XF86Q
--XF86Reload
--XF86Reply
--XF86RFKill
--XF86RotateWindows
--XF86RotationKB
--XF86RotationPB
--XF86Save
--XF86ScreenSaver
--XF86ScrollClick
--XF86ScrollDown
--XF86ScrollUp
--XF86Search
--XF86Send
--XF86Shop
--XF86Sleep
--XF86Spell
--XF86SplitScreen
--XF86Standby
--XF86Start
--XF86Stop
--XF86Support
--XF86Suspend
--XF86_Switch_VT_1
--XF86_Switch_VT_10
--XF86_Switch_VT_11
--XF86_Switch_VT_12
--XF86_Switch_VT_2
--XF86_Switch_VT_3
--XF86_Switch_VT_4
--XF86_Switch_VT_5
--XF86_Switch_VT_6
--XF86_Switch_VT_7
--XF86_Switch_VT_8
--XF86_Switch_VT_9
--XF86TaskPane
--XF86Terminal
--XF86ToDoList
--XF86Tools
--XF86TouchpadOff
--XF86TouchpadOn
--XF86TouchpadToggle
--XF86Travel
--XF86_Ungrab
--XF86User1KB
--XF86User2KB
--XF86UserPB
--XF86VendorHome
--XF86Video
--XF86WakeUp
--XF86WebCam
--XF86WLAN
--XF86Word
--XF86WWAN
--XF86WWW
--XF86Xfer
--XF86ZoomIn
--XF86ZoomOut


-- {{{ Key bindings
globalkeys = awful.util.table.join(
--    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
--              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

--    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
--              {description = "show main menu", group = "awesome"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey, altkey    }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, altkey    }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey,           }, "b",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "w",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
   -- moving clients to another tag are in clientkeys
    awful.key({ modkey,           }, "n", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey,           }, "p", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
   -- moving clients to another screen are in clientkeys

--    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
--              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", mkspawn(terminal),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, altkey    }, "Return", mkspawn(terminal2),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, ctlkey    }, "Return", mkspawn(terminal3),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey,           }, "v", mkspawn(terminal),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, altkey    }, "v", mkspawn(terminal2),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, ctlkey    }, "v", mkspawn(terminal3),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey,           }, "s", mkspawn(terminal3),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, altkey    }, "s", mkspawn(terminal4),
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey,           }, "r", mkspawn({"vlaunch", "launcher"}),
              {description = "open a launcher", group = "launcher"}),
    awful.key({ modkey,           }, "q", mkspawn({"vlaunch", "lockscreen"}),
              {description = "lock screen", group = "launcher"}),
    awful.key({ modkey,           }, "u", mkspawn({"vlaunch", "unicode"}),
              {description = "input unicode", group = "launcher"}),
    awful.key({            }, "XF86DOS", mkspawn({"vlaunch", "unicode"}),
              {description = "input unicode", group = "launcher"}),
    awful.key({ }, "XF86MonBrightnessDown", mkspawn({"state", "backlight", "dec"}),
              {description = "bright down", group = "launcher"}),
    awful.key({ }, "XF86MonBrightnessUp", mkspawn({"state", "backlight", "inc"}),
              {description = "bright up", group = "launcher"}),
    awful.key({ }, "XF86AudioPrev", mkspawn({"vlaunch", "musicprev"}),
              {description = "media prev", group = "launcher"}),
    awful.key({ }, "XF86AudioNext", mkspawn({"vlaunch", "musicnext"}),
              {description = "media next", group = "launcher"}),
    awful.key({ }, "XF86AudioPlay", mkspawn({"vlaunch", "musictoggle"}),
              {description = "media play", group = "launcher"}),
    awful.key({ }, "XF86AudioPause", mkspawn({"vlaunch", "musicpause"}),
              {description = "media pause", group = "launcher"}),
    awful.key({ }, "XF86AudioStop", mkspawn({"vlaunch", "musicpause"}),
              {description = "media stop", group = "launcher"}),
    awful.key({ }, "XF86AudioLowerVolume", mkspawn({"state", "volume", "dec"}),
              {description = "volume down", group = "launcher"}),
    awful.key({ }, "XF86AudioRaiseVolume", mkspawn({"state", "volume", "inc"}),
              {description = "volume up", group = "launcher"}),
    awful.key({ }, "XF86AudioMute", mkspawn({"state", "mute", "toggle"}),
              {description = "volume up", group = "launcher"}),
    awful.key({ hypkey,         }, "z", mkNonRep(mkspawn({"notify", "hyp-z pressed"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "s", mkNonRep(mkspawn({"vlaunch", "screenshot"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "g", mkNonRep(mkspawn({"gajim-remote", "show_next_pending_event"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "m", mkNonRep(mkspawn({"state", "mute", "toggle"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "u", mkNonRep(mkspawn({"state", "volume", "inc"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "d", mkNonRep(mkspawn({"state", "volume", "dec"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "l", mkNonRep(toggle_lightdark), {description = "", group = "launcher"}),
    awful.key({ hypkey,         }, "t", mkNonRep(mkspawn({"dunstctl", "close-all"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "y", mkNonRep(mkspawn({"dunstctl", "context"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "h", mkNonRep(mkspawn({"dunstctl", "history-pop"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey, supkey }, "t", mkNonRep(mkspawn({"vlaunch", "musictoggle"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey, supkey }, "s", mkNonRep(mkspawn({"vlaunch", "musicpause"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey, supkey }, "n", mkNonRep(mkspawn({"vlaunch", "musicnext"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey, supkey }, "p", mkNonRep(mkspawn({"vlaunch", "musicprev"}, {description = "", group = "launcher"}))),
    awful.key({ hypkey,         }, "r", function() nnotify("pressed hyp+r") end),
--    ----- Set keys on level 3 and level 4 shift for top row
    awful.key({ hypkey,         }, "Tab", mkNonRep(mkspawn({"set-xkb-key-from-primary 12 3"}))),
    awful.key({ hypkey,         }, "=", mkNonRep(mkspawn({"set-xkb-key-from-primary", "13", "3"}))),
    awful.key({ hypkey,         }, ";", mkNonRep(mkspawn({"set-xkb-key-from-primary", "14", "3"}))),
    awful.key({ hypkey,         }, ":", mkNonRep(mkspawn({"set-xkb-key-from-primary", "15", "3"}))),
    awful.key({ hypkey,         }, "\\", mkNonRep(mkspawn({"set-xkb-key-from-primary", "16", "3"}))),
    awful.key({ hypkey,         }, "/", mkNonRep(mkspawn({"set-xkb-key-from-primary", "17", "3"}))),
    awful.key({ hypkey,         }, "(", mkNonRep(mkspawn({"set-xkb-key-from-primary", "18", "3"}))),
    awful.key({ hypkey,         }, ")", mkNonRep(mkspawn({"set-xkb-key-from-primary", "19", "3"}))),
    awful.key({ hypkey, "Mod1"  }, "Tab", mkNonRep(mkspawn({"set-xkb-key-from-primary", "12", "4"}))),
    awful.key({ hypkey, "Mod1"  }, "=", mkNonRep(mkspawn({"set-xkb-key-from-primary", "13", "4"}))),
    awful.key({ hypkey, "Mod1"  }, ";", mkNonRep(mkspawn({"set-xkb-key-from-primary", "14", "4"}))),
    awful.key({ hypkey, "Mod1"  }, ":", mkNonRep(mkspawn({"set-xkb-key-from-primary", "15", "4"}))),
    awful.key({ hypkey, "Mod1"  }, "\\", mkNonRep(mkspawn({"set-xkb-key-from-primary", "16", "4"}))),
    awful.key({ hypkey, "Mod1"  }, "/", mkNonRep(mkspawn({"set-xkb-key-from-primary", "17", "4"}))),
    awful.key({ hypkey, "Mod1"  }, "(", mkNonRep(mkspawn({"set-xkb-key-from-primary", "18", "4"}))),
    awful.key({ hypkey, "Mod1"  }, ")", mkNonRep(mkspawn({"set-xkb-key-from-primary", "19", "4"}))),

    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
--    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
--              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, altkey    }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, altkey    }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, altkey    }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

--    awful.key({ modkey, "Control" }, "r",
--              function ()
--                  local c = awful.client.restore()
--                  -- Focus restored client
--                  if c then
--                      client.focus = c
--                      c:raise()
--                  end
--              end,
--              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey, altkey },     "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"})
    -- Menubar
--    awful.key({ modkey }, "p", function() menubar.show() end,
--              {description = "show the menubar", group = "launcher"})
)

tagMaxId = 9
tagMinId = 1

clientkeys = awful.util.table.join(
    awful.key({ modkey, altkey    }, "b", function (c) 
       -- move client one tag backward
       local curtag = client.focus and client.focus.first_tag or nil
       local curidx = curtag.index
       if curidx == tagMinId then
          c:move_to_tag(client.focus.screen.tags[tagMaxId])
       else
          c:move_to_tag(client.focus.screen.tags[curidx - 1])
       end
       awful.tag.viewprev()
    end,
              {description = "move client to previous tag", group = "tag"}),
    awful.key({ modkey, altkey    }, "w", function (c) 
       -- move client one tag forward
       local curtag = client.focus and client.focus.first_tag or nil
       local curidx = curtag.index
       if curidx == tagMaxId then
          c:move_to_tag(client.focus.screen.tags[tagMinId])
       else
          c:move_to_tag(client.focus.screen.tags[curidx + 1])
       end
       awful.tag.viewnext()
    end,
              {description = "move client to next tag", group = "tag"}),
    awful.key({ modkey, altkey    }, "n", function (c) c:move_to_screen(c.screen.index + 1) end,
              {description = "move client to the next screen", group = "screen"}),
    awful.key({ modkey, altkey    }, "p", function (c) c:move_to_screen(c.screen.index - 1) end,
              {description = "move client to the previous screen", group = "screen"}),
    -- I think this is the full-screen command that covers the top bar
    awful.key({ modkey, ctlkey    }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey,           }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, altkey    }, "f",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, altkey    }, "t",  awful.titlebar.toggle                     ,
              {description = "toggle floating", group = "client"}),
--    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
--              {description = "move to master", group = "client"}),
--    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
--              {description = "move to screen", group = "client"}),
--    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
--              {description = "toggle keep on top", group = "client"}),
--    awful.key({ modkey,           }, "n",
--        function (c)
--            -- The client currently has the input focus, so it cannot be
--            -- minimized, since minimized clients can't have the focus.
--            c.minimized = true
--        end ,
--        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "f",
        -- fullscreen not covering top bar
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
--for i = 1, 9 do
--    globalkeys = awful.util.table.join(globalkeys,
--        -- View tag only.
--        awful.key({ modkey }, "#" .. i + 9,
--                  function ()
--                        local screen = awful.screen.focused()
--                        local tag = screen.tags[i]
--                        if tag then
--                           tag:view_only()
--                        end
--                  end,
--                  {description = "view tag #"..i, group = "tag"}),
--        -- Toggle tag display.
--        awful.key({ modkey, "Control" }, "#" .. i + 9,
--                  function ()
--                      local screen = awful.screen.focused()
--                      local tag = screen.tags[i]
--                      if tag then
--                         awful.tag.viewtoggle(tag)
--                      end
--                  end,
--                  {description = "toggle tag #" .. i, group = "tag"}),
--        -- Move client to tag.
--        awful.key({ modkey, "Shift" }, "#" .. i + 9,
--                  function ()
--                      if client.focus then
--                          local tag = client.focus.screen.tags[i]
--                          if tag then
--                              client.focus:move_to_tag(tag)
--                          end
--                     end
--                  end,
--                  {description = "move focused client to tag #"..i, group = "tag"}),
--        -- Toggle tag on focused client.
--        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
--                  function ()
--                      if client.focus then
--                          local tag = client.focus.screen.tags[i]
--                          if tag then
--                              client.focus:toggle_tag(tag)
--                          end
--                      end
--                  end,
--                  {description = "toggle focused client on tag #" .. i, group = "tag"})
--    )
--end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     --titlebars_enabled = true,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
          "bashrun"
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true, titlebars_enabled = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "dialog" }
      }, properties = { titlebars_enabled = true }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- {{{ Titlebar Configuration
-- Titlebars can be toggled on a client with
-- awful.titlebar.toggle(client)
-- awful.titlebar.show(client)
-- awful.titlebar.hide(client)
-- The default state for titlebars is set in the rules with the titlebars_enabled property.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
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
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)
-- }}}

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

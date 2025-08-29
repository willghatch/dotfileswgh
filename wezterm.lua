-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This is where you actually apply your config choices
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Lua helpers
--------------------------------------------------------------------------------

local function map(func, table)
    local newTable = {}
    for i, value in ipairs(table) do
        newTable[i] = func(value)
    end
    return newTable
end

local function concatenateTables(...)
    local result = {}

    for _, tbl in ipairs({...}) do
        for _, value in ipairs(tbl) do
            table.insert(result, value)
        end
    end

    return result
end

local function flattenOnce(tableOfTables)
    local flattened = {}

    for _, innerTable in ipairs(tableOfTables) do
        for _, value in ipairs(innerTable) do
            table.insert(flattened, value)
        end
    end

    return flattened
end


local function is_macos()
  -- The common triples for MacOS are aarch64-apple-darwin
  -- and x86_64-apple-darwin
  return wezterm.target_triple:find "darwin"
end
local function is_windows()
  return wezterm.target_triple:find "windows"
end



--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

config.enable_scroll_bar=true
config.hide_tab_bar_if_only_one_tab = true

-- Wezterm isn't working on hyprland without disabling wayland, but running in Xwayland hasa its own issues...
config.enable_wayland = true

if is_windows() then
  --config.default_domain = "WSL:Ubuntu"
  config.default_prog = {"wsl.exe"}
end

wezterm.on("open-uri", function(window, pane, uri)
             --window:copy_to_clipboard(uri, "Clipboard")
             --window:copy_to_clipboard(os.getenv("DOT_XPROFILE_LOADED") or "nothing", "Clipboard")
             -- returning false prevents the default action, returning true runs the default action (xdg-open)
             return true
end)




--------------------------------------------------------------------------------
-- Keyboard handling
--------------------------------------------------------------------------------

-- Unfortunately, the CSI-U protocol is enabled by a CSI escape sequence (`\e[>4;1m`), which some programs do automatically.  The CSI-U protocol is basically broken for my keyboard layout, so I don't know that I ever want it on.  It is very buggy in emacs, and less so but still buggy in vim.  In emacs I've hacked around to disable it, but for other programs it will be annoying.  There is currently no option to disable and ignore the CSI escape sequence to turn on the csi_u key encoding.
config.enable_csi_u_key_encoding = false
config.enable_kitty_keyboard = true


local uniqueKeys = {
  {
    -- Workaround for MacOS eating shift2 plus space...  I'm not sure why it's ALT, but I'll take it.
    key = " ",
    mods = "ALT",
    action = wezterm.action.SendKey {key = " "},
  },
}


-- Encoding for keys that don't work well with terminal.
-- I'm going to use M-+ M-+ <key> to mean C-<key>, where <key> can also have esc/meta prefixed, or be capitalized.
local veCtrl = "\x1b+\x1b+"
local eAlt = "\x1b"
local terminalTooOverloadedKeys = {
  {
    key = "i",
    mods = "CTRL",
    action = wezterm.action.SendString (veCtrl .. "i"),
  },
  {
    key = "i",
    mods = "CTRL|ALT",
    action = wezterm.action.SendString (veCtrl .. eAlt .. "i"),
  },
  {
    key = "m",
    mods = "CTRL",
    action = wezterm.action.SendString (veCtrl .. "m"),
  },
  {
    key = "m",
    mods = "CTRL|ALT",
    action = wezterm.action.SendString (veCtrl .. eAlt .. "m"),
  },
}

local a_to_z = {}
for i = 97, 122 do  -- 97 is the ASCII value for 'a' and 122 for 'z'
    table.insert(a_to_z, string.char(i))
end

local terminalEncodeKeys = concatenateTables(
  map(
    (function(c)
        return {
          key = c,
          mods = "CTRL|SHIFT",
          action = wezterm.action.SendString (veCtrl .. string.upper(c))
        }
    end),
    a_to_z),
  map(
    (function(c)
        return {
          key = c,
          mods = "CTRL|ALT|SHIFT",
          action = wezterm.action.SendString (veCtrl .. eAlt .. string.upper(c))
        }
    end),
    a_to_z)
)

local terminalMiscForHatchak = {
  {
    key = "(",
    mods = "CTRL",
    action = wezterm.action.SendString (veCtrl .. "(")
  },
  {
    key = "(",
    mods = "CTRL|ALT",
    action = wezterm.action.SendString (veCtrl .. eAlt .. "(")
  },
  {
    key = ")",
    mods = "CTRL",
    action = wezterm.action.SendString (veCtrl .. ")")
  },
  {
    key = ")",
    mods = "CTRL|ALT",
    action = wezterm.action.SendString (veCtrl .. eAlt .. ")")
  },
  -- Actually, some of these are working ok with the kitty protocol and kkp package in emacs.
  -- At least, C-. and C-' are working with kkp.
  -- {
  --   key = ".",
  --   mods = "CTRL",
  --   action = wezterm.action.SendString (veCtrl .. ".")
  -- },
  -- {
  --   key = ".",
  --   mods = "CTRL|ALT",
  --   action = wezterm.action.SendString (veCtrl .. eAlt .. ".")
  -- },
  -- {
  --   key = "'",
  --   mods = "CTRL",
  --   action = wezterm.action.SendString (veCtrl .. "'")
  -- },
  -- {
  --   key = "'",
  --   mods = "CTRL|ALT",
  --   action = wezterm.action.SendString (veCtrl .. eAlt .. "'")
  -- },
}


config.keys = concatenateTables(uniqueKeys
                                , terminalTooOverloadedKeys
                                -- TODO - I need to filter out the bindings that I actually use, or explicitly add them, eg. for copy/paste on C-C C-V
                                --, terminalEncodeKeys
                                , terminalMiscForHatchak
)




--------------------------------------------------------------------------------
-- Appearance
--------------------------------------------------------------------------------

config.font = wezterm.font "Deja Vu Sans Mono"
--config.font = wezterm.font "Monaspace Radon"
if is_macos() then
  config.font = wezterm.font("Monaco")
  --config.font = wezterm.font("Monaco", {weight = "Thin"})
end
if is_windows() then
  config.font = wezterm.font("Cascadia Mono")
  --config.default_domain = "WSL:Ubuntu"
end


config.color_schemes = {
  ['wgh-dark'] = {
    -- The default text color
    foreground = '#b2b2b2',
    -- The default background color
    background = '#000000',

    -- Overrides the cell background color when the current cell is occupied by the
    -- cursor and the cursor style is set to Block
    --cursor_bg = '#52ad70',
    -- Overrides the text color when the current cell is occupied by the cursor
    --cursor_fg = 'black',
    -- Specifies the border color of the cursor when the cursor style is set to Block,
    -- or the color of the vertical or horizontal bar when the cursor style is set to
    -- Bar or Underline.
    --cursor_border = '#52ad70',

    -- the foreground color of selected text
    --selection_fg = 'black',
    -- the background color of selected text
    --selection_bg = '#fffacd',

    -- The color of the scrollbar "thumb"; the portion that represents the current viewport
    --scrollbar_thumb = '#222222',

    -- The color of the split lines between panes
    --split = '#444444',

    ansi = {
      'black',
      'maroon',
      'green',
      'olive',
      "#184eb2", --'navy',
      'purple',
      'teal',
      'silver',
    },
    brights = {
      'grey',
      'red',
      'lime',
      'yellow',
      "#5286fd", --'blue',
      'fuchsia',
      'aqua',
      'white',
    },

    -- Arbitrary colors of the palette in the range from 16 to 255
    --indexed = { [136] = '#af8700' },

    -- Since: 20220319-142410-0fcdea07
    -- When the IME, a dead key or a leader key are being processed and are effectively
    -- holding input pending the result of input composition, change the cursor
    -- to this color to give a visual cue about the compose state.
    compose_cursor = 'orange',

    -- Colors for copy_mode and quick_select
    -- available since: 20220807-113146-c2fee766
    -- In copy_mode, the color of the active text is:
    -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
    -- 2. selection_* otherwise
    --copy_mode_active_highlight_bg = { Color = '#000000' },
    -- use `AnsiColor` to specify one of the ansi color palette values
    -- (index 0-15) using one of the names "Black", "Maroon", "Green",
    --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
    -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
    --copy_mode_active_highlight_fg = { AnsiColor = 'Black' },
    --copy_mode_inactive_highlight_bg = { Color = '#52ad70' },
    --copy_mode_inactive_highlight_fg = { AnsiColor = 'White' },

    --quick_select_label_bg = { Color = 'peru' },
    --quick_select_label_fg = { Color = '#ffffff' },
    --quick_select_match_bg = { AnsiColor = 'Navy' },
    --quick_select_match_fg = { Color = '#ffffff' },

  },
}

-- audible_bell options are "Disabled" and "SystemBeep", currently no way to set another sound yet
config.audible_bell = "Disabled"
config.visual_bell = {
  fade_in_function = 'EaseIn',
  fade_in_duration_ms = 50,
  fade_out_function = 'EaseOut',
  fade_out_duration_ms = 50,
  -- TODO - set visual_bell color in color schemes
}

local function get_appearance_from_lightdark()
  local ldStatusProc = io.popen("lightdark-status")
  local ldStatus = ldStatusProc:read("*a")
  ldStatusProc:close()
  if (ldStatus == "light\n") then
    return "Light"
  elseif (ldStatus == "dark\n") then
    return "Dark"
  else
    if (is_windows()) then
      local f = io.open(os.getenv("UserProfile") .. "/lightdark-current")
      local str = f:read("*a")
      f:close()
      if (str == "light\n") then
        return "Light"
      else
        return "Dark"
      end
    end
    return "Dark"
  end
end

-- This code is based on code from the wezterm docs.
local function get_appearance()
  if wezterm.gui then
    -- TODO - this doesn't seem to be working on Linux.
    if is_macos() then
      return wezterm.gui.get_appearance()
    end
    if is_windows() then
      return wezterm.gui.get_appearance()
    end
  end
  return get_appearance_from_lightdark()
end

local function scheme_for_appearance(appearance)
  if appearance:find "Dark" then
    return "wgh-dark"
  else
    return "Solarized Light (Gogh)"
  end
end

config.color_scheme = scheme_for_appearance(get_appearance())


---- Fuss with font size for different monitors.
---- There is a DPI feature, but for now I'll just set it based on monitor resolution...
--wezterm.on("window-resized", function(window, pane)
--             print(window:get_dimensions())
--             print(wezterm.gui.screens())
--             if (wezterm.gui.screens().active.width > 1920) then
--               window:set_config_overrides({font_size=14})
--             else
--               window:set_config_overrides({font_size=12})
--             end
--end)

local function updateAppearanceDynamic(window, pane)
  --print("in updateAppearanceDynamic\n")
  local overrides = window:get_config_overrides() or {}
  local appearance = window:get_appearance()
  local scheme = scheme_for_appearance(appearance)
  if overrides.color_scheme ~= scheme then
    overrides.color_scheme = scheme
    window:set_config_overrides(overrides)
  end
end

--wezterm.on('window-config-reloaded', updateAppearanceDynamic)
-- the update-status event fires based on status_update_interval in milliseconds
-- It is suggested as a good way to update appearance...
-- It only fires when the window is active.
if is_windows() then
  wezterm.on('update-status', updateAppearanceDynamic)
  config.status_update_interval = 2000
end



--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------


-- and finally, return the configuration to wezterm
return config

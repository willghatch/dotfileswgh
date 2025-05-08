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

function is_macos()
  -- The common triples for MacOS are aarch64-apple-darwin
  -- and x86_64-apple-darwin
  return wezterm.target_triple:find "darwin"
end

config.enable_scroll_bar=true
config.hide_tab_bar_if_only_one_tab = true

-- Wezterm isn't working on hyprland without disabling wayland, but running in Xwayland hasa its own issues...
config.enable_wayland = true

-- Keyboard handling
-- Unfortunately, the CSI-U protocol is enabled by a CSI escape sequence (`\e[>4;1m`), which some programs do automatically.  The CSI-U protocol is basically broken for my keyboard layout, so I don't know that I ever want it on.  It is very buggy in emacs, and less so but still buggy in vim.  In emacs I've hacked around to disable it, but for other programs it will be annoying.  There is currently no option to disable and ignore the CSI escape sequence to turn on the csi_u key encoding.
config.enable_csi_u_key_encoding = false

config.font = wezterm.font "Deja Vu Sans Mono"
--config.font = wezterm.font "Monaspace Radon"
if is_macos() then
  config.font = wezterm.font("Monaco")
  --config.font = wezterm.font("Monaco", {weight = "Thin"})
end

config.keys = {
  {
    -- Workaround for MacOS eating shift2 plus space...  I'm not sure why it's ALT, but I'll take it.
    key = " ",
    mods = "ALT",
    action = wezterm.action.SendKey {key = " "},
  },
}

-- Color scheme
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


function get_appearance_from_lightdark()
  local ldStatusProc = io.popen("lightdark-status")
  local ldStatus = ldStatusProc:read("*a")
  ldStatusProc:close()
  if (ldStatus == "light\n") then
    return "Light"
  elseif (ldStatus == "dark\n") then
    return "Dark"
  else
    return "Dark"
  end
end

-- This code is based on code from the wezterm docs.
function get_appearance()
  if wezterm.gui then
    -- TODO - this doesn't seem to be working on Linux.
    if is_macos() then
      return wezterm.gui.get_appearance()
    end
  end
  return get_appearance_from_lightdark()
end

function scheme_for_appearance(appearance)
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

wezterm.on("open-uri", function(window, pane, uri)
             --window:copy_to_clipboard(uri, "Clipboard")
             -- returning false prevents the default action, returning true runs the default action (xdg-open)
             return true
end)

-- and finally, return the configuration to wezterm
return config

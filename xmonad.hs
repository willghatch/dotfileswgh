
import XMonad hiding ((|||))
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Util.EZConfig
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.SwapWorkspaces
import XMonad.Layout.SimplestFloat

-- Hackage dependencies: xmonad, xmonad-contrib, xmobar

carryToNamedWs = \xpconf -> withWorkspace xpconf (\ws -> windows $ W.greedyView ws . W.shift ws)

myKeys = \c -> mkKeymap c $
    [ ("M4-<Return>", spawn "vlaunch terminal")
    , ("M2-<Return>", spawn "vlaunch terminal")
    , ("M4-r", spawn "vlaunch launcher")
    -- Resize viewed windows to the correct size
    , ("M4-M1-r", refresh)
    , ("M4-q", spawn "vlaunch lockscreen")
    , ("M4-c", kill)
    --, ("M4-<Space>", sendMessage NextLayout)
    , ("M4-<Space>", cycleThroughLayouts ["Tall", "Mirror Tall"])
    , ("M4-f", cycleThroughLayouts ["Full"])
    , ("M4-C-f", cycleThroughLayouts ["SimplestFloat"])
    , ("M4-m", sendMessage ToggleStruts)
    --  Reset the layouts on the current workspace to default
    --, ("M4-S-<Space>", setLayout $ XMonad.layoutHook conf)

    -- L1
    , ("M4-j", windows W.focusDown)
    , ("M4-k", windows W.focusUp)
    , ("M4-M1-j", windows W.swapDown)
    , ("M4-M1-k", windows W.swapUp)
      -- Shrink/expand the master area
    , ("M4-h", sendMessage Shrink)
    , ("M4-l", sendMessage Expand)
      -- num clients in master area
    , ("M4-M1-h", sendMessage (IncMasterN 1))
    , ("M4-M1-l", sendMessage (IncMasterN (-1)))
      -- switching to master area
    , ("M4-x", windows W.focusMaster)
    , ("M4-M1-x", windows W.swapMaster)
    -- Push window back into tiling
    , ("M4-M1-f", withFocused $ windows . W.sink)

    -- L2
    -- TODO - use XMonad.Actions.DynamicWorkspaces to add/remove, and make namable workspaces
    -- also, maybe ignore empty workspaces when switching to N/P
    --, ("M4-w", nextWS)
    --, ("M4-b", prevWS)
    --, ("M4-M1-w", shiftToNext)
    --, ("M4-M1-b", shiftToPrev)
    , ("M4-w", moveTo Next HiddenWS)
    , ("M4-b", moveTo Prev HiddenWS)
    , ("M4-M1-w", shiftTo Next HiddenWS)
    , ("M4-M1-b", shiftTo Prev HiddenWS)
    , ("M4-C-w", swapTo Next)
    , ("M4-C-b", swapTo Prev)
    , ("M4-o o", selectWorkspace defaultXPConfig)
    , ("M4-o g", addWorkspacePrompt defaultXPConfig)
    , ("M4-o c", removeEmptyWorkspace)
    , ("M4-o r", renameWorkspace defaultXPConfig)
    , ("M4-o M4-o", carryToNamedWs defaultXPConfig)

    -- L3
    , ("M4-n", nextScreen)
    , ("M4-p", prevScreen)
    , ("M4-M1-n", shiftNextScreen)
    , ("M4-M1-p", shiftPrevScreen)
    , ("M4-C-n", swapNextScreen)
    , ("M4-C-p", swapPrevScreen)
      -- these groups are like a snapshot of which workspaces are visible now
    , ("M4-e g", promptWSGroupAdd defaultXPConfig "Name this group: ")
    , ("M4-e e", promptWSGroupView defaultXPConfig "Go to group: ")
    , ("M4-e c", promptWSGroupForget defaultXPConfig "Forget group: ")

    -- Quit xmonad
    , ("M4-M1-q", io (exitWith ExitSuccess))
    -- Restart xmonad
    , ("M4-C-q", spawn "xmonad --recompile; xmonad --restart")

    -- sundry
    , ("M4-. m", spawn "vlaunch volmute")
    , ("M4-. u", spawn "vlaunch volup")
    , ("M4-. d", spawn "vlaunch voldown")
    , ("M4-. v", spawn "vlaunch mixer")
    , ("M4-. s m", spawn "vlaunch volmuteS")
    , ("M4-. s u", spawn "vlaunch volupS")
    , ("M4-. s d", spawn "vlaunch voldownS")
    , ("M4-. s v", spawn "vlaunch mixerS")
    , ("M4-C-h b", spawn "vlaunch xmonadViewConfig")
    ]

myManageHook = composeAll
   [ className =? "Xmessage"  --> doFloat
   , title =? "bashrun"  --> doFloat
   , manageDocks
   ]

myLayoutHook = tiled ||| Mirror tiled ||| Full ||| simplestFloat where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 5/100

myConfig = defaultConfig
    { borderWidth = 2
    , terminal = "xterm"
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#22a5ff"
    , modMask = mod2Mask
    , keys = myKeys
    , manageHook = myManageHook <+> manageHook defaultConfig
    , workspaces = [ "scratch", "org", "web", "ax", "win" ]
    , layoutHook = myLayoutHook
    }

--main = xmonad $ myConfig

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Pretty Print options for status bar
myPP = xmobarPP
    { ppCurrent = xmobarColor "#4299f2" "" . wrap "<" ">"
    , ppVisible = xmobarColor "#4299f2" ""
    , ppHiddenNoWindows = xmobarColor "#444444" ""
    , ppUrgent = xmobarColor "#ff4444" ""
    , ppSep = " <fc=#999999>|</fc> "
    , ppTitle = xmobarColor "#00ff00" ""
    , ppLayout = xmobarColor "#309030" ""
    --, ppExtras = ... see docs to maybe set this up - but it basically has the
      -- same stuff that xmobar has... but likely less?
    }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask .|. controlMask, xK_m)

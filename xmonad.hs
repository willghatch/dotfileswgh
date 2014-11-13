
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
    [ ("M2-<Return>", spawn "vlaunch terminal")
    -- just in case I have keyboard issues
    , ("M4-<Return>", spawn "vlaunch terminal")
    , ("M2-r", spawn "vlaunch launcher")
    -- Resize viewed windows to the correct size
    , ("M2-M1-r", refresh)
    , ("M2-q", spawn "vlaunch lockscreen")
    , ("M2-c", kill)
    --, ("M2-<Space>", sendMessage NextLayout)
    , ("M2-<Space>", cycleThroughLayouts ["Tall", "Mirror Tall"])
    , ("M2-f", cycleThroughLayouts ["Full"])
    , ("M2-C-f", cycleThroughLayouts ["SimplestFloat"])
    , ("M2-m", sendMessage ToggleStruts)
    --  Reset the layouts on the current workspace to default
    --, ("M2-S-<Space>", setLayout $ XMonad.layoutHook conf)

    -- L1
    , ("M2-j", windows W.focusDown)
    , ("M2-k", windows W.focusUp)
    , ("M2-M1-j", windows W.swapDown)
    , ("M2-M1-k", windows W.swapUp)
      -- Shrink/expand the master area
    , ("M2-h", sendMessage Shrink)
    , ("M2-l", sendMessage Expand)
      -- num clients in master area
    , ("M2-M1-h", sendMessage (IncMasterN 1))
    , ("M2-M1-l", sendMessage (IncMasterN (-1)))
      -- switching to master area
    , ("M2-x", windows W.focusMaster)
    , ("M2-M1-x", windows W.swapMaster)
    -- Push window back into tiling
    , ("M2-M1-f", withFocused $ windows . W.sink)

    -- L2
    -- TODO - use XMonad.Actions.DynamicWorkspaces to add/remove, and make namable workspaces
    -- also, maybe ignore empty workspaces when switching to N/P
    --, ("M2-w", nextWS)
    --, ("M2-b", prevWS)
    --, ("M2-M1-w", shiftToNext)
    --, ("M2-M1-b", shiftToPrev)
    , ("M2-w", moveTo Next HiddenWS)
    , ("M2-b", moveTo Prev HiddenWS)
    , ("M2-M1-w", shiftTo Next HiddenWS)
    , ("M2-M1-b", shiftTo Prev HiddenWS)
    , ("M2-C-w", swapTo Next)
    , ("M2-C-b", swapTo Prev)
    , ("M2-o o", selectWorkspace defaultXPConfig)
    , ("M2-o g", addWorkspacePrompt defaultXPConfig)
    , ("M2-o c", removeEmptyWorkspace)
    , ("M2-o r", renameWorkspace defaultXPConfig)
    , ("M2-o M2-o", carryToNamedWs defaultXPConfig)

    -- L3
    , ("M2-n", nextScreen)
    , ("M2-p", prevScreen)
    , ("M2-M1-n", shiftNextScreen)
    , ("M2-M1-p", shiftPrevScreen)
    , ("M2-C-n", swapNextScreen)
    , ("M2-C-p", swapPrevScreen)
      -- these groups are like a snapshot of which workspaces are visible now
    , ("M2-e g", promptWSGroupAdd defaultXPConfig "Name this group: ")
    , ("M2-e e", promptWSGroupView defaultXPConfig "Go to group: ")
    , ("M2-e c", promptWSGroupForget defaultXPConfig "Forget group: ")

    -- Quit xmonad
    , ("M2-M1-q", io (exitWith ExitSuccess))
    -- Restart xmonad
    , ("M2-C-q", spawn "xmonad --recompile; xmonad --restart")

    -- sundry
    , ("M2-. m", spawn "vlaunch volmute")
    , ("M2-. u", spawn "vlaunch volup")
    , ("M2-. d", spawn "vlaunch voldown")
    , ("M2-. v", spawn "vlaunch mixer")
    , ("M2-. s m", spawn "vlaunch volmuteS")
    , ("M2-. s u", spawn "vlaunch volupS")
    , ("M2-. s d", spawn "vlaunch voldownS")
    , ("M2-. s v", spawn "vlaunch mixerS")
    , ("M2-C-h b", spawn "vlaunch xmonadViewConfig")
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

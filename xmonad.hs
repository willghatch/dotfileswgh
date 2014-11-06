
import XMonad
import XMonad.Util.EZConfig
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Hackage dependencies: xmonad, xmonad-contrib, xmobar


myKeys = \c -> mkKeymap c $
    [ ("M2-<Return>", spawn "vlaunch terminal")
    -- just in case I have keyboard issues
    , ("M4-<Return>", spawn "vlaunch terminal")
    , ("M2-r", spawn "vlaunch launcher")
    -- Resize viewed windows to the correct size
    , ("M2-M1-r", refresh)
    , ("M2-q", spawn "vlaunch lockscreen")
    , ("M2-c", kill)
    , ("M2-<Space>", sendMessage NextLayout)
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

    -- L3
    , ("M2-n", nextScreen)
    , ("M2-p", prevScreen)
    , ("M2-M1-n", shiftNextScreen)
    , ("M2-M1-p", shiftPrevScreen)
    , ("M2-C-n", swapNextScreen)
    , ("M2-C-p", swapPrevScreen)
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
    , ("M2-C-h b", spawn "vlaunch xmonadViewConfig")
    ]

myManageHook = composeAll
   [ className =? "Xmessage"  --> doFloat
   , title =? "bashrun"  --> doFloat
   , manageDocks
   ]

myConfig = defaultConfig
    { borderWidth = 2
    , terminal = "xterm"
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#22a5ff"
    , modMask = mod2Mask
    , keys = myKeys
    , manageHook = myManageHook <+> manageHook defaultConfig
    }

--main = xmonad $ myConfig

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask .|. controlMask, xK_m)
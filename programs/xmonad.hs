import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Spacing
import XMonad.Layout.Reflect
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "alacritty"
myFocusFollowsMouse = False
myClickJustFocuses = False
myBorderWidth   = 4
myModMask       = mod4Mask
myWorkspaces    = map show [1..9]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#2000e0"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_p     ), spawn "emacsclient -c")
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_y     ), io (exitWith ExitSuccess))
    , ((modm              , xK_y     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_l     ), spawn "dm-tool lock")
    , ((modm,               xK_x     ), spawn ("(setxkbmap -query | grep -q \"layout:\\s\\+us\") && setxkbmap se || setxkbmap us"))
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [ ((0, xF86XK_AudioLowerVolume   ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0,        xF86XK_AudioMute   ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0,        xF86XK_AudioMicMute   ), spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = tiled ||| Mirror tiled ||| reflectHoriz tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className  =? "confirm"                            --> doFloat
     , className  =? "file_progress"                      --> doFloat
     , className  =? "dialog"                             --> doFloat
     , className  =? "download"                           --> doFloat
     , className  =? "error"                              --> doFloat
     , className  =? "notification"                       --> doFloat
     , className  =? "toolbar"                            --> doFloat
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
     , className  =? "Emacs"                              --> doShift "1"
     , title      =? "Mozilla Firefox"                    --> doShift "2"
     , className  =? "Slack"                              --> doShift "3"
     , className  =? ""                                   --> doShift "5"
     ]


windowSpacing = spacingRaw True (Border 5 5 5 5) True (Border 15 15 15 15) True

main = do
    xmonad $ docks $ ewmhFullscreen . ewmh $ defaults
      { manageHook = myManageHook <+> manageDocks
      , layoutHook = windowSpacing $ avoidStruts myLayout
      , startupHook = do
          spawn "sh .config/polybar/launch.sh"
          spawnOnce "nm-applet"
          spawnOnce "picom --experimental-backends"
          spawnOnce "volctl"
          spawnOnce "autorandr --change && feh --bg-scale ~/Pictures/wallpapers/active.jpg"
          setWMName "LG3D"
          spawnOn "1" "emacs"
          spawnOn "2" "firefox"
          spawnOn "3" "slack"
      , handleEventHook = mempty
    }

defaults = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings
    }

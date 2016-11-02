import System.IO
import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)

import qualified XMonad.StackSet as W

-- Solarized colours
sBase02 = "#073642"
sBase00 = "#657b83" 
sBase1  = "#93a1a1"

myNavigation2DConfig = defaultNavigation2DConfig

myConfig h = ewmh defaultConfig
  { modMask             = mod4Mask
  , manageHook          = myManageHook
  , layoutHook          = myLayoutHook
  , handleEventHook     = myHandleEventHook
  , logHook             = myLogHook h
  , terminal            = "konsole"
  , clickJustFocuses    = False
  , focusFollowsMouse   = False
  , normalBorderColor   = sBase02
  , focusedBorderColor  = sBase1
  } `additionalKeysP` (myKeys ++ myKeys2 ++ myKeys3)

myLogHook h = dynamicLogWithPP $ xmobarPP
  { ppOutput            = hPutStrLn h
  , ppTitle             = xmobarColor sBase1 "" . shorten 100
  , ppCurrent           = xmobarColor sBase1 ""
  , ppVisible           = xmobarColor sBase00 ""
  , ppSort              = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP
  , ppSep               = "   "
  }

myHandleEventHook =
  mconcat
    [ docksEventHook
    , handleEventHook defaultConfig
    ]

myPlaceHook = smart (1/2, 1/2)

myManageHook =
  placeHook myPlaceHook <+>
  composeOne
    [ checkDock                   -?> doIgnore
    , isDialog                    -?> doFloat
    , className =? "kcharselect"  -?> doFloat
    , className =? "Xmessage"     -?> doFloat
    , className =? ""             -?> doFloat
    , return True                 -?> doF W.swapDown
    ]

myLayoutHook = avoidStruts(tall ||| Mirror tall ||| twoPane ||| Full) ||| noBorders (fullscreenFull Full)
  where
    tall    = Tall nmaster delta ratio
    twoPane = TwoPane delta ratio
    nmaster = 1
    ratio   = 56/100
    delta   = 3/100

myKeys =
  [("M-<Left>",       windowGo L True) -- Go left
  ,("M-<Right>",      windowGo R True) -- Go right
  ,("M-<Up>",         windowGo U True) -- Go up
  ,("M-<Down>",       windowGo D True) -- Go down

  ,("M-S-<Left>",     windowSwap L True) -- Swap left
  ,("M-S-<Right>",    windowSwap R True) -- Swap right
  ,("M-S-<Up>",       windowSwap U True) -- Swap up
  ,("M-S-<Down>",     windowSwap D True) -- Swap down

  ,("M-C-<Left>",     screenGo L True) -- Go left
  ,("M-C-<Right>",    screenGo R True) -- Go right

  ,("M-S-C-<Left>",   screenSwap L True) -- Swap left
  ,("M-S-C-<Right>",  screenSwap R True) -- Swap right

  ,("M-M1-<Left>",    rotFocusedUp) -- Rotate focused up
  ,("M-M1-<Right>",   rotFocusedDown) -- Rotate focused up

  ,("M-f",            switchLayer) -- Switch layer

  ,("M-b",            sendMessage ToggleStruts)

  ,("<XF86AudioRaiseVolume>", spawn "pulseaudio-ctl up 5")
  ,("<XF86AudioLowerVolume>", spawn "pulseaudio-ctl down 5")
  ,("<XF86AudioMute>", spawn "pulseaudio-ctl mute")

  ,("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  ,("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")

  ,("M-S-m",          spawn "xbacklight -set 100")
  ]

myKeys2 =
  [("M-p",            spawn "dmenu_run -fn \"Luxi Sans Regular-9\" -nb \"#002b36\" -nf \"#657b83\"")
  ,("M-o",            spawn "spotify")
  ,("M-i",            spawn "konsole")
  ,("M-u",            spawn "urxvt")
  ,("M-v",            spawn "vmware")
  ,("M-w",            spawn "google-chrome-stable")
  ,("M-y",            spawn "kcharselect")
  ]

myKeys3 =
  [("M-s",            namedScratchpadAction myScratchpads "spotify")
  ]

myScratchpads =
  [ NS "spotify" "/usr/bin/spotify" (className =? "" <||> className =? "Spotify") (customFloating $ W.RationalRect (1/8) (1/8) (6/8) (6/8))
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $
    withNavigation2DConfig myNavigation2DConfig $
    myConfig xmproc

module Config ( modMask
              , home
              , topRightBarProcess
              , workspaceNames
              , windowRules
              , myScratchPads
              , focusedBorderColor
              , normalBorderColor
              , terminal
              , focusFollowsMouse )
where

import XMonad hiding (modMask,terminal,focusFollowsMouse,normalBorderColor
                     ,focusedBorderColor)
import XMonad.Util.NamedScratchpad

-------------------------------------------------------------------------
-- modMask lets you specify which modkey you want to use. 
-- mod1Mask - Left Alt    [default]
-- mod3Mask - Right Alt   (does not conflict with emacs)
-- mod4Mask - Windows Key (usually!)
-------------------------------------------------------------------------
modMask            = mod4Mask

------------------------------------------------------------------------
-- Other Constants
------------------------------------------------------------------------
home               = "/home/spranesh/" -- ending slash must be present
topRightBarProcess = home ++ ".xmonad/sys_info"
workspaceNames     = ["$>", "code", "ff", "chat", "music", "doc", "fb", "etc"]

terminal           = "lxterminal"
focusFollowsMouse  = False
normalBorderColor  = "#dddddd" -- A light gray
focusedBorderColor = "#4cb7ff" -- A lightish blue

------------------------------------------------------------------------
-- Window Rules:
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
------------------------------------------------------------------------
windowRules = composeAll
    [ className =? "Gimp"           --> doFloat
    , className =? "Pidgin"         --> doFloat
    , className =? "Skype"          --> doFloat
    , className =? "frame"          --> doFloat -- vlc
    , className =? "Yakuake"        --> doFloat 
    , className =? "Tomboy"         --> doFloat 
    , className =? "Mousepad"       --> doFloat 
    , className =? "gvim"           --> doFloat 
    , className =? "Gvim"           --> doFloat 
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]


------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------
myScratchPads = [ 
    NS "notes" ("leafpad ~/notes.txt") (className =? "leafpad") defaultFloating 
    ] where role = stringProperty "WM_WINDOW_ROLE"



--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import Data.Char (isUpper)

import XMonad hiding ((|||))
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.EwmhDesktops
import System.IO

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.SimplestFloat

-- Third party things that add functionality
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace

import XMonad.Util.Scratchpad
-- import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- 3 Things that are mostly likely to change
homeDirectory = "/home/spranesh/" -- ending slash must be present
-- miscBarProcess = "python "++ homeDirectory ++ ".xmonad/bar_status.py"
miscBarProcess = homeDirectory ++ ".xmonad/sys_info"
workspaceNames = ["$>", "code", "ff", "chat", "music", "doc", "fb", "etc"]

-------------------------------------------------------------------------




-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "lxterminal"

--
-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. 
-- mod1Mask - Left Alt    [default]
-- mod3Mask - Right Alt   (does not conflict with emacs)
-- mod4Mask - Windows Key (usually!)
myModMask       = mod4Mask

myWorkspaces    = znames ++ map show [len+1..9] 
          where
          names = workspaceNames
          len  = length names
          znames = [ wrap name i | (name, i) <- (zip names [1..len])]
          wrap s i = s ++ " (" ++ show i ++ ")"

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd" -- A light gray
myFocusedBorderColor = "#4cb7ff" -- A lightish blue

-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(0,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
myDefaultGaps   = [(23,0,0,0)]

-- myScratchPads = [
--      -- run htop in xterm, find it by title, use default floating window placement
--      NS "notes" (homeDirectory ++ "bin/gvim --role notes ~/notes.txt") (role =? "notes") defaultFloating,
--      NS "quickcommand" "gnome-terminal --role quickcommand" (role =? "quickcommand") defaultFloating
--  ] where role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ 
    [ -- Default Keybindings
    -- launch a terminal
      ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. controlMask, xK_Return), spawn "xterm")
    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    -- toggle the status bar gap
    , ((modMask , xK_b), sendMessage ToggleStruts)
    , ((modMask , xK_a),  sendMessage MirrorShrink)
    , ((modMask , xK_z),  sendMessage MirrorExpand)
    -- toggle the dzen bar gap after removing the other struts with Win + b
    -- , ((modMask .|. controlMask  , xK_b ), modifyGap (
		-- \i n -> let x = (myDefaultGaps ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x))
    -- , ((modMask .|. shiftMask, xK_q     ), spawn "gnome-session-save --gui --kill")
    -- Restart xmonad
    , ((modMask .|. controlMask, xK_q     ), restart "xmonad" True)
    ]

    ++

    [ -- Layout Keybindings
     -- Rotate through the available layout algorithms
      ((modMask,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modMask .|. controlMask, xK_1), sendMessage $ JumpToLayout "Spiral")
    , ((modMask .|. controlMask, xK_2), sendMessage $ JumpToLayout "ResizableTall")
    , ((modMask .|. controlMask, xK_3), sendMessage $ JumpToLayout "Mirror ResizableTall")
    , ((modMask .|. controlMask, xK_4), sendMessage $ JumpToLayout "ThreeCol")
    , ((modMask .|. controlMask, xK_5), sendMessage $ JumpToLayout "Tabbed Simplest")
    , ((modMask .|. controlMask, xK_9), sendMessage $ JumpToLayout "Simplest Float")
    , ((modMask .|. controlMask, xK_0), sendMessage $ JumpToLayout "Full")
    -- Other Shortcut Keys to jumping to layouts since we have many
    ]

    ++

    [ -- Workspace Keybindings
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]


    ++

    [ -- My Keybindings
      -- Take Screenshot
      ((0, xK_Print), spawn "scrot -s -e mkdir -p ~/screenshots; mv $f ~/screenshots" )
    -- lock screen
    -- , ((mod1Mask .|. controlMask, xK_l), spawn "gnome-screensaver-command --lock")
    -- , ((modMask, xK_s), spawn "xterm -e screen -R")
    
    -- Win + shift Key Bindings -> extra functionality
    -- g -> goto a window
    , ((modMask  .|. shiftMask, xK_g), windowPromptGoto  defaultXPConfig)
    -- b -> bring me a window
    , ((modMask  .|. shiftMask, xK_b), windowPromptBring defaultXPConfig)
    -- shift a window to some tag space
    , ((modMask  .|. shiftMask, xK_w), workspacePrompt defaultXPConfig (windows . W.shift))

    , ((modMask .|. shiftMask, xK_n),appendFilePrompt defaultXPConfig (homeDirectory ++ "NOTES/FromXMonad"))
    , ((modMask .|. shiftMask, xK_m), manPrompt defaultXPConfig)

    -- Win + Contorl Bindings
    -- Used to run things
    , ((modMask  .|. controlMask, xK_k), spawn "audacious")
    , ((modMask  .|. controlMask, xK_m), spawn "leafpad")
    , ((modMask  .|. controlMask, xK_h), spawn "pcmanfm")
    , ((modMask  .|. controlMask, xK_x), shellPrompt defaultXPConfig)
    -- , ((modMask  .|. controlMask, xK_n), namedScratchpadAction myScratchPads "notes")
    -- , ((modMask  .|. controlMask, xK_t), namedScratchpadAction myScratchPads "quickcommand")
    -- run xmonad commands (y)
    -- , ((modMask .|. controlMask, xK_y), runCommand commands)
    ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
resizeableTiled = ResizableTall 1 (3/100) (1/2) [] 
threeColumns    = ThreeCol 1 (3/100) (1/2)
spiralLayout    = spiral (6/7)
tabbedLayout    = tabbed shrinkText chosenTheme where 
                    chosenTheme = defaultTheme
                    -- chosenTheme = (theme kavonLakeTheme) -- import XMonad.Util.Themes for this

myLayout = spiralLayout ||| Full  ||| resizeableTiled ||| Mirror resizeableTiled ||| threeColumns ||| tabbedLayout ||| simplestFloat
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
myManageHook = composeAll
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
    , resource  =? "kdesktop"       --> doIgnore]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False


------------------------------------------------------------------------
-- Pranesh Srinivasan
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
-- To emulate dwm's status bar
-- > logHook = dynamicLogDzen
-- myLogHook = return ()
-- see below for details (dzenProc)

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
main = do 
        -- default_programs
        d <- defaults
        xmonad d


-- default_programs = do
--              spawn "gnome-terminal -e 'screen -R' "
--              spawn "yakuake"

defaults = do
    wsdzn <- spawnPipe wsBar
    miscbar <- spawnPipe miscBar

    return defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ smartBorders $ myLayout,
        manageHook         = manageDocks <+> myManageHook,
                             -- <+> (namedScratchpadManageHook myScratchPads),
        logHook            = dynamicLogWithPP deiflPP {ppOutput = hPutStrLn wsdzn}
    }


miscBar = miscBarProcess ++  " | dzen2 -x '700' -ta 'r' " ++ dzenFont
wsBar = "dzen2 -ta 'l' -w '700'" ++ dzenFont


-- dzenFont = " -fn ' " ++ myFont ++ " ' "
dzenFont = ""
--myFont = "-adobe-palatino-bold-r-*-*-*-*-*-*-*-*-*-*"
-- myFont = "-bitstream-charter-*-r-*-*-*-100-*-*-*-*-*-*"
-- myFont = "-adobe-palatino-bold-r-*-*-*-*-*-*-*-*-*-*"
-- myFont = "xft:Sans:size=9:weight=regular:hinting=true:hintstyle=hintslight:antialias=true:rgba=rgb:lcdfilter=lcdligh"
-- myFont = "-*-avantgarde-demi-r-*-*-*-*-*-*-*-*-*"

------------------------------------------------------------------------
-- Theme 1

processLayoutName :: Int -> String -> String
processLayoutName n s = take n (filter (Data.Char.isUpper) s ++ repeat '-')

deiflPP = defaultPP { ppCurrent = wrap "^bg(#000)^fg(#a8a8ff) " " ^fg(#fedb73)^bg(#333)" 
                    , ppSep     = ""
                    , ppWsSep   = "|"
                    , ppVisible = wrap "^bg(#000)^fg(#eee) " " ^bg(#333)"
                    , ppLayout  = pad . wrap "[" "]" . dzenColor "#1F8534" "" . processLayoutName 3
                    , ppHidden  = wrap "^bg(#333)^fg(#888) " " "
                    , ppTitle   = pad . dzenColor "#dddddd" "" . wrap "< " " >" . shorten 30
                    , ppOrder   = \(ws:layout:title:rest) -> [layout, ws, title] ++ rest
                    }

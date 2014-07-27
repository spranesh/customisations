import qualified KeyBindings as K
import qualified Config as C

import Data.Char (isUpper)
import System.IO
import qualified Data.Map        as M

import XMonad hiding ((|||))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
-- import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.SimplestFloat

-- import XMonad.Util.NamedScratchpad

------------------------------------------------------------------------
-- Layouts:

-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
------------------------------------------------------------------------
resizeableTiled = ResizableTall 1 (3/100) (1/2) [] 
threeColumns    = ThreeCol 1 (3/100) (1/2)
spiralLayout    = spiral (6/7)
tabbedLayout    = tabbed shrinkText chosenTheme where 
                    chosenTheme = defaultTheme
                    -- chosenTheme = (theme kavonLakeTheme) -- import XMonad.Util.Themes for this

myLayout = spiralLayout ||| Full  ||| resizeableTiled ||| Mirror resizeableTiled ||| threeColumns ||| tabbedLayout ||| simplestFloat

processLayoutName :: Int -> String -> String
processLayoutName n s = take n (filter (Data.Char.isUpper) s ++ repeat '-')

ws_dzenPP = defaultPP { ppCurrent = wrap "^bg(#000)^fg(#a8a8ff) " " ^fg(#fedb73)^bg(#333)" 
                    , ppSep     = ""
                    , ppWsSep   = "|"
                    , ppVisible = wrap "^bg(#000)^fg(#eee) " " ^bg(#333)"
                    , ppLayout  = pad . wrap "[" "]" . dzenColor "#1F8534" "" . processLayoutName 3
                    , ppHidden  = wrap "^bg(#333)^fg(#888) " " "
                    , ppTitle   = pad . dzenColor "#dddddd" "" . wrap "< " " >" . shorten 30
                    , ppOrder   = \(ws:layout:title:rest) -> [layout, ws, title] ++ rest
                    }

myWorkspaces::[String]
myWorkspaces  = namesWithNumbers ++ map show [len+1..9] 
          where namesWithNumbers = [ wrapAround name i | (name, i) <- (zip names [1..len])]
                names = C.workspaceNames
                len  = length names
                wrapAround s i = s ++ " (" ++ show i ++ ")"

dzenFont,topRightBar,topLeftWSBar::String
dzenFont = "" -- like "-adobe-palatino-bold-r-*-*-*-*-*-*-*-*-*-*"
topRightBar  = C.topRightBarProcess ++  " | dzen2 -x '700' -ta 'r' " ++ dzenFont
topLeftWSBar = "dzen2 -ta 'l' -w '700'" ++ dzenFont
-- myConfig (requires the top Left Handle)

myConfig topLeftHandle = additionalKeys d $ K.keyBindings C.modMask
	where d = defaultConfig {
            	     terminal           = C.terminal,
            	     focusFollowsMouse  = C.focusFollowsMouse,
            	     borderWidth        = 1,
            	     modMask            = C.modMask,
            	     workspaces         = myWorkspaces,
            	     normalBorderColor  = C.normalBorderColor,
            	     focusedBorderColor = C.focusedBorderColor,
    
            	     layoutHook         = avoidStruts $ smartBorders $ myLayout,
            	     manageHook         = manageDocks <+> C.windowRules,
            	                          -- <+> (namedScratchpadManageHook myScratchPads),
            	     logHook            = dynamicLogWithPP ws_dzenPP {ppOutput = hPutStrLn topLeftHandle }
    	    	     }


main = do
    spawnPipe topRightBar
    wsDzen <- spawnPipe topLeftWSBar
    xmonad $ myConfig wsDzen

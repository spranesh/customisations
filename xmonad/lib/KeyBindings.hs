module KeyBindings (keyBindings) where

import Config hiding (modMask)

import XMonad hiding ((|||),modMask)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile

import XMonad.Hooks.ManageDocks

-- Third party things that add functionality
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.RunOrRaise

import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W

keyBindings::KeyMask -> [((KeyMask, KeySym), X () )]
keyBindings modMask = ( customKeyBindings modMask ++ layoutKeyBindings modMask )

------------------------------------------------------------------------
-- Custom Keybindings
------------------------------------------------------------------------
customKeyBindings modMask = [ -- My Keybindings
    -- Take Screenshot
    ((0, xK_Print), spawn "scrot -s -e mkdir -p ~/screenshots; mv $f ~/screenshots" )
    -- lock screen
    -- , ((mod1Mask .|. controlMask, xK_l), spawn "gnome-screensaver-command --lock")
    -- , ((modMask, xK_s), spawn "xterm -e screen -R")
    
    -- Win + shift Key Bindings -> extra functionality
    -- g -> goto a window
    , ((modMask, xK_s), runOrRaisePrompt defaultXPConfig)
    , ((modMask  .|. shiftMask, xK_d), windowPromptGoto  defaultXPConfig)
    -- b -> bring me a window
    , ((modMask  .|. shiftMask, xK_s), windowPromptBring defaultXPConfig)
    -- shift a window to some tag space
    , ((modMask  .|. shiftMask, xK_w), workspacePrompt defaultXPConfig (windows . W.shift))

    , ((modMask .|. shiftMask, xK_n),appendFilePrompt defaultXPConfig (Config.home ++ "NOTES/FromXMonad"))
    , ((modMask .|. shiftMask, xK_m), manPrompt defaultXPConfig)

    -- Win + Contorl Bindings
    -- Used to run things
    , ((modMask  .|. controlMask, xK_k), spawn "audacious")
    , ((modMask  .|. controlMask, xK_m), spawn "leafpad")
    , ((modMask  .|. controlMask, xK_h), spawn "pcmanfm")
    , ((modMask  .|. controlMask, xK_x), shellPrompt defaultXPConfig)

    , ((modMask  .|. controlMask, xK_n), namedScratchpadAction Config.myScratchPads "notes")
    ]

------------------------------------------------------------------------
-- Custom Keybindings related to layouts.
------------------------------------------------------------------------
layoutKeyBindings modMask = [ -- Layout Keybindings
    ((modMask .|. controlMask, xK_1), sendMessage $ JumpToLayout "Spiral")
    , ((modMask .|. controlMask, xK_2), sendMessage $ JumpToLayout "ResizableTall")
    , ((modMask .|. controlMask, xK_3), sendMessage $ JumpToLayout "Mirror ResizableTall")
    , ((modMask .|. controlMask, xK_4), sendMessage $ JumpToLayout "ThreeCol")
    , ((modMask .|. controlMask, xK_5), sendMessage $ JumpToLayout "Tabbed Simplest")
    , ((modMask .|. controlMask, xK_9), sendMessage $ JumpToLayout "Simplest Float")
    , ((modMask .|. controlMask, xK_0), sendMessage $ JumpToLayout "Full") ]

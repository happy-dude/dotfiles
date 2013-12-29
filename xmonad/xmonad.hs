{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

import qualified Data.Map as M
import qualified XMonad.StackSet as W   -- Window cycling with Alt+Tab
import XMonad
import XMonad.Actions.CycleWS           -- Workspace cycling
import XMonad.Actions.GridSelect        -- Display open windows in 2D grid
import XMonad.Actions.PhysicalScreens   -- Manipulate screens ordered by location instead of ID
import XMonad.Actions.UpdatePointer     -- Change pointer to follow whichever window focus changes to
import XMonad.Hooks.DynamicLog          -- Call loghook with every internal state update
import XMonad.Hooks.FadeInactive        -- Make inactive windows translucent
import XMonad.Hooks.ManageDocks         -- Tools to automatically manage dock programs, such as xmobar and dzen
import XMonad.Layout.Grid               -- A simple layout that attempts to put all windows in a square grid
import XMonad.Layout.IndependentScreens -- Simulate independent sets of workspaces on each screen
import XMonad.Layout.MultiToggle        -- Dynamically apply and unapply transformers to your window layout
import XMonad.Layout.MultiToggle.Instances  -- Shorthand so we don't have to type Instances.MIRROR/FULL
import XMonad.Layout.NoBorders          -- Make a given layout display without borders
import XMonad.Layout.Reflect            -- Reflect a layout horizontally or vertically
import XMonad.Layout.Renamed            -- Modify the description of its underlying layout in a flexible way
import XMonad.Layout.Spiral             -- Spiral tiling layout
import XMonad.Layout.Tabbed             -- Tabbed layout
import XMonad.Layout.ThreeColumns       -- 3 column layout similar to tall
import XMonad.Util.Run                  -- Run commands as external processes
import XMonad.Util.WorkspaceCompare

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    trayerProc <- spawnPipe "pkill trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true --widthtype percent --width 10 --heighttype pixel --height 18 --transparent true --alpha 0 --tint 0x000000 --padding 0"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayouts
        , logHook = myLogHook <+> dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                , ppSort = getSortByTag
                } >> updatePointer (TowardsCentre 0.2 0.2)
                --} >> updatePointer (Relative 0.5 0.5)         -- Recenter pointer at window
        , focusFollowsMouse = True
        , borderWidth = 2
        , normalBorderColor = "#0088ff"                         -- AzureRadiance blue
        , focusedBorderColor = "ff0000"                         -- Red
        , modMask = mod4Mask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , keys = \c -> myKeys c `M.union` keys defaultConfig c }

myLayouts = avoidStruts
            $ mkToggle (REFLECTX ?? REFLECTY ?? MIRROR ?? TABBED ?? FULL ?? EOT)
            $ Tall 1 (3/1000) (1/2)
                ||| ThreeCol 1 (3/100) (1/2)
                ||| renamed [Replace "ThreeColMid"] (ThreeColMid 1 (3/100) (1/2))
                ||| Grid
                ||| spiral (6/7)

-- Coloring for Tabbed Simplest (ModM F fullscreen mode)
myTabConfig = defaultTheme { inactiveBorderColor = "#0088ff"    -- Red
                           , activeBorderColor = "#ff0000"      -- AzureRadiance blue
                           , activeColor = "#000000"            -- Black
                           , inactiveColor = "#000000"          -- Black
                           , decoHeight = 18 }

-- Tabbed transformer
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
  transform _ x k = k (tabbed shrinkText myTabConfig) (const x)

-- Make inactive window transparent
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount where fadeAmount = 0.92

-- Names of workspaces
myWorkspaces = withScreens 1 [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

-- Keybinding configurations
myKeys conf@(XConfig {XMonad.modMask = modM}) = M.fromList $ [
    ((modM,               xK_t), sendMessage $ Toggle MIRROR)   -- Mirror Tall (reflect X + Y)
  , ((modM,               xK_f), sendMessage $ Toggle TABBED)   -- Tabbed Simplest (fullscreen with tab)
  , ((modM .|. shiftMask, xK_f), sendMessage $ Toggle FULL)     -- Full (without tabs)
  , ((modM,               xK_x), sendMessage $ Toggle REFLECTX) -- Reflect X
  , ((modM,               xK_y), sendMessage $ Toggle REFLECTY) -- Reflect Y
  , ((modM .|. shiftMask, xK_t), withFocused $ windows . W.sink)-- Tile a floating window (default was rebinded)
  , ((modM,           xK_grave), goToSelected $ gsconfig1)      -- Enable grid select with Mod+<Grave> -- the backtick
  , ((modM .|. shiftMask, xK_l), spawn "xlock -mode blank")     -- Lock screen with xlockmore with Mod+<Shift>+L
  , ((0, xK_Print), spawn "scrot")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")    -- Use scrot for printscreen snapshot of desktop and single window
  , ((mod1Mask, xK_Tab), windows W.swapDown)
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.swapUp)      -- Mimic Alt-Tab focus switching behavior
  ]
    ++
    [((m .|. modM, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    -- Switch between monitors (screens)
    [((modM .|. mask, key), f sc)
     | (key, sc) <- zip [xK_w, xK_e, xK_s, xK_d] [0..]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

    ++
    -- Use Mod+<Right/Left> arrow keys to move workspaces
    -- Also use Mod+<Shift>+<Right/Left> to move windows to workspace to left/right
    [((modM, xK_Left), prevWS)
     , ((modM, xK_Right), nextWS)
     , ((modM .|. shiftMask, xK_Left), shiftToPrev)
     , ((modM .|. shiftMask, xK_Right), shiftToNext)]

    ++
    -- Use volume media buttons to raise/lower volume and mute sound
    [((0, 0x1008FF11), spawn "amixer set Master 1-")        -- Volume Down
     , ((0, 0x1008FF13), spawn "amixer set Master 1+")      -- Volume Up
     , ((0, 0x1008FF12), spawn "amixer set Master toggle")] -- Mute

myTerminal = "~/.xmonad/urxvtdc.sh"

gsconfig1 = defaultGSConfig { gs_cellheight = 45, gs_cellwidth = 250 }

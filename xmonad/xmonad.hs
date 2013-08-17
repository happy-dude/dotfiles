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
import System.IO
import XMonad
import XMonad.Actions.CycleWS           -- Workspace cycling
import XMonad.Actions.GridSelect        -- Display open windows in 2D grid
import XMonad.Actions.PhysicalScreens   -- Manipulate screens ordered by location instead of ID
import XMonad.Actions.UpdatePointer     -- Change pointer to follow whichever window focus changes to
import XMonad.Hooks.DynamicLog          -- Call loghook with every internal state update
import XMonad.Hooks.FadeInactive        -- Make inactive windows translucent
import XMonad.Hooks.ManageDocks         -- Tools to automatically manage dock programs, such as xmobar and dzen
import XMonad.Layout.IndependentScreens -- Simulate independent sets of workspaces on each screen
import XMonad.Layout.NoBorders          -- Make a given layout display without borders
import XMonad.Util.Run(spawnPipe)       -- Run commands as external processes
import XMonad.Util.WorkspaceCompare

main = do
    --trayerKill <- spawnPipe "pkill trayer"
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    trayerProc <- spawnPipe "pkill trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true --widthtype percent --width 10 --heighttype pixel --height 18 --transparent true --alpha 0 --tint 0x000000 --padding 0"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook = myLogHook <+> dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                , ppSort = getSortByTag
                } >> updatePointer (TowardsCentre 0.2 0.2)
        , focusFollowsMouse = True
        , borderWidth = 2
        , normalBorderColor = "#0088ff"
        , focusedBorderColor = "ff0000"
        --, normalBorderColor = "#D1D1D1"
        --, focusedBorderColor = "#856042"
        , modMask = mod4Mask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
    }

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount where fadeAmount = 0.92

myWorkspaces = withScreens 1 [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

myKeys conf@(XConfig {XMonad.modMask = modM}) = M.fromList $ [
    ] ++
    [((m .|. modM, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [((modM .|. mask, key), f sc)
     | (key, sc) <- zip [xK_w, xK_e, xK_s, xK_d] [0..]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
    ++
    -- Enable grid select with Mod+<Grave> -- the backtick
    [((modM, xK_grave), goToSelected $ gsconfig1)]
    ++
    -- Use scrot for printscreen snapshot of desktop and single window
    [((0, xK_Print), spawn "scrot")
     , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")]
    ++
    -- Lock screen with xlockmore with Mod+<Shift>+L
    [((modM .|. shiftMask, xK_l), spawn "xlock -mode blank")]
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
    ++
    -- Mimic Alt-Tab focus switching behavior
    [((mod1Mask, xK_Tab), windows W.swapDown)
     , ((mod1Mask .|. shiftMask, xK_Tab), windows W.swapUp)]

myTerminal = "~/.xmonad/urxvtdc.sh"

gsconfig1 = defaultGSConfig { gs_cellheight = 45, gs_cellwidth = 250 }


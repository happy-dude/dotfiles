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
import qualified XMonad.StackSet as W
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
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
        , borderWidth = 1
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

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
    ] ++
    [ ((m .|. modm, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [((modm .|. mask, key), f sc)
     | (key, sc) <- zip [xK_w, xK_e, xK_s, xK_d] [0..]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
    ++
    [((mod4Mask, xK_Tab), goToSelected $ gsconfig1)]

myTerminal = "~/dotfiles/xmonad/urxvtdc.sh"

gsconfig1 = defaultGSConfig { gs_cellheight = 45, gs_cellwidth = 250 }


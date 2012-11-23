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
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

gsconfig1 = defaultGSConfig { gs_cellheight = 45, gs_cellwidth = 250 }

main = do
    trayerKill <- spawnPipe "pkill trayer"
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    trayerProc <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --widthtype percent --width 10 --heighttype pixel --height 18 --transparent true --alpha 1000 --tint 0x000000 --padding 0"
    xmonad $ defaultConfig
        { terminal = myTerminal
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                , ppSort = getSortByTag
                } >> updatePointer (TowardsCentre 0.2 0.2)
        , focusFollowsMouse = True
        --, normalBorderColor = "#D1D1D1"
        --, focusedBorderColor = "#856042"
        , borderWidth = 2
        , normalBorderColor = "#0088ff"
        , focusedBorderColor = "ff0000"
        , workspaces = myWorkspaces
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
    }

myWorkspaces = withScreens 1 [ "1", "2", "3", "4" ]

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
    [((mod4Mask, xK_space), goToSelected $ gsconfig1)]

myTerminal = "~/dotfiles/xmonad/urxvtdc.sh"

module Main where

{-
    This is xmonad.hs for me.
    You may need to install the following:
      - xmobar
    If you don't have, the following as well:
      - pactl
    Troubleshootings:
      Some programs open a weird file manager
        $ xdg-mime default pcmanfm.desktop inode/directory
      Java programs don't work very well
        $ export _JAVA_AWT_WM_NONREPARENTING=1
-}

import           Data.Functor                  ((<&>))
import           Data.Ratio                    ((%))
import           Lib.XMonad.Actions            (decVolume, incVolume, lock,
                                                toggleMute, volumeControl)
import           Lib.XMonad.Actions.XineramaWS (initScreens, nextWS, prevWS)
import           XMonad
import           XMonad.Actions.CycleWS        (nextScreen, shiftNextScreen)
import           XMonad.Actions.Minimize       (maximizeWindow, minimizeWindow,
                                                withLastMinimized)
import           XMonad.Actions.Search         (Browser, SearchEngine, hoogle,
                                                promptSearchBrowser)
import           XMonad.Hooks.DynamicLog       (xmobar)
import           XMonad.Hooks.EwmhDesktops     (ewmh)
import           XMonad.Hooks.ManageDocks      (avoidStruts, docks)
import           XMonad.Layout.BoringWindows   (boringWindows, focusDown,
                                                focusUp)
import           XMonad.Layout.Minimize        (minimize)
import           XMonad.Layout.NoBorders       (noBorders)
import           XMonad.Layout.ResizableTile   (MirrorResize (MirrorExpand, MirrorShrink),
                                                ResizableTall (ResizableTall))
import qualified XMonad.Prompt                 as P (XPConfig (..),
                                                     XPPosition (..))
import qualified XMonad.Prompt.Shell           as P (shellPrompt)
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig          (additionalKeysP, removeKeysP)
import           XMonad.Util.Run               (safeSpawnProg)


main :: IO ()
main = xmobar myConfig >>= xmonad
    where
    myConfig = docks . ewmh . configKeys $ def
        { borderWidth = 3 :: Dimension
        , focusFollowsMouse = False
        , layoutHook = myLayout
        , modMask = mod1Mask :: KeyMask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        }
    myLayout = avoidStruts . boringWindows . minimize $ myTiles
        where
        myTiles = myTile ||| Mirror myTile ||| noBorders Full
        myTile = ResizableTall 1 (3 % 100) (1 / phi) [1, 6 % 5]
        phi = 8 % 5

myTerminal :: String
myTerminal = "lxterminal"

myWorkspaces :: [WorkspaceId]
myWorkspaces = [0..9] <&> (show :: Int -> String)

configKeys :: XConfig l -> XConfig l
configKeys c = c `additionalKeysP` myAdditionalKeys `removeKeysP` myRemovedKeys
    where
    myAdditionalKeys :: [(String, X ())]
    myAdditionalKeys =
        [ ("M4-l", lock)
        , ("M1-C-t", safeSpawnProg myTerminal)
        , ("M-p", P.shellPrompt myPromptConfig)
        , ("M-a", sendMessage MirrorShrink)
        , ("M-z", sendMessage MirrorExpand)
        , ("M-j", focusDown)
        , ("M-k", focusUp)
        , ("M-r", nextScreen)
        , ("M-S-r", shiftNextScreen >> nextScreen)
        , ("M-i", windows W.swapMaster)
        , ("M1-C-l", nextWS)
        , ("M1-C-h", prevWS)
        , ("M-m", withFocused minimizeWindow)
        , ("M-S-m", withLastMinimized maximizeWindow)
        , ("M-u a", incVolume)
        , ("M-u x", decVolume)
        , ("M-u m", toggleMute)
        , ("M-u v", volumeControl)
        , ("M-u i", initScreens)
        , ("M-u h", myPrompt hoogle)
        ]
    myRemovedKeys :: [String]
    myRemovedKeys =
        [ "M-S-q"
        , "M1-<Return>" -- For intellij idea. Use M-i instead
        ]

myPrompt :: SearchEngine -> X ()
myPrompt = promptSearchBrowser myPromptConfig myBrowser

myBrowser :: Browser
myBrowser = "chromium-browser"

myPromptConfig :: P.XPConfig
myPromptConfig = def
    { P.font = "-misc-fixed-*-*-*-*-18-*-*-*-*-*-*-*"
    , P.position = P.Top
    , P.height = 20
    }

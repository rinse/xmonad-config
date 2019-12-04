module Lib
    ( lock
    , incVolume
    , decVolume
    , toggleMute
    ) where

import           XMonad          (X)
import           XMonad.Util.Run (safeSpawn, safeSpawnProg)


lock :: X ()
lock = safeSpawnProg "lxlock"

incVolume,decVolume, toggleMute :: X ()
incVolume = safeSpawn "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", "+10%" ]
decVolume = safeSpawn "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", "-10%" ]
toggleMute = safeSpawn "pactl" [ "set-sink-mute", "@DEFAULT_SINK@", "toggle" ]

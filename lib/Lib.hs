module Lib
    ( lock
    , incVolume
    , decVolume
    , toggleMute
    , volumeControl
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           XMonad.Util.Run        (safeSpawn, safeSpawnProg)


lock :: MonadIO m => m ()
lock = safeSpawnProg "lxlock"

incVolume, decVolume, toggleMute :: MonadIO m => m ()
incVolume = safeSpawn "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", "+10%" ]
decVolume = safeSpawn "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", "-10%" ]
toggleMute = safeSpawn "pactl" [ "set-sink-mute", "@DEFAULT_SINK@", "toggle" ]

volumeControl :: MonadIO m => m ()
volumeControl = safeSpawnProg "pavucontrol"

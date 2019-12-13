module Lib.XMonad.Actions
    ( lock
    , incVolume
    , decVolume
    , toggleMute
    , volumeControl
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           XMonad.Util.Run        (safeSpawn, safeSpawnProg)


-- |Locks the screen. It uses lxlock by default.
lock :: MonadIO m => m ()
lock = safeSpawnProg "lxlock"

incVolume, decVolume, toggleMute :: MonadIO m => m ()

-- |Increments the volume by 10%.
incVolume = safeSpawn "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", "+10%" ]
-- |Decrements the volume by 10%.
decVolume = safeSpawn "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", "-10%" ]
-- |Toggles mute.
toggleMute = safeSpawn "pactl" [ "set-sink-mute", "@DEFAULT_SINK@", "toggle" ]

-- |Opens a volume control gui.
volumeControl :: MonadIO m => m ()
volumeControl = safeSpawnProg "pavucontrol"

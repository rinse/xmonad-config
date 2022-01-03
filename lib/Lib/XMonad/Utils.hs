{- |It is a collection of utilities on XMonad.

    Basically, they have generalized types instead of specifying 'X'.
-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Utils
    ( xstate
    , xwindowset
    , xconfig
    , workspaceIds
    , xconf
    , currentWorkspaceId
    , currentScreenId
    , screenIds
    ) where

import           Data.Functor    ((<&>))
import qualified Data.List       as L
import           XMonad
import qualified XMonad.StackSet as W


-- |Gets xstate. This is a synonym for 'get'.
xstate :: MonadState XState m => m XState
xstate = get

-- |Gets windowset
xwindowset :: MonadState XState m => m WindowSet
xwindowset = windowset <$> xstate

-- |Gets xconf. This is a synonym for 'ask'.
xconf :: MonadReader XConf m => m XConf
xconf = ask

-- |Gets xconfig
xconfig :: MonadReader XConf m => m (XConfig Layout)
xconfig = config <$> xconf

-- |Gets workspaceIds
workspaceIds :: MonadReader XConf m => m [WorkspaceId]
workspaceIds = workspaces <$> xconfig

-- |Gets the current workspace
currentWorkspaceId :: MonadState XState m => m WorkspaceId
currentWorkspaceId = xwindowset <&> W.current <&> W.workspace <&> W.tag

-- |Gets the current screen
currentScreenId :: MonadState XState m => m ScreenId
currentScreenId = xwindowset <&> W.current <&> W.screen

-- |Available screenIds
screenIds :: XState -> [ScreenId]
screenIds = L.sort . fmap W.screen . W.screens . windowset

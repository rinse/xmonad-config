{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Utils
    ( xstate
    , xwindowset
    , xconfig
    , workspaceIds
    , currentWorkspaceId
    , currentScreenId
    , screenIds
    ) where

import           Data.Functor    ((<&>))
import           Data.List
import           XMonad
import qualified XMonad.StackSet as W


-- |Gets xstate
xstate :: MonadState XState m => m XState
xstate = get

-- |Gets windowset
xwindowset :: MonadState XState m => m WindowSet
xwindowset = windowset <$> xstate

-- |Gets xconf
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
screenIds :: MonadState XState m => m [ScreenId]
screenIds = do
    s <- W.screens <$> xwindowset
    return $ sort $ W.screen <$> s

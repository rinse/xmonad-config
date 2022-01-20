{- |It is a collection of utilities on XMonad.

    Basically, they have generalized types instead of specifying 'X'.
-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Utils
    ( xstate
    , sortedScreenIds
    , screenIds
    , screens
    , passEnvAndState
    , (??)
    ) where

import qualified Data.List       as L
import           Lens.Micro.Extras
import           Lib.XMonad.Classes
import           XMonad
import qualified XMonad.StackSet as W

-- |Gets xstate. This is a synonym for 'get'.
xstate :: MonadState XState m => m XState
xstate = get

-- |Takes all screens from an environment.
sortedScreenIds :: (HasCurrent st, HasVisible st) => st -> [ScreenId]
sortedScreenIds st = L.sort $ screenIds st

-- |Takes ids of all screens from an environment.
screenIds :: (HasCurrent st, HasVisible st) => st -> [ScreenId]
screenIds = fmap W.screen . screens

-- |Takes all screens from an environment.
screens :: (HasCurrent st, HasVisible st) => st -> [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
screens st = view currentL st : view visibleL st

-- |Passes a current environment and state to a function.
-- |It doesn't change the current state.
passEnvAndState :: (MonadState st m, MonadReader env m) => (env -> st -> a) -> m a
passEnvAndState f = do
    env <- ask
    f env <$> get

infixl 1 ??
(??) :: Functor f => f (a -> b) -> a -> f b
f ?? a = fmap ($ a) f

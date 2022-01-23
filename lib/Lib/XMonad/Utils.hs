{- |It is a collection of utilities on XMonad.

    Basically, they have generalized types instead of specifying 'X'.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Lib.XMonad.Utils
    ( sortedScreenIds
    , screenIds
    , screens
    , passEnvAndState
    , (??)
    , uncons
    , allWorkspaces
    , findWorkspace
    , elemOnStack
    , insertUp
    , insertUpIf
    ) where

import           Control.Monad.State
import qualified Data.List           as L
import           Lens.Micro
import           Lens.Micro.Extras   as E
import           Lib.XMonad.Classes
import           XMonad
import qualified XMonad.StackSet     as W

-- |Takes all screens from an environment.
sortedScreenIds :: ( HasCurrent st (W.Screen i l a sid sd)
                   , HasVisible st [W.Screen i l a sid sd]
                   , Ord sid
                   ) => st -> [sid]
sortedScreenIds st = L.sort $ screenIds st

-- |Takes ids of all screens from an environment.
screenIds :: ( HasCurrent st (W.Screen i l a sid sd)
             , HasVisible st [W.Screen i l a sid sd]
             ) => st -> [sid]
screenIds = fmap W.screen . screens

-- |Takes all screens from an environment.
screens :: ( HasCurrent st (W.Screen i l a sid sd)
           , HasVisible st [W.Screen i l a sid sd]
           ) => st -> [W.Screen i l a sid sd]
screens st = E.view currentL st : E.view visibleL st

-- |Passes a current environment and state to a function.
-- |It doesn't change the current state.
passEnvAndState :: (MonadState st m, MonadReader env m) => (env -> st -> a) -> m a
passEnvAndState f = do
    env <- ask
    f env <$> get

infixl 1 ??
(??) :: Functor f => f (a -> b) -> a -> f b
f ?? a = fmap ($ a) f

-- |Produces the first element of the stack, and a stack of the remaining elements, if any.
-- |Try to move focus to the down, otherwise try to move focus to the up.
uncons :: W.Stack a -> (a, Maybe (W.Stack a))
uncons (W.Stack a up (x:xs)) = (a, pure $ W.Stack x up xs)
uncons (W.Stack a (x:xs) []) = (a, pure $ W.Stack x xs [])
uncons (W.Stack a [] [])     = (a, Nothing)

-- |An alternative of W.workspaces
-- |Produces all workspaces in a StackSet.
allWorkspaces :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasHidden st [W.Workspace i l a]
                 ) => st -> [W.Workspace i l a]
allWorkspaces st =
    let current = st ^. currentL
        visible = st ^. visibleL
        hidden = st ^. hiddenL
     in W.workspace current : fmap W.workspace visible <> hidden

-- |Find a workspace in windowSet
findWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasHidden st [W.Workspace i l a]
                 , Eq i
                 ) => i -> st -> Maybe (W.Workspace i l a)
findWorkspace workspaceId = L.find (\w -> W.tag w == workspaceId) . allWorkspaces

-- |`elem` but on `W.Stack`.
elemOnStack :: Eq a => a -> W.Stack a -> Bool
elemOnStack a W.Stack { W.up, W.focus = focused, W.down } = a == focused || L.elem a up || L.elem a down

-- |Insert an item into a `W.Stack`.
-- |The new item will be focused and a previously focused item will be down on the newly focused item.
insertUp :: a -> W.Stack a -> W.Stack a
insertUp a (W.Stack focused up down) = W.Stack a up (focused:down)

insertUpIf :: (a -> Bool) -> a -> W.Stack a -> W.Stack a
insertUpIf p a
    | p a       = insertUp a
    | otherwise = id

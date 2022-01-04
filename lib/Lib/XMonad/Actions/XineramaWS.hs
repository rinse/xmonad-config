{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Actions.XineramaWS
    ( initScreens
    , nextWS
    , prevWS
    , neighbourWorkspace
    , correspondence
    ) where

import           Control.Monad
import qualified Data.List          as L
import           Data.Maybe
import           Lens.Micro         (to)
import           Lens.Micro.Mtl     (use, view)
import           Lib.Utils
import           Lib.XMonad.Classes (HasWindowSet (..), HasWorkspaces (..))
import           Lib.XMonad.Utils
import           XMonad
import qualified XMonad.StackSet    as W


{- |Initializes screen-workspace correspondence.

    (screen0, workspace0), (screen1, workspace1), ..
-}
initScreens :: X ()
initScreens = switchScreen initialWorkspaces

-- |Go to the next workspace.
nextWS :: X ()
nextWS = switchScreen nextWorkspace

-- |Go to the previous workspace.
prevWS :: X ()
prevWS = switchScreen prevWorkspace

{- |Switches screen to the workspace.
    Does nothing when the workspace not found.
-}
switchScreen :: (ScreenId -> X (Maybe WorkspaceId)) -> X ()
switchScreen f =
    withCurrentScreen $ do
        ss <- (fmap . fmap) W.screen $ W.screens <$> xwindowset
        forM_ ss $ \s -> do
            xviewS s -- switch screens temporarily
            newW <- f s
            whenJust newW $ windows . W.greedyView
    where
    withCurrentScreen action = do
        csid <- currentScreenId
        r <- action
        xviewS csid
        return r

{- |Obtain an initial workspace of the given screen id.
    Returns Nothing when there is no screen for sid.
-}
initialWorkspaces :: (MonadState XState m, MonadReader XConf m)
                  => ScreenId -> m (Maybe WorkspaceId)
initialWorkspaces sId = do
    sIds <- use $ windowSetL . to screenIds
    wIds <- view workspacesL
    pure $ lookup sId (correspondence sIds wIds) >>= headMaybe

-- |The next workspace of the screen.
nextWorkspace :: (MonadState XState m, MonadReader XConf m)
              => ScreenId -> m (Maybe WorkspaceId)
nextWorkspace screenId = do
    sids <- use $ windowSetL . to screenIds
    allWorkspaceIds <- view workspacesL
    let a = correspondence sids allWorkspaceIds
    let wids = fromMaybe [] $ lookup screenId a
    currentWId <- currentWorkspaceId <$> xstate
    pure $ neighbourWorkspace wids currentWId (+ 1)

-- |The previous workspace of the screen.
prevWorkspace :: (MonadState XState m, MonadReader XConf m)
              => ScreenId -> m (Maybe WorkspaceId)
prevWorkspace screenId = do
    sids <- use $ windowSetL . to screenIds
    allWorkspaceIds <- view workspacesL
    let a = correspondence sids allWorkspaceIds
    let wids = fromMaybe [] $ lookup screenId a
    currentWId <- currentWorkspaceId <$> xstate
    pure $ neighbourWorkspace wids currentWId (subtract 1)

-- |Gets some neighbour workspaces
neighbourWorkspace :: Eq a => [a] -> a -> (Int -> Int) -> Maybe a
neighbourWorkspace wids currentWId f = do
    i <- L.elemIndex currentWId wids
    wids `at` f i

-- |Switches screens
xviewS :: ScreenId -> X ()
xviewS i = do
    s <- screenWorkspace i
    whenJust s $ windows . W.view

{- |Correspondence between screenId and workspaceId.
    Screen ids must not be empty.
-}
correspondence :: Ord a => [a] -> [b] -> [(a, [b])]
correspondence a = groupSort . zip (cycle a)

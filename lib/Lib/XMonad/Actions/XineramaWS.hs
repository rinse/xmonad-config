{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Actions.XineramaWS
    ( initScreens
    , nextWS
    , prevWS
    , stepElement
    , correspondence
    ) where

import           Control.Monad
import qualified Data.List          as L
import           Data.Maybe
import           Lens.Micro         (ix, (^?))
import           Lens.Micro.Mtl     (use, view)
import           Lib.Utils
import           Lib.XMonad.Classes (HasCurrentWorkspaceTag (..), HasScreenIds (..), HasWorkspaces (..))
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
initialWorkspaces :: (MonadState st m, MonadReader env m, HasScreenIds st, HasWorkspaces env)
                  => ScreenId -> m (Maybe WorkspaceId)
initialWorkspaces sId = do
    sIds <- use screenIdsG
    wIds <- view workspacesL
    pure $ lookup sId (correspondence sIds wIds) >>= headMaybe

-- |The next workspace of the screen.
nextWorkspace :: ( MonadState st m, MonadReader env m
                 , HasScreenIds st, HasCurrentWorkspaceTag st WorkspaceId
                 , HasWorkspaces env
                 ) => ScreenId -> m (Maybe WorkspaceId)
nextWorkspace screenId = do
    sids <- use screenIdsG
    allWorkspaceIds <- view workspacesL
    let wids = fromMaybe [] $ lookup screenId $ correspondence sids allWorkspaceIds
    currentWorkspaceTag <- use currentWorkspaceTagL
    pure $ stepElement (+ 1) wids currentWorkspaceTag

-- |The previous workspace of the screen.
prevWorkspace :: ( MonadState st m, MonadReader env m
                 , HasScreenIds st, HasCurrentWorkspaceTag st WorkspaceId
                 , HasWorkspaces env
                 ) => ScreenId -> m (Maybe WorkspaceId)
prevWorkspace screenId = do
    sids <- use screenIdsG
    allWorkspaceIds <- view workspacesL
    let wids = fromMaybe [] $ lookup screenId $ correspondence sids allWorkspaceIds
    currentWorkspaceTag <- use currentWorkspaceTagL
    pure $ stepElement (subtract 1) wids currentWorkspaceTag

-- |Steps an element of a list with an update function.
-- |The update function gets and returns an index.
stepElement :: Eq a => (Int -> Int) -> [a] -> a -> Maybe a
stepElement updateIndex l e = do
    i <- L.elemIndex e l
    l ^? ix (updateIndex i)

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

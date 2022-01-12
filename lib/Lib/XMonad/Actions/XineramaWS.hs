{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Actions.XineramaWS
    ( initScreens
    , nextWS
    , prevWS
    , initialWorkspaces    -- Only for testings
    , stepElement          -- Only for testings
    , stepWorkspace        -- Only for testings
    , workspaceIdsOfScreen -- Only for testings
    , correspondence       -- Only for testings
    ) where

import           Control.Monad
import qualified Data.List          as L
import           Lens.Micro         (ix, to, (^?), (^.))
import           Lens.Micro.Mtl     (use)
import           Lib.Utils
import           Lib.XMonad.Classes
import           Lib.XMonad.Lenses
import           Lib.XMonad.Utils
import           XMonad
import qualified XMonad.StackSet    as W


{- |Initializes screen-workspace correspondence.
    (screen0, workspace0), (screen1, workspace1), ..
-}
initScreens :: X ()
initScreens = switchScreen $ \screenId -> passEnvAndState initialWorkspaces ?? screenId

-- |Go to the next workspace.
nextWS :: X ()
nextWS = switchScreen $ \screenId -> passEnvAndState nextWorkspace ?? screenId

-- |Go to the previous workspace.
prevWS :: X ()
prevWS = switchScreen $ \screenId -> passEnvAndState prevWorkspace ?? screenId

-- |Switches all screens to a next workspace.
-- |The next workspace is fetched by an input function.
-- |When the function returns @Nothing@, does nothing to the screen.
switchScreen :: (ScreenId -> X (Maybe WorkspaceId)) -> X ()
switchScreen f = withCurrentScreen $ do
    sids <- use $ to screenIds
    forM_ sids $ \screenId -> do
        xviewS screenId -- switch screens temporarily
        newWorkspace <- f screenId
        whenJust newWorkspace $ windows . W.greedyView

-- |Stores a current screen and runs a given @action@.
-- |Then restores the screen and returns the value from the @action@.
withCurrentScreen :: X a -> X a
withCurrentScreen action = do
    originalScreenId <- use $ currentL . screenL
    r <- action
    xviewS originalScreenId
    pure r

{- |Obtain an initial workspace of the given screen id.
    Returns Nothing when there is no screen for sid.
-}
initialWorkspaces :: (HasCurrent st, HasVisible st, HasWorkspaces env) => env -> st -> ScreenId -> Maybe WorkspaceId
initialWorkspaces env st sId = do
    let sIds = sortedScreenIds st
        wIds = env ^. workspacesL
    lookup sId (correspondence sIds wIds) >>= headMaybe

-- |The next workspace of the current screen.
nextWorkspace :: (HasCurrent st, HasVisible st, HasWorkspaces env) => env -> st -> ScreenId -> Maybe WorkspaceId
nextWorkspace env st = stepWorkspace env st (+ 1)

-- |The previous workspace of the current screen.
prevWorkspace :: (HasCurrent st, HasVisible st, HasWorkspaces env) => env -> st -> ScreenId -> Maybe WorkspaceId
prevWorkspace env st = stepWorkspace env st (subtract 1)

-- |Steps workspaces of the current screen.
-- |The update function gets and returns an index of the workspace.
stepWorkspace :: (HasCurrent st, HasVisible st, HasWorkspaces env)
              => env -> st -> (Int -> Int) -> ScreenId -> Maybe WorkspaceId
stepWorkspace env st updateIndex screenId = do
    wids <- workspaceIdsOfScreen env st screenId
    let currentWorkspaceTag = st ^. currentL . workspaceL . tagL
    stepElement updateIndex wids currentWorkspaceTag

-- |Returns `[WorkspaceId]` of a given screen.
-- |Returns `Nothing` when the given screen does not exist.
workspaceIdsOfScreen :: (HasCurrent st, HasVisible st, HasWorkspaces env)
                     => env -> st -> ScreenId -> Maybe [WorkspaceId]
workspaceIdsOfScreen env st screenId =
    let sids = sortedScreenIds st
        allWorkspaceIds = env ^. workspacesL
     in lookup screenId $ correspondence sids allWorkspaceIds

-- |Steps an element of a list with an update function.
-- |The update function gets and returns an index.
stepElement :: Eq a => (Int -> Int) -> [a] -> a -> Maybe a
stepElement updateIndex l e = do
    i <- L.elemIndex e l
    l ^? ix (updateIndex i)

-- |Set focus to a given @ScreenId@.
-- |Do nothing when there is no such screen.
xviewS :: ScreenId -> X ()
xviewS i = do
    s <- screenWorkspace i
    whenJust s $ windows . W.view

{- |Correspondence between screenId and workspaceId.
    Screen ids must not be empty.
-}
correspondence :: Ord a => [a] -> [b] -> [(a, [b])]
correspondence a = groupSort . zip (cycle a)

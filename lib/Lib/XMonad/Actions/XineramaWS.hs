{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Actions.XineramaWS
    ( initScreens
    , initScreens'         -- Only for testings
    , nextWS
    , nextWS'              -- Only for testings
    , prevWS
    , prevWS'              -- Only for testings
    , switchScreen         -- Only for testings
    , withCurrentScreen    -- Only for testings
    , xviewS               -- Only for testings
    , initialWorkspaces    -- Only for testings
    , stepElement          -- Only for testings
    , stepWorkspace        -- Only for testings
    , workspaceIdsOfScreen -- Only for testings
    , correspondence       -- Only for testings
    ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.List           as L
import           Lens.Micro
import           Lens.Micro.Mtl      (use, (%=))
import           Lib.Utils
import           Lib.XMonad.Classes
import           Lib.XMonad.Lenses
import           Lib.XMonad.Utils
import           Lib.XMonad.XMock
import           XMonad
import qualified XMonad.StackSet     as W


-- |Initializes screen-workspace correspondence.
-- |(screen0, workspace0), (screen1, workspace1), ..
initScreens :: X ()
initScreens = do
    env <- ask
    windows $ toUpdateFunction env initScreens'

-- |A pure alternative of @initScreens@.
initScreens' :: (MonadState st m, MonadReader env m, HasWindowSet st, HasCurrent st, HasVisible st, HasWorkspaces env)
             => m ()
initScreens' = switchScreen $ \screenId -> passEnvAndState initialWorkspaces ?? screenId

-- |Go to the next workspace.
nextWS :: X ()
nextWS = do
    env <- ask
    windows $ toUpdateFunction env nextWS'

-- |A pure alternative of @nextWS@.
nextWS' :: (MonadState st m, MonadReader env m, HasWindowSet st, HasCurrent st, HasVisible st, HasWorkspaces env) => m ()
nextWS' = switchScreen $ \screenId -> passEnvAndState nextWorkspace ?? screenId

-- |Go to the previous workspace.
prevWS :: X ()
prevWS = do
    env <- ask
    windows $ toUpdateFunction env prevWS'

-- |A pure alternative of @prevWS@.
prevWS' :: (MonadState st m, MonadReader env m, HasWindowSet st, HasCurrent st, HasVisible st, HasWorkspaces env) => m ()
prevWS' = switchScreen $ \screenId -> passEnvAndState prevWorkspace ?? screenId

-- |Make a monadic action into a pure update function.
toUpdateFunction :: env -> XMock WindowSet env () -> WindowSet -> WindowSet
toUpdateFunction env m st = execXMock st env m

-- |Switches all screens to a next workspace.
-- |The next workspace is fetched by an input function.
-- |When the function returns @Nothing@, does nothing to the screen.
switchScreen :: (MonadState st m, HasWindowSet st, HasCurrent st, HasVisible st)
             => (ScreenId -> m (Maybe WorkspaceId)) -> m ()
switchScreen f = withCurrentScreen $ do
    sids <- use $ to screenIds
    forM_ sids $ \screenId -> do       -- switch each workspace on a screen to a workspace given by f
        windowSetL %= xviewS screenId -- switch screens temporarily
        maybeWorkspaceId <- f screenId
        whenJust maybeWorkspaceId $ \i ->
            windowSetL %= W.greedyView i

-- |Stores a current screen and runs a given @action@.
-- |Then restores the screen and returns the value from the @action@.
-- |Requires update with the @windows@ function to apply the change.
withCurrentScreen :: (MonadState st m, HasWindowSet st, HasCurrent st) => m a -> m a
withCurrentScreen action = do
    originalScreenId <- use $ currentL . screenL
    r <- action
    windowSetL %= xviewS originalScreenId
    pure r

-- |Set focus to a given @ScreenId@.
-- |Do nothing when there is no such screen.
xviewS :: ScreenId -> WindowSet -> WindowSet
xviewS i windowSet = do
    let maybeWorkspaceId = screenWorkspace' windowSet i
    maybe windowSet (`W.view` windowSet) maybeWorkspaceId

-- |A pure alternative of @screenWorkspace@.
screenWorkspace' :: HasVisible env => env -> ScreenId -> Maybe WorkspaceId
screenWorkspace' env screenId =
    let visibleScreens = env ^. visibleL
        maybeScreen = L.find (\s -> W.screen s == screenId) visibleScreens
     in W.tag . W.workspace <$> maybeScreen

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

-- |Steps workspaces of a screenId.
-- |The update function gets and returns an index of the workspace.
stepWorkspace :: (HasCurrent st, HasVisible st, HasWorkspaces env)
              => env -> st -> (Int -> Int) -> ScreenId -> Maybe WorkspaceId
stepWorkspace env st updateIndex screenId = do
    screen <- L.find (\s -> W.screen s == screenId) (screens st)
    let workspaceTag = screen ^. workspaceL. tagL
    wids <- workspaceIdsOfScreen env st screenId
    stepElement updateIndex wids workspaceTag

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

{- |Correspondence between screenId and workspaceId.
    Screen ids must not be empty.
-}
correspondence :: Ord a => [a] -> [b] -> [(a, [b])]
correspondence a = groupSort . zip (cycle a)

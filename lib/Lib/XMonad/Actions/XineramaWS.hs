{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Lib.XMonad.Actions.XineramaWS
    ( initScreens
    , initScreens'         -- Only for testings
    , nextWS
    , nextWS'              -- Only for testings
    , prevWS
    , prevWS'              -- Only for testings
    , shiftAndMoveToNextWS
    , shiftAndMoveToPrevWS
    , shiftToWorkspace     -- Only for testings
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
import           Data.Maybe
import           Lens.Micro
import           Lens.Micro.Mtl      (use, (%=), (.=))
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
initScreens' :: ( MonadState st m
                , MonadReader env m
                , HasStackSet st (W.StackSet i l a sid sd)
                , HasCurrent st (W.Screen i l a sid sd)
                , HasVisible st [W.Screen i l a sid sd]
                , HasWorkspaces env i
                , Eq i, Ord sid
                ) => m ()
initScreens' = switchScreen $ \screenId -> passEnvAndState initialWorkspaces ?? screenId

-- |Go to the next workspace.
nextWS :: X ()
nextWS = do
    env <- ask
    windows $ toUpdateFunction env nextWS'

-- |A pure alternative of @nextWS@.
nextWS' :: ( MonadState st m, MonadReader env m
           , HasStackSet st (W.StackSet i l a sid sd)
           , HasCurrent st (W.Screen i l a sid sd)
           , HasVisible st [W.Screen i l a sid sd]
           , HasWorkspaces env i
           , Eq i, Ord sid
           ) => m ()
nextWS' = switchScreen $ \screenId -> passEnvAndState nextWorkspace ?? screenId

-- |Go to the previous workspace.
prevWS :: X ()
prevWS = do
    env <- ask
    windows $ toUpdateFunction env prevWS'

-- |A pure alternative of @prevWS@.
prevWS' :: ( MonadState st m, MonadReader env m
           , HasStackSet st (W.StackSet i l a sid sd)
           , HasCurrent st (W.Screen i l a sid sd)
           , HasVisible st [W.Screen i l a sid sd]
           , HasWorkspaces env i
           , Eq i, Ord sid
           ) => m ()
prevWS' = switchScreen $ \screenId -> passEnvAndState prevWorkspace ?? screenId

-- |Make a monadic action into a pure update function.
toUpdateFunction :: env -> XMock WindowSet env () -> WindowSet -> WindowSet
toUpdateFunction env m st = execXMock st env m

-- |Shift the currently focused window to a next workspace and move to the workspace.
shiftAndMoveToNextWS :: X ()
shiftAndMoveToNextWS = ask >>= windows . shiftAndMoveToNextWS'

-- |A pure alternative of `shiftAndMoveToNextWS`.
shiftAndMoveToNextWS' :: ( HasStackSet st (W.StackSet i l a sid sd)
                         , HasCurrent st (W.Screen i l a sid sd)
                         , HasVisible st [W.Screen i l a sid sd]
                         , HasHidden st [W.Workspace i l a]
                         , HasWorkspaces env i
                         , Eq i, Eq a, Ord sid
                         ) => env -> st -> st
shiftAndMoveToNextWS' env st = nextWS'' $ shiftToNextWS' env st
    where
    nextWS'' s = execXMock s env nextWS'

-- |Shift the currently focused window to a next workspace.
shiftToNextWS' :: ( HasCurrent st (W.Screen i l a sid sd)
                  , HasVisible st [W.Screen i l a sid sd]
                  , HasHidden st [W.Workspace i l a]
                  , HasWorkspaces env i
                  , Eq i, Eq a, Ord sid
                  ) => env -> st -> st
shiftToNextWS' env st =
    let currentScreen = st ^. currentL . screenL
        maybeDestinationTag = nextWorkspace env st currentScreen
     in maybe st (`shiftToWorkspace` st) maybeDestinationTag

-- |Shift the currently focused window to a previous workspace and move to the workspace.
shiftAndMoveToPrevWS :: X ()
shiftAndMoveToPrevWS = ask >>= windows . shiftAndMoveToPrevWS'

-- |A pure alternative of `shiftAndMoveToPrevWS`.
shiftAndMoveToPrevWS' :: ( HasStackSet st (W.StackSet i l a sid sd)
                         , HasCurrent st (W.Screen i l a sid sd)
                         , HasVisible st [W.Screen i l a sid sd]
                         , HasHidden st [W.Workspace i l a]
                         , HasWorkspaces env i
                         , Eq i, Eq a, Ord sid
                         ) => env -> st -> st
shiftAndMoveToPrevWS' env st = prevWS'' $ shiftToPrevWS' env st
    where
    prevWS'' s = execXMock s env prevWS'

-- |Shift the currently focused window to a previous workspace.
shiftToPrevWS' :: ( HasCurrent st (W.Screen i l a sid sd)
                  , HasVisible st [W.Screen i l a sid sd]
                  , HasHidden st [W.Workspace i l a]
                  , HasWorkspaces env i
                  , Eq i, Eq a, Ord sid
                  ) => env -> st -> st
shiftToPrevWS' env st =
    let currentScreen = st ^. currentL . screenL
        maybeDestinationTag = prevWorkspace env st currentScreen
     in maybe st (`shiftToWorkspace` st) maybeDestinationTag

-- |Shifts a current window to a destination workspace.
shiftToWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                    , HasVisible st [W.Screen i l a sid sd]
                    , HasHidden st [W.Workspace i l a]
                    , Eq i, Eq a
                    ) => i -> st -> st
shiftToWorkspace destinationTag = execState $ do
    -- make sure the destination does exist in WindowSet
    destinationExist <- gets $ isJust . findWorkspace destinationTag
    when destinationExist $ do
        -- remove a window from a workspace and put the window on another window
        maybeWindow <- popCurrentlyFocusedWindow
        forM_ maybeWindow $ \window ->
            state' $ insertWindowUpOnWindowSet window destinationTag

-- |Pops a currently focused window.
popCurrentlyFocusedWindow :: (MonadState st m, HasCurrent st (W.Screen i l a sid sd)) => m (Maybe a)
popCurrentlyFocusedWindow = do
    maybeStack <- use $ currentL . workspaceL . stackL
    forM maybeStack $ \oldStack -> do   -- does nothing when no window is currently focused
        let (window, newStack) = uncons oldStack
        currentL . workspaceL . stackL .= newStack
        pure window

-- |`insertUpIfNotElem` but it's lifted.
insertWindowUpOnWindowSet :: ( HasCurrent st (W.Screen i l a sid sd)
                             , HasVisible st [W.Screen i l a sid sd]
                             , HasHidden st [W.Workspace i l a]
                             , Eq i, Eq a
                             ) => a -> i -> st -> st
insertWindowUpOnWindowSet window destinationTag = execState $ do
    currentL . workspaceL %= putWindowOnWorkspaceIf
    visibleL . each . workspaceL %= putWindowOnWorkspaceIf
    hiddenL . each %= putWindowOnWorkspaceIf
    where
    putWindowOnWorkspaceIf ws =
        if W.tag ws /= destinationTag
            then ws
            else insertWindowUpOnWorkspace window ws

-- |`insertUpIfNotElem` but it's lifted.
insertWindowUpOnWorkspace :: Eq a => a -> W.Workspace i l a -> W.Workspace i l a
insertWindowUpOnWorkspace window W.Workspace { W.tag, W.layout, W.stack } =
    W.Workspace
        { W.tag = tag
        , W.layout = layout
        , W.stack = pure $ insertUpIfNotElem' window stack
        }

-- |Puts an item into a stack which can be empty.
insertUpIfNotElem' :: Eq a => a -> Maybe (W.Stack a) -> W.Stack a
insertUpIfNotElem' a Nothing      = W.Stack a [] []
insertUpIfNotElem' a (Just stack) = insertUpIfNotElem a stack

-- |Puts an item into a stack, if the item not presents already.
insertUpIfNotElem :: Eq a => a -> W.Stack a -> W.Stack a
insertUpIfNotElem a st = insertUpIf elemNotOnStack a st
    where elemNotOnStack b = not $ elemOnStack b st

-- |Switches all screens to a next workspace.
-- |The next workspace is fetched by an input function.
-- |When the function returns @Nothing@, does nothing to the screen.
switchScreen :: ( MonadState st m
                , HasStackSet st (W.StackSet i l a sid sd)
                , HasCurrent st (W.Screen i l a sid sd)
                , HasVisible st [W.Screen i l a sid sd]
                , Eq i, Eq sid
                ) => (sid -> m (Maybe i)) -> m ()
switchScreen f = withCurrentScreen $ do
    sids <- use $ to screenIds
    forM_ sids $ \screenId -> do       -- switch each workspace on a screen to a workspace given by f
        stackSetL %= xviewS screenId -- switch screens temporarily
        maybeWorkspaceId <- f screenId
        whenJust maybeWorkspaceId $ \i ->
            stackSetL %= W.greedyView i

-- |Stores a current screen and runs a given @action@.
-- |Then restores the screen and returns the value from the @action@.
-- |Requires update with the @windows@ function to apply the change.
withCurrentScreen :: ( MonadState st m
                     , HasStackSet st (W.StackSet i l a sid sd)
                     , HasCurrent st (W.Screen i l a sid sd)
                     , Eq i, Eq sid
                     ) => m b -> m b
withCurrentScreen action = do
    originalScreenId <- use $ currentL . screenL
    r <- action
    stackSetL %= xviewS originalScreenId
    pure r

-- |Set focus to a given @ScreenId@.
-- |Do nothing when there is no such screen.
xviewS :: (Eq i, Eq sid) => sid -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
xviewS i windowSet = do
    let maybeWorkspaceId = screenWorkspace' windowSet i
    maybe windowSet (`W.view` windowSet) maybeWorkspaceId

-- |A pure alternative of @screenWorkspace@.
screenWorkspace' :: ( HasVisible env [W.Screen i l a sid sd]
                    , Eq sid
                    ) => env -> sid -> Maybe i
screenWorkspace' env screenId =
    let visibleScreens = env ^. visibleL
        maybeScreen = L.find (\s -> W.screen s == screenId) visibleScreens
     in W.tag . W.workspace <$> maybeScreen

{- |Obtain an initial workspace of the given screen id.
    Returns Nothing when there is no screen for sid.
-}
initialWorkspaces :: (HasCurrent st (W.Screen i l a sid sd)
                    , HasVisible st [W.Screen i l a sid sd]
                    , HasWorkspaces env i
                    , Ord sid
                    ) => env -> st -> sid -> Maybe i
initialWorkspaces env st sId = do
    let sIds = sortedScreenIds st
        wIds = env ^. workspacesL
    lookup sId (correspondence sIds wIds) >>= headMaybe

-- |The next workspace of the current screen.
nextWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasWorkspaces env i
                 , Eq i, Ord sid
                 ) => env -> st -> sid -> Maybe i
nextWorkspace env st = stepWorkspace env st (+ 1)

-- |The previous workspace of the current screen.
prevWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasWorkspaces env i
                 , Eq i, Ord sid
                 ) => env -> st -> sid -> Maybe i
prevWorkspace env st = stepWorkspace env st (subtract 1)

-- |Steps workspaces of a screenId.
-- |The update function gets and returns an index of the workspace.
stepWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasWorkspaces env i
                 , Eq i, Ord sid
                 ) => env -> st -> (Int -> Int) -> sid -> Maybe i
stepWorkspace env st updateIndex screenId = do
    screen <- L.find (\s -> W.screen s == screenId) (screens st)
    let workspaceTag = screen ^. workspaceL . tagL
    wids <- workspaceIdsOfScreen env st screenId
    stepElement updateIndex wids workspaceTag

-- |Returns `[WorkspaceId]` of a given screen.
-- |Returns `Nothing` when the given screen does not exist.
workspaceIdsOfScreen :: ( HasCurrent st (W.Screen i l a sid sd)
                        , HasVisible st [W.Screen i l a sid sd]
                        , HasWorkspaces env i
                        , Ord sid
                        ) => env -> st -> sid -> Maybe [i]
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

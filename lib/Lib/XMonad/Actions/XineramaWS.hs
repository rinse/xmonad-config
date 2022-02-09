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
import           Data.Foldable
import           Data.Traversable
import           Lens.Micro
import           Lens.Micro.Mtl      (use, (%=), (.=))
import           Lib.Utils
import           Lib.XMonad.Classes
import           Lib.XMonad.Lenses
import           Lib.XMonad.Utils
import           XMonad
import qualified XMonad.StackSet     as W

-- |Initializes screen-workspace correspondence.
-- |(screen0, workspace0), (screen1, workspace1), ..
initScreens :: X ()
initScreens = ask >>= windows . initScreens'

-- |A pure alternative of @initScreens@.
initScreens' :: ( HasStackSet st (W.StackSet i l a sid sd)
                , HasCurrent st (W.Screen i l a sid sd)
                , HasVisible st [W.Screen i l a sid sd]
                , HasWorkspaces env i
                , Eq i, Ord sid
                ) => env -> st -> st
initScreens' env = execState $
    switchScreen $ \sid ->
        gets $ initialWorkspaces sid env

-- |Go to the next workspace.
nextWS :: X ()
nextWS = ask >>= (windows . execState) . nextWS'

-- |A pure alternative of @nextWS@.
nextWS' :: ( MonadState st m
           , HasStackSet st (W.StackSet i l a sid sd)
           , HasCurrent st (W.Screen i l a sid sd)
           , HasVisible st [W.Screen i l a sid sd]
           , HasWorkspaces env i
           , Eq i, Ord sid
           ) => env -> m Bool
nextWS' env = switchScreen $ \sid ->
    gets $ nextWorkspace sid env

-- |Go to the previous workspace.
prevWS :: X ()
prevWS = ask >>= (windows . execState) . prevWS'

-- |A pure alternative of @prevWS@.
prevWS' :: ( MonadState st m
           , HasStackSet st (W.StackSet i l a sid sd)
           , HasCurrent st (W.Screen i l a sid sd)
           , HasVisible st [W.Screen i l a sid sd]
           , HasWorkspaces env i
           , Eq i, Ord sid
           ) => env -> m Bool
prevWS' env = switchScreen $ \sid ->
    gets $ prevWorkspace sid env

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
shiftAndMoveToNextWS' env st =
    -- Try to nextWS', then do shiftToNextWS and nextWS'
    if evalState (nextWS' env) st
    then flip execState st $ do
        state' $ shiftToNextWS' env
        void $ nextWS' env
    else st

-- |Shift the currently focused window to a next workspace.
shiftToNextWS' :: ( HasCurrent st (W.Screen i l a sid sd)
                  , HasVisible st [W.Screen i l a sid sd]
                  , HasHidden st [W.Workspace i l a]
                  , HasWorkspaces env i
                  , Eq i, Eq a, Ord sid
                  ) => env -> st -> st
shiftToNextWS' env st =
    let currentScreen = st ^. currentL . screenL
        maybeDestinationTag = nextWorkspace currentScreen env st
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
shiftAndMoveToPrevWS' env st =
    -- Try to prevWS', then do shiftToNextWS and nextWS'
    if evalState (prevWS' env) st
    then flip execState st $ do
        state' $ shiftToPrevWS' env
        void $ prevWS' env
    else st

-- |Shift the currently focused window to a previous workspace.
shiftToPrevWS' :: ( HasCurrent st (W.Screen i l a sid sd)
                  , HasVisible st [W.Screen i l a sid sd]
                  , HasHidden st [W.Workspace i l a]
                  , HasWorkspaces env i
                  , Eq i, Eq a, Ord sid
                  ) => env -> st -> st
shiftToPrevWS' env st =
    let currentScreen = st ^. currentL . screenL
        maybeDestinationTag = prevWorkspace currentScreen env st
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

-- |Switches each screens to a workspace.
-- |
-- |The next workspace is given by a function.
-- |None of screen switches when any of a screen doesn't have a next workspace.
-- |Returns @True@ if the screens successfully switched, returns @False@ otherwise.
switchScreen :: ( MonadState st m
                , HasStackSet st (W.StackSet i l a sid sd)
                , HasCurrent st (W.Screen i l a sid sd)
                , HasVisible st [W.Screen i l a sid sd]
                , Eq i, Eq sid
                ) => (sid -> m (Maybe i)) -> m Bool
switchScreen f = withCurrentScreen $ do
    sids <- use $ to screenIds
    listOfMaybe <- for sids $ \screenId -> do
        maybeWorkspaceId <- f screenId
        for maybeWorkspaceId $ \workspaceId ->
            pure (screenId, workspaceId)
    let maybeOfList = sequenceA listOfMaybe
    ret <- for maybeOfList $ \list -> do
        for_ list $ \(screenId, i) -> do
            stackSetL %= xviewS screenId -- switch screens temporarily
            stackSetL %= W.greedyView i
    pure $ isJust ret

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
                    ) => sid -> env -> st -> Maybe i
initialWorkspaces sId env st = do
    let sIds = sortedScreenIds st
        wIds = env ^. workspacesL
    lookup sId (correspondence sIds wIds) >>= headMaybe

-- |The next workspace of the current screen.
nextWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasWorkspaces env i
                 , Eq i, Ord sid
                 ) => sid -> env -> st -> Maybe i
nextWorkspace = stepWorkspace (+ 1)

-- |The previous workspace of the current screen.
prevWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasWorkspaces env i
                 , Eq i, Ord sid
                 ) => sid -> env -> st -> Maybe i
prevWorkspace = stepWorkspace (subtract 1)

-- |Steps workspaces of a screenId.
-- |The update function gets and returns an index of the workspace.
stepWorkspace :: ( HasCurrent st (W.Screen i l a sid sd)
                 , HasVisible st [W.Screen i l a sid sd]
                 , HasWorkspaces env i
                 , Eq i, Ord sid
                 ) => (Int -> Int) -> sid -> env -> st -> Maybe i
stepWorkspace updateIndex screenId env st = do
    screen <- L.find (\s -> W.screen s == screenId) (screens st)
    let workspaceTag = screen ^. workspaceL . tagL
    wids <- workspaceIdsOfScreen screenId env st
    stepElement updateIndex wids workspaceTag

-- |Returns `[WorkspaceId]` of a given screen.
-- |Returns `Nothing` when the given screen does not exist.
workspaceIdsOfScreen :: ( HasCurrent st (W.Screen i l a sid sd)
                        , HasVisible st [W.Screen i l a sid sd]
                        , HasWorkspaces env i
                        , Ord sid
                        ) => sid -> env -> st -> Maybe [i]
workspaceIdsOfScreen screenId env st =
    let sids = sortedScreenIds st
        allWorkspaceIds = env ^. workspacesL
     in lookup screenId $ correspondence sids allWorkspaceIds

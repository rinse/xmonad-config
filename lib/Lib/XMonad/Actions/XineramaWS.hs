{-# LANGUAGE FlexibleContexts #-}

module Lib.XMonad.Actions.XineramaWS
    ( initScreens
    , nextWS
    , prevWS
    ) where

import           Control.Monad
import           Data.Functor      ((<&>))
import           Data.List
import           Data.Maybe        (listToMaybe)
import           XMonad
import           XMonad.Operations (screenWorkspace)
import qualified XMonad.StackSet   as W


{- |Initializes screen-workspace correspondence.
    screen0: workspace0, screen1: workspace1, ..
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
initialWorkspaces sid = headMaybe <$> getCorresponding sid

-- |The safe variant of head :: [a] -> a
headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

-- |The next workspace of the screen.
nextWorkspace :: (MonadState XState m, MonadReader XConf m)
              => ScreenId -> m (Maybe WorkspaceId)
nextWorkspace = neighbourWorkspace (+ 1)

-- |The previous workspace of the screen.
prevWorkspace :: (MonadState XState m, MonadReader XConf m)
              => ScreenId -> m (Maybe WorkspaceId)
prevWorkspace = neighbourWorkspace (subtract 1)

-- |Gets some neighbour workspaces
neighbourWorkspace :: (MonadState XState m, MonadReader XConf m)
                   => (Int -> Int) -> ScreenId -> m (Maybe WorkspaceId)
neighbourWorkspace f sid = do
    cwid <- currentWorkspaceId
    wids <- getCorresponding sid
    return $ do
        i <- elemIndex cwid wids
        wids `at` f i

-- |Safe (!!)
at :: [a] -> Int -> Maybe a
at xs n
    | n < length xs = Just $ xs !! n
    | otherwise = Nothing

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

-- |Switches screens
xviewS :: ScreenId -> X ()
xviewS i = do
    s <- screenWorkspace i
    whenJust s $ windows . W.view

-- |Gets corresponding workspaces to the given screen.
getCorresponding :: (MonadState XState m, MonadReader XConf m)
                 => ScreenId -> m [WorkspaceId]
getCorresponding sid = maybe [] snd . find f <$> correspondence
    where
    f :: (ScreenId, [WorkspaceId]) -> Bool
    f = (sid ==) . fst

{- |Correnspondence between screenId and workspaceId.
    WorkspaceIds must not be empty.
-}
correspondence :: (MonadState XState m, MonadReader XConf m)
               => m [(ScreenId, [WorkspaceId])]
correspondence = do
    sids <- screenIds
    groupSort . zip (cycle sids) <$> workspaceIds

-- |Sort on a key and group with the key
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = groupWithKeyBy (==) . sortOn fst

-- |Similar to groupBy but with a key
groupWithKeyBy :: (k -> k -> Bool) -> [(k, v)] -> [(k, [v])]
groupWithKeyBy _ [] = []
groupWithKeyBy p ((fx, sx):xs) =
    let (ys, zs) = span (p' fx) xs
     in (fx, sx : (snd <$> ys)) : groupWithKeyBy p zs
    where
    p' x y = p x $ fst y

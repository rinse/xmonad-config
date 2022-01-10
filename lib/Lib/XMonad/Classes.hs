{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.XMonad.Classes
    ( HasConfig (..)
    , HasWorkspaces (..)
    , HasWindowSet (..)
    , HasCurrent (..)
    , HasVisible (..)
    , HasHidden (..)
    ) where

import           Lens.Micro
import           XMonad
import qualified XMonad.StackSet as W

idLens :: Lens' a a
idLens = lens id $ \_ y -> y

class HasConfig s where
    configL :: Lens' s (XConfig Layout)

class HasWorkspaces s where
    workspacesL :: Lens' s [WorkspaceId]

class HasWindowSet s where
    windowSetL :: Lens' s WindowSet

instance HasWindowSet WindowSet where
    windowSetL = idLens

type WScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

class HasCurrent s where
    currentL :: Lens' s WScreen

class HasVisible s where
    visibleL :: Lens' s [WScreen]

type WWorkspace = W.Workspace WorkspaceId (Layout Window) Window

class HasHidden s where
    hiddenL :: Lens' s [WWorkspace]

instance HasConfig XConf where
    configL = lens config $ \x y -> x { config = y }

instance HasWorkspaces (XConfig l) where
    workspacesL = lens workspaces $ \x y -> x { workspaces = y }

instance HasWorkspaces XConf where
    workspacesL = configL . workspacesL

instance HasWindowSet XState where
    windowSetL = lens windowset $ \x y -> x { windowset = y }

instance HasCurrent WindowSet where
    currentL = lens W.current $ \x y -> x { W.current = y }

instance HasCurrent XState where
    currentL = windowSetL . currentL

instance HasVisible WindowSet where
    visibleL = lens W.visible $ \x y -> x { W.visible = y }

instance HasVisible XState where
    visibleL = windowSetL . visibleL

instance HasHidden WindowSet where
    hiddenL = lens W.hidden $ \x y -> x { W.hidden = y }

instance HasHidden XState where
    hiddenL = windowSetL . hiddenL

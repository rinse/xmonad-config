{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lib.XMonad.Classes
    ( HasConfig (..)
    , HasWorkspaces (..)
    , HasStackSet (..)
    , HasCurrent (..)
    , HasVisible (..)
    , HasHidden (..)
    ) where

import           Lens.Micro
import           XMonad
import qualified XMonad.StackSet as W

idLens :: Lens' a a
idLens = lens id $ \_ y -> y

class HasConfig s a | s -> a where
    configL :: Lens' s a

class HasWorkspaces s i | s -> i where
    workspacesL :: Lens' s [i]

class HasStackSet s a | s -> a where
    stackSetL :: Lens' s a

instance HasStackSet (W.StackSet i l a sid sd) (W.StackSet i l a sid sd) where
    stackSetL = idLens

class HasCurrent s a | s -> a where
    currentL :: Lens' s a

class HasVisible s a | s -> a where
    visibleL :: Lens' s a

class HasHidden s a | s -> a where
    hiddenL :: Lens' s a

instance HasConfig XConf (XConfig Layout) where
    configL = lens config $ \x y -> x { config = y }

instance HasWorkspaces (XConfig l) WorkspaceId where
    workspacesL = lens workspaces $ \x y -> x { workspaces = y }

instance HasWorkspaces XConf WorkspaceId where
    workspacesL = configL . workspacesL

instance HasStackSet XState WindowSet where
    stackSetL = lens windowset $ \x y -> x { windowset = y }

instance HasCurrent (W.StackSet i l a sid sd) (W.Screen i l a sid sd) where
    currentL = lens W.current $ \x y -> x { W.current = y }

type WScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

instance HasCurrent XState WScreen where
    currentL = stackSetL . currentL

instance HasVisible (W.StackSet i l a sid sd) [W.Screen i l a sid sd] where
    visibleL = lens W.visible $ \x y -> x { W.visible = y }

instance HasVisible XState [WScreen] where
    visibleL = stackSetL . visibleL

instance HasHidden (W.StackSet i l a sid sd) [W.Workspace i l a] where
    hiddenL = lens W.hidden $ \x y -> x { W.hidden = y }

instance HasHidden XState [W.Workspace WorkspaceId (Layout Window) Window] where
    hiddenL = stackSetL . hiddenL

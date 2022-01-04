{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lib.XMonad.Classes where

import           Lens.Micro
import           Lib.XMonad.Utils
import           XMonad
import           XMonad.StackSet (current, tag, workspace)

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

class HasScreenIds s where
    screenIdsG :: SimpleGetter s [ScreenId]

class HasCurrentWorkspaceTag s i | s -> i where
    currentWorkspaceTagL :: Lens' s i

instance HasConfig XConf where
    configL = lens config $ \x y -> x { config = y }

instance HasWorkspaces (XConfig l) where
    workspacesL = lens workspaces $ \x y -> x { workspaces = y }

instance HasWorkspaces XConf where
    workspacesL = configL . workspacesL

instance HasWindowSet XState where
    windowSetL = lens windowset $ \x y -> x { windowset = y }

instance HasScreenIds XState where
    screenIdsG = windowSetL . to screenIds

instance HasCurrentWorkspaceTag XState WorkspaceId where
    currentWorkspaceTagL = windowSetL . currentL . workspaceL . tagL
        where
        currentL = lens current $ \x y -> x { current = y }
        workspaceL = lens workspace $ \x y -> x { workspace = y }
        tagL = lens tag $ \x y -> x { tag = y }

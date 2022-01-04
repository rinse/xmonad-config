{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.XMonad.Classes where

import           Lens.Micro
import           XMonad

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

instance HasConfig XConf where
    configL = lens config $ \x y -> x { config = y }

instance HasWorkspaces (XConfig l) where
    workspacesL = lens workspaces $ \x y -> x { workspaces = y }

instance HasWorkspaces XConf where
    workspacesL = configL . workspacesL

instance HasWindowSet XState where
    windowSetL = lens windowset $ \x y -> x { windowset = y }

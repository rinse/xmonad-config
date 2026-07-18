{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib.XMonad.ScreensMock
    ( ScreensMock (..)
    , mkMockScreen
    ) where

import           Lens.Micro
import           Lib.XMonad.Classes
import           XMonad
import qualified XMonad.Core        as W
import qualified XMonad.StackSet    as W

type WScreen = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

data ScreensMock i l a sid sd = ScreensMock
    { _current :: W.Screen i l a sid sd
    , _visible :: [W.Screen i l a sid sd]
    }

instance HasCurrent (ScreensMock i l a sid sd) (W.Screen i l a sid sd) where
    currentL = lens _current $ \x y -> x { _current = y }

instance HasVisible (ScreensMock i l a sid sd) [W.Screen i l a sid sd] where
    visibleL = lens _visible $ \x y -> x { _visible = y }

mkMockScreen :: ScreenId -> W.WorkspaceId -> WScreen
mkMockScreen screenId workspaceId = W.Screen
    { W.workspace = W.Workspace
        { W.tag = workspaceId
        , W.layout = Layout Full
        , W.stack = Nothing
        }
    , W.screen = screenId
    , W.screenDetail = W.SD $ Rectangle 0 0 0 0
    }

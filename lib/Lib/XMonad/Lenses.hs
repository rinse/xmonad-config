{-|
    Lenses for XState and XConf.
    See also `Lib.XMonad.Classes`.
-}
module Lib.XMonad.Lenses
    ( screenL
    , tagL
    , stackL
    , focusL
    , workspaceL
    ) where

import           Lens.Micro
import qualified XMonad.StackSet as W

workspaceL  :: Lens' (W.Screen i l a sid sd) (W.Workspace i l a)
workspaceL = lens W.workspace $ \x y -> x { W.workspace = y }

tagL :: Lens' (W.Workspace i l a) i
tagL = lens W.tag $ \x y -> x { W.tag = y }

stackL :: Lens' (W.Workspace i l a) (Maybe (W.Stack a))
stackL = lens W.stack $ \x y -> x { W.stack = y }

focusL :: Lens' (W.Stack a) a
focusL = lens W.focus $ \x y -> x { W.focus = y }

screenL :: Lens' (W.Screen i l a sid sd) sid
screenL = lens W.screen $ \x y -> x { W.screen = y }

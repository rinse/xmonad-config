{-|
    Lenses for XState and XConf.
    See also `Lib.XMonad.Classes`.
-}
module Lib.XMonad.Lenses
    ( screenL
    , tagL
    , workspaceL
    ) where

import           Lens.Micro
import qualified XMonad.StackSet as W

workspaceL  :: Lens' (W.Screen i l a sid sd) (W.Workspace i l a)
workspaceL = lens W.workspace $ \x y -> x { W.workspace = y }

tagL :: Lens' (W.Workspace i (l a) a) i
tagL = lens W.tag $ \x y -> x { W.tag = y }

screenL :: Lens' (W.Screen i l a sid sd) sid
screenL = lens W.screen $ \x y -> x { W.screen = y }

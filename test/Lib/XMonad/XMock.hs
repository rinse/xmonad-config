{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Lib.XMonad.XMock
    ( XMockT (..), runXMockT, execXMockT
    , XMock, runXMock, execXMock
    ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import           Lens.Micro
import           Lib.XMonad.Classes
import qualified XMonad                as X

newtype XMockT st env m a = XMockT
    { unXMockT :: ReaderT env (StateT st m) a
    } deriving (Functor)

runXMockT :: env -> st -> XMockT st env m a -> m (a, st)
runXMockT env st m = runStateT (runReaderT (unXMockT m) env) st

execXMockT :: Functor m => env -> st -> XMockT st env m a -> m st
execXMockT env st m = snd <$> runXMockT env st m

type XMock st env = XMockT st env Identity

runXMock :: env -> st -> XMock st env a -> (a, st)
runXMock env st m = runIdentity $ runXMockT  env st m

execXMock :: env -> st -> XMock st env a -> st
execXMock env st m = snd $ runXMock env st m

instance Monad m => Applicative (XMockT st env m) where
    pure = XMockT . pure
    XMockT f <*> XMockT a = XMockT $ f <*> a

instance Monad m => Monad (XMockT st env m) where
    XMockT a >>= f = XMockT $ a >>= unXMockT . f

instance MonadTrans (XMockT st env) where
    lift = XMockT . lift . lift

instance Monad m => MonadReader env (XMockT st env m) where
    ask = XMockT ask
    local f = XMockT . local f . unXMockT

instance Monad m => MonadState st (XMockT st env m) where
    get = XMockT . lift $ get
    put = XMockT . lift . put

newtype WorkspaceIds = WorkspaceIds
    { unWorkspaceIds :: [X.WorkspaceId]
    } deriving (Show, Read, Eq, Ord)

instance HasWorkspaces WorkspaceIds where
    workspacesL = lens unWorkspaceIds $ \x y -> x { unWorkspaceIds = y }

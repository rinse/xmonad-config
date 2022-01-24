module Lib.Utils
    ( headMaybe
    , groupSort
    , groupWithKeyBy
    , correspondence
    , state'
    , stepElement
    ) where

import           Control.Monad.State
import qualified Data.List           as L
import           Data.Maybe          (listToMaybe)
import           Lens.Micro

{- $setup
    >>> import Lens.Micro
-}

{- |The safe variant of 'head'.
    prop> headMaybe xs == xs ^? ix 0
-}
headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

{- |Sort on a key and group with the key.

    >>> :{
    groupSort
        [ (0, "a"), (1, "b")
        , (1, "c"), (0, "d")
        ]
    :}
    [(0,["a","d"]),(1,["b","c"])]
-}
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = groupWithKeyBy (==) . L.sortOn fst

{- |Correspondence between screenId and workspaceId.
    Screen ids must not be empty.
-}
correspondence :: Ord a => [a] -> [b] -> [(a, [b])]
correspondence a = groupSort . zip (cycle a)

{- |Similar to 'groupBy' but with a key.

    >>> :{
    groupWithKeyBy (==)
        [ (0, "a"), (1, "b")
        , (1, "c"), (0, "d")
        ]
    :}
    [(0,["a"]),(1,["b","c"]),(0,["d"])]
-}
groupWithKeyBy :: (k -> k -> Bool) -> [(k, v)] -> [(k, [v])]
groupWithKeyBy _ [] = []
groupWithKeyBy p ((fx, sx):xs) =
    let (ys, zs) = span (p' fx) xs
     in (fx, sx : (snd <$> ys)) : groupWithKeyBy p zs
    where
    p' x y = p x $ fst y

-- |A simpler variant of state
-- |Makes update function to a state monad.
-- |`execState` will do the inverse operation for `State`.
state' :: MonadState s m => (s -> s) -> m ()
state' f = state $ \s -> ((), f s)

-- |Steps an element of a list with an update function.
-- |The update function gets and returns an index.
stepElement :: Eq a => (Int -> Int) -> [a] -> a -> Maybe a
stepElement updateIndex l e = do
    i <- L.elemIndex e l
    l ^? ix (updateIndex i)

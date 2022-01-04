module Lib.Utils
    ( headMaybe
    , groupSort
    , groupWithKeyBy
    ) where

import           Data.List
import           Data.Maybe (listToMaybe)

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
groupSort = groupWithKeyBy (==) . sortOn fst

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

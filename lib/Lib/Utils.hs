module Lib.Utils
    ( headMaybe
    , at
    , groupSort
    , groupWithKeyBy
    ) where

import           Data.List
import           Data.Maybe (listToMaybe)


-- |The safe variant of head :: [a] -> a
headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

-- |Safe (!!)
at :: [a] -> Int -> Maybe a
at xs n
    | n < length xs = Just $ xs !! n
    | otherwise = Nothing

-- |Sort on a key and group with the key
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = groupWithKeyBy (==) . sortOn fst

-- |Similar to groupBy but with a key
groupWithKeyBy :: (k -> k -> Bool) -> [(k, v)] -> [(k, [v])]
groupWithKeyBy _ [] = []
groupWithKeyBy p ((fx, sx):xs) =
    let (ys, zs) = span (p' fx) xs
     in (fx, sx : (snd <$> ys)) : groupWithKeyBy p zs
    where
    p' x y = p x $ fst y

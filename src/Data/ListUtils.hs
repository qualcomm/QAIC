-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Data.ListUtils where

import Data.List(
     deleteFirstsBy
   )

-- | O(1) A list of one element
singleton                           :: a -> [a]
singleton x                          = [x]

-- | Unzip a list and then pass both lists to the function argument
unzipWith                           :: ([a] -> [b] -> c) -> [(a,b)] -> c
unzipWith f                          = uncurry f . unzip

-- | O(n) same as intersperse, but apply a function to each item and its
--   separator iff a separator follows.
intersperseWith              :: (a -> a -> a) -> a -> [a] -> [a]
intersperseWith _ _   []      = []
intersperseWith _ _   [x]     = [x]
intersperseWith f sep (x:xs)  = f x sep : intersperseWith f sep xs

-- | not . null
notNull                             :: [a] -> Bool
notNull                              = not . null

-- | Safe head.  Returns default value if list is empty.
headDef                             :: a -> [a] -> a
headDef  def []                      = def
headDef _def (x:_)                   = x

-- | Remove first element in each set that exists in any set to the left, and concatenate the results
unionl                              :: Eq a => [[a]] -> [a]
unionl                               = foldl unionFirsts []
   where

-- | Same as prelude's union, but removing only the first occurrence
unionFirsts                         :: Eq a => [a] -> [a] -> [a]
unionFirsts xs ys                    = xs ++ deleteFirstsBy (==) ys xs


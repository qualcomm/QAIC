-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Control.MonadUtils(
     concatM
   , consM
   , appendM
   , tupleM
   , singletonM
   , concatMapM
   , butReturn
   , followedBy
   , mapAccumM
   ) where

import Control.Monad(
     liftM
   , liftM2
   )
import Data.ListUtils(
     singleton
   )

-- |Monadic version of 'concat'
concatM                             :: Monad m => m [[a]] -> m [a]
concatM                              = liftM concat

-- |Monadic version of '(:)'
consM                               :: Monad m => m a -> m [a] -> m [a]
consM                                = liftM2 (:)

-- |Monadic version of '(++)'
appendM                             :: Monad m => m [a] -> m [a] -> m [a]
appendM                              = liftM2 (++)

-- |Monadic version of '(,)'
tupleM                              :: Monad m => m a -> m b -> m (a,b)
tupleM                               = liftM2 (,)

-- | Monadic version of 'singleton'
singletonM                          :: Monad m => m a -> m [a]
singletonM                           = liftM singleton

-- |Monadic version of 'concatMap'
concatMapM                          :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f                         = liftM concat . mapM f

-- |Discard the result of the first parameter, and return the second
butReturn                           :: Monad m => m a -> b -> m b
p `butReturn` x                      = p >> return x

-- |Monadic version of 'const'
followedBy                          :: Monad m => m a -> m b -> m a
followedBy                           = liftM2 const

-- |Monadic version of mapAccumL
mapAccumM                           :: Monad m
                                    => (a -> b -> m (a,c))
                                    -> a
                                    -> [b]
                                    -> m (a,[c])

mapAccumM _ acc []                   = return (acc, [])
mapAccumM m acc (x:xs)               = do
                                          (acc', c)   <- m acc x
                                          (acc'', cs) <- mapAccumM m acc' xs
                                          return (acc'', c:cs)



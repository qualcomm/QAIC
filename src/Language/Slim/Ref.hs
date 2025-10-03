-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Language.Slim.Ref where

import qualified Data.List as List
import Data.Function(on)
import Data.Generics(everywhere' , mkT, listify, Data, Typeable)
import Data.Maybe(isNothing, fromMaybe)
import Data.List(nub,elemIndex)

data Ref a = Val a
           | Ref Int
           deriving (Eq, Data, Typeable, Show)

compressAndUpdate :: (Data a, Typeable a1, Eq a, Data a11, Eq a1) => a11 -> (a1 -> [a]) -> ([a], a11)
compressAndUpdate typ ff =
   let
         db = getCompressedData ff typ
         typ' = updateListRefs ff db typ
   in    (db, typ')

getCompressedData :: (Eq a, Eq a1, Data a11, Typeable a1) => (a1 -> [a]) -> a11 -> [a]
getCompressedData ff typ = compress $ map ff $ getVals typ

updateListRefs :: (Data a, Typeable t, Eq a1) => (t -> [a1]) -> [a1] -> a -> a
updateListRefs ff lst tp = toRefs tp $ \ val -> fromJust $ (ff val) `listIndex` lst
   where
      fromJust = fromMaybe (error "internal error: list compression failure")

nubAndUpdate :: (Show b, Data b, Typeable a, Eq b, Data a1, Eq a,Show a) => a1 -> (a -> b) -> ([b], a1)
nubAndUpdate typ ff =
   let
         db = getNubData ff typ
         (db',typ') = updateElemRefs ff db (db,typ)
   in    (db', typ')

getNubData :: (Eq b, Eq a, Data a1, Typeable a) => (a -> b) -> a1 -> [b]
getNubData ff typ = nub $ map ff $ getVal
                   where getVal = getVals typ

updateElemRefs :: (Show a1, Data a, Typeable t, Eq a1) => (t -> a1) -> [a1] -> a -> a
updateElemRefs ff lst tp = toRefs tp $ \ val -> fromJust (ff val) $ (ff val) `elemIndex` lst
   where
      fromJust vv = fromMaybe (error $ "internal error: elem compression failure:" ++ (show (vv, lst)))


getVals :: (Eq a, Data a1, Typeable a) => a1 -> [a]
getVals tp =
   let
         isVal (Val _) = True
         isVal _ = False
   in List.nub $ map fromVal $ listify isVal tp

fromVal :: Ref a -> a
fromVal (Val aa) = aa
fromVal _ = error "fromVal: unexpected ref"

fromRef :: Ref a -> Int
fromRef (Ref aa) = aa
fromRef _ = error "fromRef: unexpected val"

isRef :: Ref a -> Bool
isRef (Ref _) = True
isRef _ = False


toRefs :: (Data a, Typeable t) => a -> (t -> Int) -> a
toRefs tp dbq =
   let
         fromVal' (Val aa) = Ref $ (dbq aa)
         fromVal' _ = error "toRefs: unexpected ref"
   in everywhere' (mkT fromVal') tp

unref :: (Data a, Typeable a) => a -> Int
unref tp =
   let
         isInt :: Int -> Bool
         isInt _ = True
         singleton' [a] = a
         singleton' _ = error "internal error: Language.Slim.Ref.unref"
   in    singleton' $ listify isInt tp


compress :: Eq a => [[a]] -> [a]
compress ll =
   let
         sorted = reverse $ List.sortBy (compare `on` length) ll

         compress' mms mm
            | isNothing ((reverse mm) `listIndex` mms) = (reverse mm) ++ mms
            | otherwise = mms
      in reverse $ List.foldl' compress' [] sorted


listIndex :: Eq a => [a] -> [a] -> Maybe Int
listIndex ll lst =
   let
         (nots, trues) = List.span ((==) False) $ map (List.isPrefixOf ll) $ List.tails lst
   in    case(trues) of
            [] -> Nothing
            _ -> Just (length nots)



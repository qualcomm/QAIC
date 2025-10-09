-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.Slim.Alignment where

import qualified Language.Idl.Data                    as ID
import qualified Data.List                            as List
import qualified Data.Omg.Prim                        as OP
import qualified Language.Slim.State                  as State

import Language.Slim.Data(SizeROut(SizeROut),SizeAlign(SizeAlign),Size, SizeNative(SizeNative),asROut, primROut, asIn)
import Data.Int( Int32)
import Data.Word( Word8)

fromType :: ID.Type -> State.StateM SizeAlign
fromType (ID.TypeRef sp _ sn)                   = do
   def <- State.lookupDef (sp,sn)
   fromDef def

fromType (ID.Struct _ mms)                      = do
   szs <- mapM fromStructMem mms
   let
      compute iis =
         let
               (iss, ias) = unzip iis
               is' = List.foldl' alignAndAdd 0 (zip iss ias)
               ia = maximum ias
               is = alignBuf (is', ia)
         in    (is, ia)
   return $ foldEach compute szs

fromType (ID.Union _ _ desc cases defcase)       = do
   dsz <- fromType $ desc
   szs <- fromCases defcase cases
   let
      compute iis =
         let
               (dis:iss, dia:ias) = unzip iis
               is' = List.foldl' alignAndAdd 0 ((dis, dia):[((maximum iss), (maximum ias))])
               ia = maximum (dia:ias)
               is = alignBuf (is', ia)
         in    (is,ia)
      dsz' = dsz  { asROut = (SizeROut (asIn dsz) (primROut (asROut dsz))) }
   return $ foldEach compute (dsz':szs)

fromType (ID.Enum _ _)                                         = do
   return $ (SizeAlign (4,4) (SizeROut (0,1) (4,4)) (SizeNative (4,4) (4,4)))

fromType (ID.Sequence  _ _)                                     = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (16,8)))

fromType (ID.StringType _)                                     = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (16,8)))

fromType (ID.WideStringType _)                                 = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (16,8)))

fromType (ID.DmahandleType _)                                 = do
   return $ (SizeAlign (0,0) (SizeROut (0,0) (0,0)) (SizeNative (12,4) (12,8)))

fromType (ID.PrimType prim )                                    = do
   let ps = OP.primSize prim
       pa = fromIntegral ps
   return $ (SizeAlign (ps,pa) (SizeROut (0,1) (ps,pa)) (SizeNative (ps,pa) (ps,pa)))

fromType (ID.Array ml tp)                                      = do
   sz <- fromType tp
   ml' <- State.constExprToInt32 ml
   let
      compute [(ss,al)] = (ss * ml', al)
      compute _ = error "internal error: pattern match"
   return $ foldEach compute [sz]

fromType (ID.Interface)                                        = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (16,8)))

fromType (ID.Native _)                                         = error "internal error: pattern match"

fromDef :: ID.Definition -> State.StateM SizeAlign
fromDef (ID.TypeDcl ty)                = fromType ty
fromDef (ID.InterfaceDcl _ _ _ _ _ _)    = return $ SizeAlign (0,1) (SizeROut (0,1) (0,1)) (SizeNative (4,4) (8,8))
fromDef _                              = error "internal error: pattern match"

fromStructMem :: (ID.Member) -> State.StateM SizeAlign
fromStructMem (ID.Member _ _ _ tp) = fromStructMem' tp
fromStructMem' :: ID.Type -> State.StateM SizeAlign
fromStructMem' (ID.TypeRef sp _ sn)   = do
   def <- State.lookupDef (sp,sn)
   fromDef' def
fromStructMem' (ID.Sequence _ _)                                     = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (12,8)))
fromStructMem' (ID.StringType _)                                     = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (12,8)))
fromStructMem' (ID.WideStringType _)                                 = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (12,8)))
fromStructMem' (ID.Interface)                                        = do
   return $ (SizeAlign (4,4) (SizeROut (4,4) (0,1)) (SizeNative (8,4) (12,8)))
fromStructMem' tp = fromType tp

fromDef' :: ID.Definition -> State.StateM SizeAlign
fromDef' (ID.TypeDcl ty)    = fromStructMem' ty
fromDef' def                = fromDef def

fromMem :: (ID.Member) -> State.StateM SizeAlign
fromMem (ID.Member _ _ _ tp) = fromType tp

fromCases ::  (Maybe ID.DefaultCase) -> [(ID.UnionCase)] -> State.StateM [SizeAlign]
fromCases (Just dc) cases = do
   ds <- fromMem dc
   szs <- mapM fromCase cases
   return $ ds : szs

fromCases Nothing cases = mapM fromCase cases

fromCase :: (ID.UnionCase) -> State.StateM SizeAlign
fromCase (ID.UnionCase _ mm) = fromMem mm

foldEach :: ([Size] -> Size) -> [SizeAlign] -> SizeAlign
foldEach ff sizes =
   let
      unzip' (SizeAlign (iin') (SizeROut oin' orout') (SizeNative ntv32' ntv64')) = [iin',oin',orout',ntv32',ntv64']
      [iin,oin,orout,ntv32, ntv64] = map ff $ List.transpose $ map unzip' sizes
   in SizeAlign iin (SizeROut oin orout) (SizeNative ntv32 ntv64)


alignBuf :: (Int32, Word8) -> Int32
alignBuf (bsz, 0) = bsz
alignBuf (bsz, al)
   | (bsz) `mod` (fromIntegral al) == 0 = (bsz)
   | otherwise = (fromIntegral bsz) + ((fromIntegral al) - ((fromIntegral bsz) `mod` (fromIntegral al)))

alignAndAdd :: Int32 -> (Int32, Word8) -> Int32
alignAndAdd sz (bsz, al) = (alignBuf (sz, al)) + bsz



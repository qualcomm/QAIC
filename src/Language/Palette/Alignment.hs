-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

-- | A module for converting the IDL datatype to the Palette datatype.
module Language.Palette.Alignment where

import Language.Palette.Data(
    Type(..)
  , Prim(..)
  , Member(Member)
  , Literal
  , primSize
  )
import Data.Int(
    Int32
  )

alignReq                            :: Type -> Int32
alignReq (Void)                      = 0
alignReq (PrimType x)                = primSize x
alignReq (Struct al _)               = al
alignReq (Union al _ _ _ _)          = al
alignReq (Interface sz _ _)          = sz
alignReq (InterfaceConstraint Nothing)
                                     = 4
alignReq (InterfaceConstraint _)     = 0
alignReq (Sequence _ _ _)            = 4
alignReq (SequenceConstraint _ _ _ _)= 4
alignReq (TaggedSequence _ _ _)      = 4
alignReq (Array _ ty)                = alignReq ty
alignReq (TypeLambda _ ty)           = alignReq ty
alignReq (RecursiveTypeRef _)        = 0
alignReq (Native _)                  = 0 -- We might calculate the alignment of a Native type, but it should never find its way to the user.
alignReq (OperationType)             = error "internal error: unexpected OperationType" -- TODO: send function handles over the wire
alignReq (TypeRef _)                 = error "internal error: unexpected TypeRef"

structAlignReq                      :: [Member] -> Int32
structAlignReq xs                    = maximum (0 : map (alignReq . memberType) xs)

unionAlignReq                       :: Prim -> [(Literal, Member)] -> Maybe Member -> Int32
unionAlignReq d xs def               = maximum (primSize d : map alignReq (unionTypes xs def))

size                                :: Type -> Int32
size (Void)                          = 0
size (PrimType p)                    = primSize p
size (Struct al ms)                  = structSize al ms
size (Union al caseSz _ _ _)         = al + caseSz
size (Interface sz _ _)              = sz
size (InterfaceConstraint Nothing)   = 4
size (InterfaceConstraint _)         = 0
size (Sequence _ _ _)                = 4
size (SequenceConstraint _ _ _ _)    = 4
size (TaggedSequence _ _ _)          = 8
size (Array sz ty)                   = sz * size ty
size (TypeLambda _ ty)               = size ty  -- Assumes recursion can only happen from within a sequence, which at the time of this writing, is valid.
size (RecursiveTypeRef _)            = 0        -- TODO: size of type from its TypeLambda, which will loop forever if recursion isn't done indirectly with a sequence
size (Native _)                      = 0 -- We might calculate the size of a Native type, but it should never find its way to the user.
size (OperationType)                 = error "internal error: unexpected OperationType"
size (TypeRef _)                     = error "internal error: unexpected TypeRef"

structSize                          :: Int32 -> [Member] -> Int32
structSize n ms                      = align n (map (sizes . memberType) ms)
   where
      sizes ty                       = (alignReq ty, size ty)

unionCaseSize                       :: [(Literal, Member)] -> Maybe Member -> Int32
unionCaseSize cs def                 = maximum (map size (defTy : map (memberType . snd) cs))
   where
      defTy                          = maybe Void memberType def

align                               :: Int32 -> [(Int32, Int32)] -> Int32
align n szs                          = foldl pad 0 (szs ++ [(n, 0)])
   where
      pad offset (al, sz)            = offset + padding al offset + sz

      padding edge offset
         | edge == 0 || offset `mod` edge == 0
                                     = 0
         | otherwise                 = edge - (offset `mod` edge)

memberType                          :: Member -> Type
memberType (Member _ ty)             = ty

unionTypes                          :: [(Literal, Member)] -> Maybe Member -> [Type]
unionTypes xs def                    = caseTys ++ defTys def
   where
      caseTys                        = map (memberType . snd) xs
      defTys (Just (Member _ ty))    = [ty]
      defTys _                       = []


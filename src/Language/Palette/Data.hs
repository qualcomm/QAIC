-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveDataTypeable #-}

-- | Palette is a data structure for interface definitions.
module Language.Palette.Data(
     module Data.Omg.Literal
   , module Data.Omg.Prim
   , Palette(..)
   , Declaration(..)
   , Definition(..)
   , Type(..)
   , SequenceType(..)
   , Buffer(..)
   , Member(..)
   , Parameter(..)
   , ParameterMode(..)
   , OpAttr(..)
   , InterfaceId
   , FunctionId
   , DefaultCase
   , Alignment
   , Size
   , MaxLen
   ) where

import Data.Omg.Literal
import Data.Omg.Prim
import Data.Word(
     Word32
   )
import Data.Int(
     Int32
   )
import Data.LittleBinary(
     Binary
   , get
   , put
   , getWord8
   , Word8
   , Put
   , Get
   )
import Control.Monad(
     liftM
   , liftM2
   , liftM3
   , replicateM
   )
import Data.Data(
     Typeable
   , Data
   )

data Palette                         = Palette [Declaration]
                                     deriving (Show, Eq)

data Declaration                     = Declaration String Definition
                                     deriving (Show, Eq)

data Definition                      = TypeDcl Type
                                     | InterfaceDcl IsLocal (Maybe InterfaceId) [Declaration]
                                     | ConstDcl Type Literal
                                     | OperationDcl OpAttr [Parameter]
                                     | ModuleDcl [Declaration]
                                     deriving (Show, Eq)

data Parameter                       = Parameter String ParameterMode Type
                                     deriving (Show, Eq)

data ParameterMode                   = ParameterIn
                                     | ParameterROut
                                     | ParameterInROut
                                     deriving (Show, Eq)

data OpAttr                          = OpAttrSync
                                     | OpAttrOneway
                                     | OpAttrSignal
                                     deriving (Show, Eq, Data, Typeable)

data Buffer                          = Buffer Size Type
                                     deriving (Show, Eq, Ord, Data, Typeable)

data Type                            = Void
                                     | PrimType Prim
                                     | Struct Alignment [Member]
                                     | Union Alignment Size Prim [(Literal, Member)] (Maybe DefaultCase)
                                     | Sequence SequenceType MaxLen Buffer
                                     | SequenceConstraint SequenceType MaxLen Buffer Size -- MaxLength ReturnedSize
                                     | TaggedSequence SequenceType MaxLen Buffer
                                     | Array Int32 Type
                                     | Interface Size Bool (Maybe InterfaceId)
                                     | InterfaceConstraint (Maybe InterfaceId)
                                     | OperationType       -- TODO: Add "OperationType OpAttr [Parameter]", and remove OperationDcl
                                     | Native String
                                     | TypeLambda FunctionId Type
                                     | RecursiveTypeRef FunctionId
                                     | TypeRef Int32   -- Used only by InterfaceMap
                                     deriving (Show, Eq, Ord, Data, Typeable)

data SequenceType                    = Seq
                                     | StringSeq
                                     | WideStringSeq
                                     deriving (Show, Eq, Ord, Data, Typeable)

data Member                          = Member String Type
                                     deriving (Show, Eq, Ord, Data, Typeable)

type InterfaceId                     = Word32
type FunctionId                      = Int32
type DefaultCase                     = Member
type Alignment                       = Int32
type Size                            = Int32
type MaxLen                          = Int32
type IsLocal                         = Bool

instance Binary Buffer where
   put (Buffer sz ty)                = put sz >> put ty
   get                               = liftM2 Buffer get get

instance Binary Type where
   put (Void)                        = put (0 :: Word8)
   put (PrimType SignedShortType)    = put (1 :: Word8)
   put (PrimType SignedLongType)     = put (2 :: Word8)
   put (PrimType SignedLongLongType) = put (3 :: Word8)
   put (PrimType UnsignedShortType)  = put (4 :: Word8)
   put (PrimType UnsignedLongType)   = put (5 :: Word8)
   put (PrimType UnsignedLongLongType) = put (6 :: Word8)
   put (PrimType BooleanType)        = put (7 :: Word8)
   put (PrimType FloatType)          = put (8 :: Word8)
   put (PrimType DoubleType)         = put (9 :: Word8)
   put (PrimType OctetType)          = put (10 :: Word8)
   put (PrimType CharType)           = put (11 :: Word8)
   put (PrimType WideCharType)       = put (12 :: Word8)
   put (PrimType (EnumType xs))      = put (13 :: Word8) >> put xs
   put (Struct al ms)                = put (14 :: Word8) >> put al >> put ms
                                       -- Warning: this breaks from "get . put == id"
                                       -- Information about the Literal is dropped,
                                       -- since the type of the discriminator tells us
                                       -- the type of each literal.
                                       -- Ideally, we'd find a type signature for UnionCase
                                       -- that doesn't force it to have to store the redundant
                                       -- typing information in the first place.
                                       --
                                       -- Warning 2: We serialize list length as an Int, which
                                       -- at the time of this writing is how Binary is implemented
                                       -- for List.  If changed for list to be a variable list of Word8's,
                                       -- we should change here too.
   put (Union al sz ty xs mayDef)    = put (15 :: Word8)
                                    >> put al
                                    >> put sz
                                    >> putDis ty
                                    >> put (length xs)
                                    >> mapM_ (putUnionCase ty) xs
                                    >> put mayDef
   put (Sequence Seq i ty)           = put (16 :: Word8) >> put i >> put ty
   put (Sequence StringSeq i _)      = put (17 :: Word8) >> put i
   put (Sequence WideStringSeq i _)  = put (18 :: Word8) >> put i
   put (SequenceConstraint Seq ln ty sz)
                                     = put (19 :: Word8) >> put ln >> put ty >> put sz
   put (SequenceConstraint StringSeq ln _ _)
                                     = put (20 :: Word8) >> put ln
   put (SequenceConstraint WideStringSeq ln _ _)
                                     = put (21 :: Word8) >> put ln
   put (TaggedSequence Seq i ty)     = put (22 :: Word8) >> put i >> put ty
   put (TaggedSequence StringSeq i _)= put (23 :: Word8) >> put i
   put (TaggedSequence WideStringSeq i _)
                                     = put (24 :: Word8) >> put i
   put (Array sz ty)                 = put (25 :: Word8) >> put sz >> put ty
   put (Interface _ False (Just iid))= put (26 :: Word8) >> put iid
   put (Interface _ True  (Just iid))= put (27 :: Word8) >> put iid
   put (InterfaceConstraint (Just iid))
                                     = put (28 :: Word8) >> put iid
   put (Interface 0 False Nothing)   = put (29 :: Word8)  -- Runtime-defined Interface
   put (Interface 0 True  Nothing)   = put (30 :: Word8)  -- Notnil Runtime-defined Interface
   put (InterfaceConstraint Nothing) = put (31 :: Word8)  -- Runtime-defined Interface Constraint
   put (TypeLambda fid ty)           = put (32 :: Word8) >> put fid >> put ty
   put (RecursiveTypeRef fid)        = put (33 :: Word8) >> put fid
   put (TypeRef offset)              = put (34 :: Word8) >> put offset
   put (Interface _ False Nothing)   = put (35 :: Word8)  -- IID-tagged Interface
   put (Interface _ True  Nothing)   = put (36 :: Word8)  -- IID-tagged Interface
   put (PrimType SignedCharFixedTType)    = put (37 :: Word8)
   put (PrimType SignedShortFixedTType)   = put (38 :: Word8)
   put (PrimType SignedLongFixedTType)    = put (39 :: Word8)
   put (PrimType SignedLongLongFixedTType)= put (40 :: Word8)
   put (PrimType UnsignedCharFixedTType)  = put (41 :: Word8)
   put (PrimType UnsignedShortFixedTType) = put (42 :: Word8)
   put (PrimType UnsignedLongFixedTType)  = put (43 :: Word8)
   put (PrimType UnsignedLongLongFixedTType)= put (44 :: Word8)
   put (PrimType SignedCharFixedType)    = put (45 :: Word8)
   put (PrimType SignedShortFixedType)   = put (46 :: Word8)
   put (PrimType SignedLongFixedType)    = put (47 :: Word8)
   put (PrimType SignedLongLongFixedType)= put (48 :: Word8)
   put (PrimType UnsignedCharFixedType)  = put (49 :: Word8)
   put (PrimType UnsignedShortFixedType) = put (50 :: Word8)
   put (PrimType UnsignedLongFixedType)  = put (51 :: Word8)
   put (PrimType UnsignedLongLongFixedType)= put (52 :: Word8)
   put (Native nm)                   = put (254 :: Word8) >> put nm  -- We shouldn't see this get serialized
   put OperationType                 = put (255 :: Word8)            -- We shouldn't see this get serialized

   get                               = do tag <- getWord8
                                          case tag of
                                             0 -> return  Void
                                             1 -> return (PrimType SignedShortType)
                                             2 -> return (PrimType SignedLongType)
                                             3 -> return (PrimType SignedLongLongType)
                                             4 -> return (PrimType UnsignedShortType)
                                             5 -> return (PrimType UnsignedLongType)
                                             6 -> return (PrimType UnsignedLongLongType)
                                             7 -> return (PrimType BooleanType)
                                             8 -> return (PrimType FloatType)
                                             9 -> return (PrimType DoubleType)
                                             10 -> return (PrimType OctetType)
                                             11 -> return (PrimType CharType)
                                             12 -> return (PrimType WideCharType)
                                             13 -> liftM (PrimType . EnumType) get
                                             14 -> liftM2  Struct get get
                                             15 -> do
                                                     al     <- get
                                                     sz     <- get
                                                     ty     <- getDis
                                                     len    <- get
                                                     xs     <- replicateM len (getUnionCase ty)
                                                     mayDef <- get
                                                     return (Union al sz ty xs mayDef)
                                             16 -> liftM2 (Sequence Seq) get get
                                             17 -> liftM  (\i -> Sequence StringSeq i charBuf) get
                                             18 -> liftM  (\i -> Sequence WideStringSeq i wcharBuf) get
                                             19 -> liftM3 (SequenceConstraint Seq) get get get
                                             20 -> liftM  (\i -> SequenceConstraint StringSeq i emptyBuf 1) get
                                             21 -> liftM  (\i -> SequenceConstraint WideStringSeq i emptyBuf 2) get
                                             22 -> liftM2 (TaggedSequence Seq) get get
                                             23 -> liftM  (\i -> TaggedSequence StringSeq i charBuf) get
                                             24 -> liftM  (\i -> TaggedSequence WideStringSeq i wcharBuf) get
                                             25 -> liftM2 Array get get
                                             26 -> liftM  (Interface 0 False . Just) get
                                             27 -> liftM  (Interface 0 True . Just) get
                                             28 -> liftM  (InterfaceConstraint . Just) get
                                             29 -> return (Interface 0 False Nothing)
                                             30 -> return (Interface 0 True Nothing)
                                             31 -> return (InterfaceConstraint Nothing)
                                             32 -> liftM2 TypeLambda get get
                                             33 -> liftM  RecursiveTypeRef get
                                             34 -> liftM  TypeRef get
                                             35 -> return (Interface 4 False Nothing)
                                             36 -> return (Interface 4 True Nothing)
                                             37 -> return (PrimType SignedCharFixedTType)
                                             38 -> return (PrimType SignedShortFixedTType)
                                             39 -> return (PrimType SignedLongFixedTType)
                                             40 -> return (PrimType SignedLongLongFixedTType)
                                             41 -> return (PrimType UnsignedCharFixedTType)
                                             42 -> return (PrimType UnsignedShortFixedTType)
                                             43 -> return (PrimType UnsignedLongFixedTType)
                                             44 -> return (PrimType UnsignedLongLongFixedTType)
                                             45 -> return (PrimType SignedCharFixedType)
                                             46 -> return (PrimType SignedShortFixedType)
                                             47 -> return (PrimType SignedLongFixedType)
                                             48 -> return (PrimType SignedLongLongFixedType)
                                             49 -> return (PrimType UnsignedCharFixedType)
                                             50 -> return (PrimType UnsignedShortFixedType)
                                             51 -> return (PrimType UnsignedLongFixedType)
                                             52 -> return (PrimType UnsignedLongLongFixedType)
                                             254 -> liftM  Native get          -- We shouldn't see this get serialized
                                             255 -> return OperationType       -- We shouldn't see this get serialized
                                             _ -> error "corrupt binary data"
      where
         emptyBuf                    = Buffer 0 Void
         charBuf                     = Buffer 1 (PrimType CharType)
         wcharBuf                    = Buffer 2 (PrimType WideCharType)

instance Binary Member where
   put (Member nm ty)                = put nm >> put ty
   get                               = liftM2 Member get get

putUnionCase                        :: Prim -> (Literal, Member) -> Put
putUnionCase ty (lit, m)             = putLit ty lit >> put m

getUnionCase                        :: Prim -> Get (Literal, Member)
getUnionCase ty                      = liftM2 (,) (getLit ty) get

putDis                              :: Prim -> Put
putDis SignedShortType               = put (0 :: Word8)
putDis SignedLongType                = put (1 :: Word8)
putDis SignedLongLongType            = put (2 :: Word8)
putDis UnsignedShortType             = put (3 :: Word8)
putDis UnsignedLongType              = put (4 :: Word8)
putDis UnsignedLongLongType          = put (5 :: Word8)
putDis CharType                      = put (6 :: Word8)
putDis BooleanType                   = put (7 :: Word8)
putDis SignedCharFixedTType               = put (8 :: Word8)
putDis SignedShortFixedTType              = put (9 :: Word8)
putDis SignedLongFixedTType               = put (10 :: Word8)
putDis SignedLongLongFixedTType           = put (11 :: Word8)
putDis UnsignedCharFixedTType             = put (12 :: Word8)
putDis UnsignedShortFixedTType            = put (13 :: Word8)
putDis UnsignedLongFixedTType             = put (14 :: Word8)
putDis UnsignedLongLongFixedTType         = put (15 :: Word8)
putDis SignedCharFixedType                = put (16 :: Word8)
putDis SignedShortFixedType               = put (17 :: Word8)
putDis SignedLongFixedType                = put (18 :: Word8)
putDis SignedLongLongFixedType            = put (19 :: Word8)
putDis UnsignedCharFixedType              = put (20 :: Word8)
putDis UnsignedShortFixedType             = put (21 :: Word8)
putDis UnsignedLongFixedType              = put (22 :: Word8)
putDis UnsignedLongLongFixedType          = put (23 :: Word8)
putDis _                             = error "unexpected discriminator type"

getDis                              :: Get Prim
getDis                               = do tag <- getWord8
                                          case tag of
                                             0 -> return SignedShortType
                                             1 -> return SignedLongType
                                             2 -> return SignedLongLongType
                                             3 -> return UnsignedShortType
                                             4 -> return UnsignedLongType
                                             5 -> return UnsignedLongLongType
                                             6 -> return CharType
                                             7 -> return BooleanType
                                             8 -> return SignedCharFixedTType
                                             9 -> return SignedShortFixedTType
                                             10 -> return SignedLongFixedTType
                                             11 -> return SignedLongLongFixedTType
                                             12 -> return UnsignedCharFixedTType
                                             13-> return UnsignedShortFixedTType
                                             14 -> return UnsignedLongFixedTType
                                             15 -> return UnsignedLongLongFixedTType
                                             16 -> return SignedCharFixedType
                                             17 -> return SignedShortFixedType
                                             18 -> return SignedLongFixedType
                                             19 -> return SignedLongLongFixedType
                                             20 -> return UnsignedCharFixedType
                                             21 -> return UnsignedShortFixedType
                                             22 -> return UnsignedLongFixedType
                                             23 -> return UnsignedLongLongFixedType
                                             _ -> error "corrupt binary data"

putLit                              :: Prim -> Literal -> Put
putLit SignedShortType      (IntLiteral _ (SignedShortLiteral x))      = put x
putLit SignedLongType       (IntLiteral _ (SignedLongLiteral x))       = put x
putLit (EnumType _)         (IntLiteral _ (SignedLongLiteral x))       = put x
putLit SignedLongLongType   (IntLiteral _ (SignedLongLongLiteral x))   = put x
putLit UnsignedShortType    (IntLiteral _ (UnsignedShortLiteral x))    = put x
putLit UnsignedLongType     (IntLiteral _ (UnsignedLongLiteral x))     = put x
putLit UnsignedLongLongType (IntLiteral _ (UnsignedLongLongLiteral x)) = put x
putLit SignedCharFixedTType       (IntLiteral _ (SignedCharLiteral x))       = put x
putLit SignedShortFixedTType      (IntLiteral _ (SignedShortLiteral x))      = put x
putLit SignedLongFixedTType       (IntLiteral _ (SignedLongLiteral x))       = put x
putLit SignedLongLongFixedTType   (IntLiteral _ (SignedLongLongLiteral x))   = put x
putLit UnsignedCharFixedTType     (IntLiteral _ (UnsignedCharLiteral x))     = put x
putLit UnsignedShortFixedTType    (IntLiteral _ (UnsignedShortLiteral x))    = put x
putLit UnsignedLongFixedTType     (IntLiteral _ (UnsignedLongLiteral x))     = put x
putLit UnsignedLongLongFixedTType (IntLiteral _ (UnsignedLongLongLiteral x)) = put x
putLit SignedCharFixedType        (IntLiteral _ (SignedCharLiteral x))       = put x
putLit SignedShortFixedType       (IntLiteral _ (SignedShortLiteral x))      = put x
putLit SignedLongFixedType        (IntLiteral _ (SignedLongLiteral x))       = put x
putLit SignedLongLongFixedType    (IntLiteral _ (SignedLongLongLiteral x))   = put x
putLit UnsignedCharFixedType      (IntLiteral _ (UnsignedCharLiteral x))     = put x
putLit UnsignedShortFixedType     (IntLiteral _ (UnsignedShortLiteral x))    = put x
putLit UnsignedLongFixedType      (IntLiteral _ (UnsignedLongLiteral x))     = put x
putLit UnsignedLongLongFixedType  (IntLiteral _ (UnsignedLongLongLiteral x)) = put x
putLit CharType             (CharLiteral x)                            = put x
putLit BooleanType          (BoolLiteral x)                            = put x
putLit _                    _                                          = error "unexpected discriminator type"

getLit                              :: Prim -> Get Literal
getLit SignedShortType               = liftM (IntLiteral Dec . SignedShortLiteral)      get
getLit SignedLongType                = liftM (IntLiteral Dec . SignedLongLiteral)       get
getLit (EnumType _)                  = liftM (IntLiteral Dec . SignedLongLiteral)       get
getLit SignedLongLongType            = liftM (IntLiteral Dec . SignedLongLongLiteral)   get
getLit UnsignedShortType             = liftM (IntLiteral Hex . UnsignedShortLiteral)    get
getLit UnsignedLongType              = liftM (IntLiteral Hex . UnsignedLongLiteral)     get
getLit UnsignedLongLongType          = liftM (IntLiteral Hex . UnsignedLongLongLiteral) get
getLit SignedCharFixedTType               = liftM (IntLiteral Dec . SignedCharLiteral)       get
getLit SignedShortFixedTType              = liftM (IntLiteral Dec . SignedShortLiteral)      get
getLit SignedLongFixedTType               = liftM (IntLiteral Dec . SignedLongLiteral)       get
getLit SignedLongLongFixedTType           = liftM (IntLiteral Dec . SignedLongLongLiteral)   get
getLit UnsignedCharFixedTType             = liftM (IntLiteral Hex . UnsignedCharLiteral)     get
getLit UnsignedShortFixedTType            = liftM (IntLiteral Hex . UnsignedShortLiteral)    get
getLit UnsignedLongFixedTType             = liftM (IntLiteral Hex . UnsignedLongLiteral)     get
getLit UnsignedLongLongFixedTType         = liftM (IntLiteral Hex . UnsignedLongLongLiteral) get
getLit SignedCharFixedType                = liftM (IntLiteral Dec . SignedCharLiteral)       get
getLit SignedShortFixedType               = liftM (IntLiteral Dec . SignedShortLiteral)      get
getLit SignedLongFixedType                = liftM (IntLiteral Dec . SignedLongLiteral)       get
getLit SignedLongLongFixedType            = liftM (IntLiteral Dec . SignedLongLongLiteral)   get
getLit UnsignedCharFixedType              = liftM (IntLiteral Hex . UnsignedCharLiteral)     get
getLit UnsignedShortFixedType             = liftM (IntLiteral Hex . UnsignedShortLiteral)    get
getLit UnsignedLongFixedType              = liftM (IntLiteral Hex . UnsignedLongLiteral)     get
getLit UnsignedLongLongFixedType          = liftM (IntLiteral Hex . UnsignedLongLongLiteral) get
getLit CharType                      = liftM CharLiteral get
getLit BooleanType                   = liftM BoolLiteral get
getLit _                             = error "unexpected discriminator type"

instance Binary OpAttr where
     put OpAttrSync   = put (0 :: Word8)
     put OpAttrOneway = put (1 :: Word8)
     put OpAttrSignal = put (2 :: Word8)

     get            = do tag <- getWord8
                         case tag of
                            0 -> return OpAttrSync
                            1 -> return OpAttrOneway
                            2 -> return OpAttrSignal
                            _ -> error "corrupt binary data"

-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Language.Slim.Data where

import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Language.Idl.Data                    as ID
import qualified Language.Idl.ToPalette               as TP
import qualified Language.Slim.TypeRefMap             as TD
import qualified Language.Slim.Ref                    as Ref
import Data.Word( Word64, Word32, Word16, Word8)
import Data.Int( Int32)
import Data.Generics(Data,Typeable)



data State = State { paletteDics :: (TP.TypeDictionary , TP.ConstDictionary)
                   , typeRefMap :: TD.TypeRefMap
                   , constMap :: TD.ConstWordMap
                   , curScalars :: Scalars
                   , mode :: ID.ParameterMode
                   }
                   deriving Show


data Slim = Slim { interfaces                 :: [Interface]
                 , methods                    :: [Method]
                 , methodArrays               :: [MethodRef]
                 , parameters                 :: [Parameter]
                 , parameterArrays            :: [ParameterRef]
                 , types                      :: [Type]
                 , typeArrays                 :: [TypeRef]
                 , structTypes                :: [StructType]
                 , unionTypes                 :: [UnionType]
                 , sequenceTypes              :: [SequenceType]
                 , value64s                   :: [Word64]
                 , value32s                   :: [Word32]
                 , value16s                   :: [Word16]
                 , value8s                    :: [Word8]
                 , strings                    :: String
                 , methodStrings              :: [StringRef]
                 , methodStringsArrays        :: [MethodStringsRef]
                 , moduleIIds                 :: [AEEIID]
                 }
          deriving (Eq, Show, Data, Typeable)


new :: Slim
new = Slim [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []

newtype MethodRef                       = MethodRef (Ref.Ref Method)
                                        deriving (Eq, Show, Data, Typeable)

newtype MethodRefArrayRef               = MethodRefArrayRef (Ref.Ref [MethodRef])
                                        deriving (Eq, Show, Data, Typeable)

newtype AEEIIDArrayRef                  = AEEIIDArrayRef (Ref.Ref [AEEIID])
                                        deriving (Eq, Show, Data, Typeable)

type NumMethods              = Int
type NumIIDs                 = Int
type AEEIID                  = Word32
type NumDerived              = Int

newtype StringRef       = StringRef (Ref.Ref String)
                        deriving (Eq, Show, Data, Typeable)
data MethodStrings      = MethodStrings StringRef [ParameterStrings]
                        deriving (Eq, Show, Data, Typeable)
data ParameterStrings   = ParameterStrings StringRef TypeStrings
                        deriving (Eq, Show, Data, Typeable)
data TypeStrings        = TypeStrings [StringRef] [TypeStrings]
                        | NoName
                        deriving (Eq, Show, Data, Typeable)

data Interface  = Interface [String] NumMethods MethodRefArrayRef NumIIDs AEEIIDArrayRef MethodStringsArrayRef
                deriving (Eq, Show, Data, Typeable)

newtype MethodStringsRef       = MethodStringsRef (Ref.Ref MethodStrings)
                               deriving (Eq, Show, Data, Typeable)
newtype MethodStringsArrayRef  = MethodStringsArrayRef  (Ref.Ref [MethodStringsRef])
                               deriving (Eq, Show, Data, Typeable)

iid_remote_handle :: Word32
iid_remote_handle = 0xdeadc0de

type OpAttr       = Word8
type BufsIn       = Word8
type BufsROut     = Word8
type ObjsIn       = Word8
type ObjsROut     = Word8
type PrimInSize   = Size
type PrimROutSize = Size
type NumIn        = Int32
type NumROut      = Int32
type Reserved     = Int32

data Scalars = Scalars { bufsIn :: BufsIn
                       , bufsROut :: BufsROut
                       , objsIn :: ObjsIn
                       , objsROut :: ObjsROut
                       }
              deriving (Eq, Show, Data, Typeable)


data Method  = Method { opAttr :: Maybe (Pos.SourcePos, ID.OpAttr)
                      , methodScalars:: Scalars
                      , primInSize :: PrimInSize
                      , primROutSize :: PrimROutSize
                      , numArgs  :: Int32
                      , numParams :: Int32
                      , params :: ParameterRefArrayRef
                      }
              deriving (Eq, Show, Data, Typeable)

type HasMethodArg = Bool

scalars :: HasMethodArg -> Method -> Scalars
scalars False mm = methodScalars mm
scalars True mm
   | (bufsIn ms) == 0 = ms { bufsIn = 1 }
   | otherwise = ms
   where
      ms = methodScalars mm

newtype ParameterRef = ParameterRef (Ref.Ref Parameter)
                     deriving (Eq, Show, Data, Typeable)

newtype ParameterRefArrayRef  = ParameterRefArrayRef (Ref.Ref [ParameterRef])
                               deriving (Eq, Show, Data, Typeable)

data Parameter = Parameter Type ID.ParameterMode Bool
               deriving (Eq, Show, Data, Typeable)

data TypeDesc = TypeObject             AEEIID Bool
              | TypeInterface          Bool
              | SignedShortType           (Maybe String) Bool
              | SignedLongType            (Maybe String) Bool
              | SignedLongLongType        (Maybe String) Bool
              | UnsignedShortType           (Maybe String) Bool
              | UnsignedLongType            (Maybe String) Bool
              | UnsignedLongLongType        (Maybe String) Bool
              | UnsignedCharFixedTType      (Maybe String) Bool
              | UnsignedShortFixedTType     (Maybe String) Bool
              | UnsignedLongFixedTType      (Maybe String) Bool
              | UnsignedLongLongFixedTType   (Maybe String) Bool
              | SignedCharFixedTType        (Maybe String) Bool
              | SignedShortFixedTType       (Maybe String) Bool
              | SignedLongFixedTType        (Maybe String) Bool
              | SignedLongLongFixedTType    (Maybe String) Bool
              | UnsignedCharFixedType       (Maybe String) Bool
              | UnsignedShortFixedType      (Maybe String) Bool
              | UnsignedLongFixedType       (Maybe String) Bool
              | UnsignedLongLongFixedType   (Maybe String) Bool
              | SignedCharFixedType         (Maybe String) Bool
              | SignedShortFixedType        (Maybe String) Bool
              | SignedLongFixedType         (Maybe String) Bool
              | SignedLongLongFixedType     (Maybe String) Bool
              | CharType                    (Maybe String) Bool
              | OctetType                   (Maybe String) Bool
              | FloatType                   (Maybe String) Bool
              | DoubleType                  (Maybe String) Bool
              | WideCharType                (Maybe String) Bool
              | BooleanType                 (Maybe String) Bool
              | TypeEnum String Bool
              | TypeString             Int32 String Bool
              | TypeWString            Int32 String Bool
              | TypeDmahandle          Int32
              | TypeStructure          StructTypeRef String Bool
              | TypeUnion              UnionTypeRef String
              | TypeArray              TypeRef  Int32 String Bool
              | TypeSequence           TypeRef  Int32 String Bool String
              | TypeComplexStructure   StructTypeRef String Bool
              | TypeComplexUnion       UnionTypeRef
              | TypeComplexArray       TypeRef  Int32 String
              | TypeComplexSequence    SequenceTypeRef String Bool String
              deriving (Eq, Show, Data, Typeable)

newtype TypeRef = TypeRef (Ref.Ref Type)
                deriving (Eq, Show, Data, Typeable)

data Type = Type TypeDesc SizeNative
          deriving (Eq, Show, Data, Typeable)

newtype SequenceTypeRef = SequenceTypeRef (Ref.Ref SequenceType)
                        deriving (Eq, Show, Data, Typeable)

data SequenceType = SequenceType TypeRef Int32 SizeIn SizeROut
                  deriving (Eq, Show, Data, Typeable)

data UnionType    = UnionType TypeRef Int CaseValueArrayRef TypeRefArrayRef SizeIn SizeROut CaseAlignments DefaultCase
                   deriving (Eq, Show, Data, Typeable)
newtype UnionTypeRef = UnionTypeRef (Ref.Ref UnionType)
                     deriving (Eq, Show, Data, Typeable)

data CaseValueArrayRef       = CaseValue8ArrayRef  (Ref.Ref [Word8])
                             | CaseValue16ArrayRef (Ref.Ref [Word16])
                             | CaseValue32ArrayRef (Ref.Ref [Word32])
                             | CaseValue64ArrayRef (Ref.Ref [Word64])
                             deriving (Eq, Show, Data, Typeable)

type CaseAlignments              = (InCaseAlignment, ROutCaseAlignmentPrimIn, ROutCaseAlignmentPrimROut, NativeCaseAlignment)
type InCaseAlignment             = Word8
type ROutCaseAlignmentPrimIn     = Word8
type ROutCaseAlignmentPrimROut   = Word8
type NativeCaseAlignment         = (Word8,Word8)
type DefaultCase                 = Bool

data StructType    = StructType Word32 TypeRefArrayRef SizeIn SizeROut
                    deriving (Eq, Show, Data, Typeable)
newtype StructTypeRef = StructTypeRef (Ref.Ref StructType)
                      deriving (Eq, Show, Data, Typeable)

newtype TypeRefArrayRef  = TypeRefArrayRef (Ref.Ref [TypeRef])
                           deriving (Eq, Show, Data, Typeable)

type Size = (Int32, Word8)

data SizeROut = SizeROut { primIn :: Size
                         , primROut :: Size
                         }
          deriving (Eq, Show, Data, Typeable)

type SizeIn = Size
type Size32 = Size
type Size64 = Size
data SizeNative = SizeNative Size32 Size64
          deriving (Ord, Eq, Show, Data, Typeable)

sznSz :: SizeNative -> (Int32,Int32)
sznSz (SizeNative (s32,_) (s64,_)) = (s32,s64)
sznAl :: SizeNative -> (Word8,Word8)
sznAl (SizeNative (_,a32) (_,a64)) = (a32,a64)

data SizeAlign = SizeAlign { asIn :: SizeIn
                           , asROut :: SizeROut
                           , asNative :: SizeNative
                           }
          deriving (Eq, Show, Data, Typeable)



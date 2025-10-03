-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Language.Slim.Serialize where

import Data.Data                                      ( Data )
import Data.Word                                      ( Word8, Word32 )
import Language.Idl.Data                              as Idl
import Data.Generics                                  ( listify, Typeable )
import Data.List                                      ( sortBy )
import Data.Function                                  ( on )
import Control.Monad                                  ( liftM )
import qualified Control.Monad.State                  as State
import Language.Slim.Data                             as Slim
import Language.Slim.Ref
import Control.Applicative                            ((<$>))


type BufferIndex = Int
type ArgNum = Int

type IsMember = Bool


data Offset = Offset { szNative :: SizeNative, oNative :: (Int,Int), oIn :: Int, oROut :: Int
                     , oBufIn :: Int, oBufROut :: Int }
            deriving (Data, Typeable, Ord, Eq, Show)

data ArgIndex = ArgIndex { xN :: ArgNum, xIsM :: IsMember, xO :: Offset }
              deriving (Data, Typeable, Ord, Eq, Show)

data Arg = Arg {aM :: Mode,  aI :: ArgIndex, aIsIR :: Bool }
         deriving (Data, Typeable, Ord, Eq, Show)

data Mode = In | ROut
          deriving (Data, Typeable, Ord, Eq, Show)

data Buf = Buf Mode BufferIndex
         deriving (Data, Typeable, Ord, Eq, Show)

type ObjIndex = Int
data Obj = Obj Mode ObjIndex
          deriving (Data, Typeable, Ord, Eq, Show)

data BufType = SequenceBuf Size IsString TypeDesc String
             deriving (Data, Typeable, Eq, Show)

type IsFloat = Bool
type IsString = Bool
type IsDmaHandle = Bool
data ScalarType = Register    IsFloat Word8
                | Scalar      Size
                | Len         (Maybe Word8)
                deriving (Data, Typeable, Ord, Eq, Show)

data Primary = Prim Mode BufferIndex Size
             deriving (Data, Typeable, Ord, Eq, Show)

data DmahandleType = Fd (Maybe Word32)
             | DOffset (Maybe Word32)
             | DLen (Maybe Word32)
             deriving (Data, Typeable, Ord, Eq, Show)

data ComplexType = ComplexStruct String
                 | ComplexSeq SizeNative String IsDmaHandle
                 deriving (Data, Typeable, Ord, Eq, Show)

data SerealArg = ScalarArg    { saArg :: Arg, saST :: ScalarType, saPB :: Primary, typedesc :: String}
               | BufferArg    { saArg :: Arg, saBT :: BufType, saBuf :: Buf }
               | ComplexArg   { saArg :: Arg, saCT :: ComplexType, saPBs :: [Primary], saCA :: [SerealArg] }
               | ObjArg { saArg :: Arg, saObj :: Obj }
               | DmahandleArg { saArg :: Arg, saDT :: DmahandleType, saPBd :: Obj }
               deriving (Data, Typeable, Eq, Show)

argInBuf :: Arg -> Int
argInBuf arg = oBufIn $ xO $ aI arg

argROutBuf :: Arg -> Int
argROutBuf arg = oBufROut $ xO $ aI arg

saIsIn :: SerealArg -> Bool
saIsIn sa = not $ null $ listify argIsIn sa

saIsIR :: SerealArg -> Bool
saIsIR sa = aIsIR $ saArg sa

saN :: SerealArg -> Int
saN sa = xN $ aI $ saArg $ sa

saNativeO :: SerealArg -> (Int, Int)
saNativeO sa = oNative $ xO $ aI $ saArg $ sa

aNativeSZ :: Arg -> SizeNative
aNativeSZ arg = szNative $ xO $ aI $ arg

singleton :: String -> [a] -> a
singleton _ [a] = a
singleton str _ = error $ "internal error: singleton: " ++ str

aN  :: Arg -> Int
aN arg = xN $ aI arg

argIsIn  :: Arg -> Bool
argIsIn (Arg In _ _) = True
argIsIn _            = False

methodParameters :: Slim -> Method -> [Slim.Parameter]
methodParameters sl method =
   let
         (ParameterRefArrayRef paref) = params method
         prefs = reflist (parameterArrays sl) (numParams method) (paref)
   in    map (((!!) (parameters sl)) . unref) prefs


serializeParameters :: Slim -> HasMethodArg -> [Slim.Parameter] -> String -> [SerealArg]
serializeParameters sl False pars name = serializeMembers sl False pars name
serializeParameters sl True pars name = serializeMembers sl False (method:pars) name
   where
      method = Slim.Parameter ((Type (Slim.UnsignedLongFixedTType Nothing False) (SizeNative (4,4) (4,4)))) ParameterIn False

serializeMembers :: Slim -> IsMember -> [Slim.Parameter] -> String -> [SerealArg]
serializeMembers sl ism pars name =
   let
         zipWithM ff aa bb = mapM (uncurry ff) $ zip aa bb
         sereal = concat $ fst $ State.runState (zipWithM (serializeParameter ism name) [0..] pars) (Ctx sl (0,1) (0,1) (SizeNative (0,1) (0,1)) 0 0 0 0 )
   in    sereal

-- Function to check if complex argument has at least one dmahandle or none
hasDmahandleInSerealArg:: [SerealArg] -> Bool
hasDmahandleInSerealArg sereal =
   let
      is' (ComplexArg _ (ComplexSeq _ _ isDmaHandle) _ serealArg) = if isDmaHandle == True then True
                                                         else if length serealArg > 0 then (hasDmahandleInSerealArg serealArg)
                                                         else False
      is' (ComplexArg _ (ComplexStruct _) _ serealArg) = if length serealArg > 0 then (hasDmahandleInSerealArg serealArg)
                                                   else False
      is' (DmahandleArg _ _ _) = True
      is' _ = False
   in not $ null $ filter is' sereal

hasString :: [SerealArg] -> Bool
hasString sereal =
   let
         is' (SequenceBuf _ isString _ _ ) = isString
   in    not $ null $ listify is' sereal

hasDmahandle :: [SerealArg] -> Bool
hasDmahandle sereal =
   let
         is' (DmahandleArg {}) = True
         is' _ = False
   in    not $ null $ filter is' sereal

hasDmahandleROut :: [SerealArg] -> Bool
hasDmahandleROut sereal =
   let
         is' (DmahandleArg (Arg ROut _ _) _ _) = True
         is' _ = False
   in    not $ null $ filter is' sereal

hasComplex :: [SerealArg] -> Bool
hasComplex sereal =
   let
         is' (ComplexArg {}) = True
         is' _ = False
   in    not $ null $ filter is' sereal

hasComplexROut :: [SerealArg] -> Bool
hasComplexROut sereal =
   let
         is' (ComplexArg (Arg ROut _ _) _ _ _) = True
         is' _ = False
   in    not $ null $ filter is' sereal

numSecondaryBufs :: Mode -> [SerealArg] -> Int
numSecondaryBufs md sereal =
   let
         is' (BufferArg _ _ (Buf md' _)) = md == md'
         is' (ComplexArg _ (ComplexSeq _ _ _) prims _) = not $ null $ filter ((==) md . primMode) prims
         is' _ = False
   in    (length $ filter is' sereal)

numRemoteHandles :: Mode -> [SerealArg] -> Int
numRemoteHandles md sereal =
   let
         is' (ObjArg _ (Obj md' _)) = md == md'
         is' _ = False
   in    (length $ filter is' sereal)

numHandles :: Mode -> [SerealArg] -> Int
numHandles md sereal =
   let
         is' (DmahandleArg _ (Fd _) (Obj md' _)) = md == md'
         is' _ = False
   in    (length $ filter is' sereal)

numAllStaticBufs :: Mode -> [SerealArg] -> Int
numAllStaticBufs md sereal = numSecondaryBufs md sereal + numPrimBufs md sereal

numPrimBufs :: Mode -> [SerealArg] -> Int
numPrimBufs md sereal = (length $ primaryBufs md sereal)

type RunState = State.State Ctx

argIn :: Int -> IsMember -> Offset -> Arg
argIn arg ism off = (Arg In (ArgIndex arg ism off) False)

argROut :: Int -> IsMember -> Offset -> Arg
argROut arg ism off = (Arg ROut (ArgIndex arg ism off) False)

primaryBufs :: Mode -> [SerealArg] -> [Primary]
primaryBufs md args = last' $ sortBy (compare `on` Language.Slim.Serialize.primSize) $ filter ((==) md . primMode) $ concatMap toPrimary args
   where
      last' [] = []
      last' ls = [last ls]

primMode :: (Primary) -> Mode
primMode (Prim md _ _) = md

primSize :: (Primary) -> Size
primSize (Prim _ _ sz) = sz

toPrimary :: SerealArg -> [Primary]
toPrimary (ScalarArg _ _ pp _)                 = [pp]
toPrimary (ComplexArg _ (ComplexStruct _) pps _) = pps
toPrimary _                                  = []

serializeParameter :: IsMember -> String -> Int -> Slim.Parameter -> RunState [SerealArg]
serializeParameter ism name arg (Slim.Parameter tt ParameterIn _)     = serializeP (argIn arg ism) tt ParameterIn name
serializeParameter ism name arg (Slim.Parameter tt ParameterROut _)   = serializeP (argROut arg ism) tt ParameterROut name
serializeParameter ism name arg (Slim.Parameter tt ParameterInROut _)   = do
   aa <- serializeP (argIn arg ism) tt ParameterIn name
   bb <- serializeP (argROut arg ism) tt ParameterROut name
   return $ map setIR $ aa ++ bb

setIR :: SerealArg -> SerealArg
setIR ss =
   let   arg = saArg ss
   in    ss { saArg = arg { aIsIR = True }}

type ArgCtor = Offset -> Arg

simpleSizeNative :: SizeNative -> Size
simpleSizeNative (SizeNative s32 s64)
      | s32 /= s64 = error "internal error: simpleSizeNative, bad size"
      | otherwise = s32

serializeP :: ArgCtor -> Slim.Type -> ParameterMode -> String -> RunState [SerealArg]
serializeP _ _ ParameterInROut _ = error "internal error: parameter out is unsupported"

serializeP arg (Type (TypeEnum nm True)            szal) md name = constantScalar arg False szal md nm
serializeP arg (Type (TypeEnum nm False)            szal) md name = constantScalar arg False szal md (name ++ "_" ++ nm)

serializeP arg (Type (TypeStructure stref nm True) szal) md _ = constantStructure arg szal md nm
serializeP arg (Type (TypeStructure stref nm False) szal) md name = constantStructure arg szal md (name ++ "_" ++ nm)
serializeP arg (Type (TypeArray tr _ nme True) szal) md _ = constantStructure arg szal md nme
serializeP arg (Type (TypeArray tr _ nme False) szal) md name = constantStructure arg szal md (name ++ "_" ++ nme)

serializeP arg (Type (TypeSequence tr _ "" _ "--") _) md name = do
   szal <- simpleSizeNative <$> typeNativeSize <$> lookupType tr
   td <- typeDesc <$> lookupType tr
   serializeSeq td arg szal False md "--"

serializeP arg (Type (TypeSequence tr _ "" nme km) _) md name = do
   szal <- simpleSizeNative <$> typeNativeSize <$> lookupType tr
   td <- typeDesc <$> lookupType tr
   let typename
              | nme = km
              | otherwise = name ++ "_" ++ km
   serializeSeq td arg szal False md typename


serializeP arg (Type (TypeSequence tr _ nm nme "--") _) md name = do
   szal <- simpleSizeNative <$> typeNativeSize <$> lookupType tr
   td <- typeDesc <$> lookupType tr
   let typename
              | nme = nm
              | otherwise = name ++ "_" ++ nm
   serializeSeq td arg szal False md typename

serializeP arg (Type (TypeSequence tr _ nm nme km) _) md name = do
   szal <- simpleSizeNative <$> typeNativeSize <$> lookupType tr
   td <- typeDesc <$> lookupType tr
   let typename
              | nme = km
              | otherwise = name ++ "_" ++ km
   serializeSeq td arg szal False md typename
serializeP arg (Type (TypeString _ _ _) _) md _ = serializeSeq (TypeString 0 "" False) arg (1,1) True md ""
serializeP arg (Type (TypeWString _ _ _) _) md _ = serializeSeq (TypeWString 1 "" False) arg (2,2) True md ""
serializeP arg (Type (TypeDmahandle _) _) md _ = dmahandleSeq arg md

serializeP arg (Type (TypeComplexStructure stref nm nme) szal) ParameterIn name = do
   (StructType num memrefs sizein _) <- lookupStruct stref
   mems <- lookupMembers num memrefs
   offset <- addOffset szal sizein (0,1)
   complexStruct (arg offset) ParameterIn mems (0,1) nm name nme

serializeP arg (Type (TypeComplexStructure stref nm nme) szal) ParameterROut name = do
   (StructType num memrefs _ (SizeROut sizein sizerout)) <- lookupStruct stref
   mems <- lookupMembers num memrefs
   offset <- addOffset szal sizein sizerout
   complexStruct (arg offset) ParameterROut mems sizerout nm name nme

serializeP arg (Type (TypeComplexSequence stref nm dm "--") _) ParameterIn name = do
   (SequenceType tr _ sizein _) <- lookupSequence stref
   tp <- lookupType tr
   complexSeq arg ParameterIn tp sizein (0,1) nm name dm

serializeP arg (Type (TypeComplexSequence stref nm dm km) _) ParameterIn name = do
   (SequenceType tr _ sizein _) <- lookupSequence stref
   tp <- lookupType tr
   complexSeq arg ParameterIn tp sizein (0,1) km name dm




serializeP arg (Type (TypeComplexSequence stref nm dm "--") _) ParameterROut name = do
   (SequenceType tr _ _ (SizeROut sizein sizerout)) <- lookupSequence stref
   tp <- lookupType tr
   complexSeq arg ParameterROut tp sizein sizerout  nm name dm

serializeP arg (Type (TypeComplexSequence stref nm dm km) _) ParameterROut name = do
   (SequenceType tr _ _ (SizeROut sizein sizerout)) <- lookupSequence stref
   tp <- lookupType tr
   complexSeq arg ParameterROut tp sizein sizerout  km name dm


serializeP arg (Type (TypeObject iid False) _) ParameterIn _
   | iid == iid_remote_handle             = do
      offset <- addOffsetNat (SizeNative (4,4) (4,4))
      objix <- nextObjIn
      return [ObjArg (arg offset) $ Obj In objix]

serializeP arg (Type (TypeObject iid False) _) ParameterROut _
   | iid == iid_remote_handle             = do
      offset <- addOffsetNat (SizeNative (4,4) (4,4))
      objix <- nextObjROut
      return [ObjArg (arg offset) $ Obj ROut objix]

serializeP _ (Type (TypeUnion {}) _)         _  _ = error "internal error: union types are unsupported"
serializeP _ (Type (TypeInterface {}) _)     _ _ = error "internal error: object interface types are unsupported"
serializeP _ (Type obj@(TypeObject {}) _)    _ _ = error $ "internal error: object types other then remote_handle are unsupported" ++ (show obj)
serializeP _ (Type (TypeComplexUnion {}) _)  _ _ = error "internal error: complex union types are unsupported"
serializeP _ (Type (TypeComplexArray {}) _)  _ _ = error "internal error: complex array types are unsupported"
serializeP arg (Type (Slim.SignedShortType (Just nm)  True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedShortType (Just nm)  _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedShortType _ _ ) szal) md _ = constantScalar arg False szal md "short"
serializeP arg (Type (Slim.SignedLongType (Just nm)  True) szal) md _  = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedLongType (Just nm)  _) szal) md name  = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedLongType _ _) szal) md _  = constantScalar arg False szal md "int"
serializeP arg (Type (Slim.SignedLongLongType (Just nm)  True) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedLongLongType (Just nm)  _) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedLongLongType _ _) szal) md _ = constantScalar arg False szal md "int64"
serializeP arg (Type (Slim.SignedCharFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedCharFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedCharFixedTType _ _ ) szal) md _ = constantScalar arg False szal md "int8_t"
serializeP arg (Type (Slim.SignedShortFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedShortFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedShortFixedTType _  _) szal) md _ = constantScalar arg False szal md "int16_t"
serializeP arg (Type (Slim.SignedLongFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedLongFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedLongFixedTType _  _) szal) md _ = constantScalar arg False szal md "int32_t"
serializeP arg (Type (Slim.SignedLongLongFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedLongLongFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedLongLongFixedTType _ _ ) szal) md _ = constantScalar arg False szal md "int64_t"
serializeP arg (Type (Slim.SignedCharFixedType (Just nm) True ) szal) md  _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedCharFixedType (Just nm) _ ) szal) md  name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedCharFixedType _  _) szal) md  _ = constantScalar arg False szal md "int8"
serializeP arg (Type (Slim.SignedShortFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedShortFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedShortFixedType _ _ ) szal) md _ = constantScalar arg False szal md "int16"
serializeP arg (Type (Slim.SignedLongFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedLongFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedLongFixedType _  _ ) szal) md _ = constantScalar arg False szal md "int32"
serializeP arg (Type (Slim.SignedLongLongFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.SignedLongLongFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.SignedLongLongFixedType _  _) szal) md _ = constantScalar arg False szal md "int64"
serializeP arg (Type (Slim.UnsignedShortType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedShortType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedShortType _  _) szal) md _ = constantScalar arg False szal md "unsigned short"
serializeP arg (Type (Slim.UnsignedLongType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedLongType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedLongType _  _) szal) md _ = constantScalar arg False szal md "unsigned int"
serializeP arg (Type (Slim.UnsignedLongLongType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedLongLongType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedLongLongType _ _) szal) md _ = constantScalar arg False szal md "uint64"
serializeP arg (Type (Slim.UnsignedCharFixedTType (Just nm) True ) szal) md  _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedCharFixedTType (Just nm) _ ) szal) md  name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedCharFixedTType _ _) szal) md  _ = constantScalar arg False szal md "uint8_t"
serializeP arg (Type (Slim.UnsignedShortFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedShortFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedShortFixedTType _ _) szal) md _ = constantScalar arg False szal md "uint16_t"
serializeP arg (Type (Slim.UnsignedLongFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedLongFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedLongFixedTType _ _) szal) md _ = constantScalar arg False szal md "uint32_t"
serializeP arg (Type (Slim.UnsignedLongLongFixedTType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedLongLongFixedTType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedLongLongFixedTType _ _) szal) md _ = constantScalar arg False szal md "uint64_t"
serializeP arg (Type (Slim.UnsignedCharFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedCharFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedCharFixedType _ _) szal) md _ = constantScalar arg False szal md "uint8"
serializeP arg (Type (Slim.UnsignedShortFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedShortFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedShortFixedType _ _) szal) md _ = constantScalar arg False szal md "uint16"
serializeP arg (Type (Slim.UnsignedLongFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedLongFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedLongFixedType _ _) szal) md _ = constantScalar arg False szal md "uint32"
serializeP arg (Type (Slim.UnsignedLongLongFixedType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.UnsignedLongLongFixedType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.UnsignedLongLongFixedType _ _) szal) md _ = constantScalar arg False szal md "uint64"
serializeP arg (Type (Slim.FloatType (Just nm) True ) szal) md _ = constantScalar arg True szal md nm
serializeP arg (Type (Slim.FloatType (Just nm) _ ) szal) md name = constantScalar arg True szal md (name++"_"++nm)
serializeP arg (Type (Slim.FloatType _ _) szal) md _ = constantScalar arg True szal md "float"
serializeP arg (Type (Slim.DoubleType (Just nm) True ) szal) md _ = constantScalar arg True szal md nm
serializeP arg (Type (Slim.DoubleType (Just nm) _ ) szal) md name = constantScalar arg True szal md (name++"_"++nm)
serializeP arg (Type (Slim.DoubleType _ _) szal) md _ = constantScalar arg True szal md "double"
serializeP arg (Type (Slim.CharType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.CharType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.CharType _ _) szal) md _ = constantScalar arg False szal md "char"
serializeP arg (Type (Slim.WideCharType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.WideCharType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.WideCharType _ _) szal) md _ = constantScalar arg False szal md "_wchar_t"
serializeP arg (Type (Slim.OctetType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.OctetType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.OctetType _ _) szal) md _ = constantScalar arg False szal md "unsigned char"
serializeP arg (Type (Slim.BooleanType (Just nm) True ) szal) md _ = constantScalar arg False szal md nm
serializeP arg (Type (Slim.BooleanType (Just nm) _ ) szal) md name = constantScalar arg False szal md (name++"_"++nm)
serializeP arg (Type (Slim.BooleanType _ _) szal) md _ = constantScalar arg False szal md "boolean"

isEmpty :: String -> Bool
isEmpty nm@"" = True
isEmpty _ = False

isStruct :: Slim.Type -> Bool
isStruct (Type (TypeComplexStructure _ _ _)_) = True
isStruct _ = False

lookStruct :: Slim.Type -> StructTypeRef
lookStruct (Type (TypeComplexStructure stref _ _) _) =  stref

lookupStructl :: StructTypeRef -> RunState StructType
lookupStructl (StructTypeRef (Val tr)) = return tr

complexSeq :: ArgCtor -> ParameterMode -> Slim.Type -> Size -> Size -> String -> String -> Bool -> RunState [SerealArg]
complexSeq arg md tp sizein sizerout nm name nme = do
   buf' <- addOffsetNat (SizeNative (4,4) (8,8))
   len <- addOffsetNatIn (4,4)
   (ix, isz) <- currentPrimIn
   let szal = typeNativeSize tp
   let bool = isDmaHandle (typeDesc tp)
   let stringbool = checkString (typeDesc tp)
   let wstringbool = checkWString (typeDesc tp)
   ctx <- State.get
   let sargs = serializeMembers (ctxSlim ctx) True (map (\ tt -> (Slim.Parameter tt md False)) [tp]) name
   prin <- case (sizein) of
      (0,_) -> return []
      _ -> do bin <- nextBufIn
              return $ [Prim In bin sizein]
   prout <- case (sizerout) of
      (0,_) -> return []
      _ -> do brout <- nextBufROut
              return $ [Prim ROut brout sizerout]
   ctx' <- State.get
   let typename
         | stringbool = "_cstring1_t"
         | wstringbool = "_wstring1_t" 
         | nme = nm
         | otherwise = name ++ "_" ++ nm
   let buf = buf' {oBufIn = (ctxNumIn ctx'), oBufROut = (ctxNumROut ctx')}
   return $ [(ScalarArg (arg len) (Len Nothing) (Prim In ix isz) "")
            ,ComplexArg (arg buf) (ComplexSeq szal typename bool) (prin ++ prout) sargs]

complexStruct :: Arg -> ParameterMode -> [Slim.Type] -> Size -> String -> String  -> Bool -> RunState [SerealArg]
complexStruct arg md mems sizerout nm name nme = do
   ctx <- State.get
   (ix, isz) <- currentPrimIn
   (ox, osz) <- currentPrimROut
   let typename
         | nme = nm
         | otherwise = name ++ "_" ++ nm
   let
         sargs = serializeMembers (ctxSlim ctx) True (map (\ tt -> (Slim.Parameter tt md False)) mems) name
         prims
            | (fst sizerout) == 0 = [Prim In ix isz]
            | otherwise = [Prim In ix isz, Prim ROut ox osz]

   return [ComplexArg arg (ComplexStruct typename) prims sargs]

constantScalar :: ArgCtor -> IsFloat -> SizeNative -> ParameterMode -> String -> RunState [SerealArg]
constantScalar _ _ _ ParameterInROut _ = error "internal error: parameter inrout unsupported"
constantScalar arg isf (SizeNative s32 s64) ParameterIn nm
   | s32 /= s64 = error "internal error: bad scalar native size"
   | otherwise = do
   offset <- addOffsetNatIn s32
   (ix, sz) <- currentPrimIn
   return [ScalarArg (arg offset) (Register isf $ snd s32) (Prim In ix sz) nm]

constantScalar arg _ (SizeNative s32 s64) ParameterROut nm
   | s32 /= s64 = error "internal error: bad scalar native size"
   | otherwise = do
   offset <- addOffsetNatROut s32
   (ix,sz) <- currentPrimROut
   return [ScalarArg (arg offset) (Scalar s32) (Prim ROut ix sz) nm]

dmahandleSeq :: ArgCtor -> ParameterMode -> RunState [SerealArg]
dmahandleSeq _ ParameterInROut = error "internal error: parameter inrout unsupported"
dmahandleSeq arg ParameterIn = do
   fd <- addOffsetNat (SizeNative (4,4) (4,4))
   offset <- addOffsetNat (SizeNative (4,4) (4,4))
   len <- addOffsetNat (SizeNative (4,4) (4,4))
   hin <- nextObjIn
   return [DmahandleArg (arg fd) (Fd Nothing) (Obj In hin)
          ,DmahandleArg (arg offset) (DOffset Nothing) (Obj In hin)
          ,DmahandleArg (arg len) (DLen Nothing) (Obj In hin)]

dmahandleSeq arg ParameterROut = do
   fd <- addOffsetNat (SizeNative (4,4) (4,4))
   offset <- addOffsetNat (SizeNative (4,4) (4,4))
   len <- addOffsetNat (SizeNative (4,4) (4,4))
   hout <- nextObjROut
   return [DmahandleArg (arg fd) (Fd Nothing) (Obj ROut hout)
          ,DmahandleArg (arg offset) (DOffset Nothing) (Obj ROut hout)
          ,DmahandleArg (arg len) (DLen Nothing) (Obj ROut hout)]

serializeSeq :: TypeDesc -> ArgCtor -> Size -> Bool -> ParameterMode -> String -> RunState [SerealArg]
serializeSeq _ _ _ _ ParameterInROut nm = error "internal error: parameter inrout unsupported"
serializeSeq td arg szal isString ParameterIn nm = do
   buf <- addOffsetNat (SizeNative (4,4) (8,8))
   len <- addOffsetNatIn (4,4)
   bin <- nextBufIn
   (ix,sz) <- currentPrimIn
   let lenal = if isString then Just (snd szal) else Nothing
   return [ScalarArg (arg len) (Len lenal) (Prim In ix sz) ""
          ,BufferArg (arg buf) (SequenceBuf szal isString td nm) (Buf In bin)]

serializeSeq td arg szal isString ParameterROut nm = do
   buf <- addOffsetNat (SizeNative (4,4) (8,8))
   len <- addOffsetNatIn (4,4)
   brout <- nextBufROut
   (pin,isz) <- currentPrimIn
   let lenal = if isString then Just (snd szal) else Nothing
   return [ScalarArg (arg len) (Len lenal) (Prim In pin isz) ""
          ,BufferArg (arg buf) (SequenceBuf szal isString td nm) (Buf ROut brout)]


constantStructure :: ArgCtor -> SizeNative -> ParameterMode -> String -> RunState [SerealArg]
constantStructure _ _ ParameterInROut _ = error "internal error: parameter inrout unsupported"
constantStructure arg (SizeNative s32 s64) ParameterIn nm
   | s32 /= s64 = error "internal error: bad scalar native size"
   | otherwise = do
   offset <- addOffsetNatIn s32
   (ix, sz) <- currentPrimIn
   return [ScalarArg (arg offset) (Scalar s32) (Prim In ix sz) nm]

constantStructure arg (SizeNative s32 s64) ParameterROut nm
   | s32 /= s64 = error "internal error: bad scalar native size"
   | otherwise = do
   offset <- addOffsetNatROut s32
   (ix,sz) <- currentPrimROut
   return [ScalarArg (arg offset) (Scalar s32) (Prim ROut ix sz) nm]

data Ctx = Ctx { ctxSlim :: Slim
               , ctxPrimIn :: Size
               , ctxPrimROut :: Size
               , ctxNative :: SizeNative
               , ctxNumIn :: BufferIndex
               , ctxNumROut :: BufferIndex
               , ctxObjIn :: ObjIndex
               , ctxObjROut :: ObjIndex
               }
               deriving Show

alignAdd :: Size -> Size -> (Int, Size)
alignAdd _ (_, 0) = error "internal error: alignAdd al 0"
alignAdd (_,0) _ = error "internal error: alignAdd al 0"
alignAdd (bz,bl) (sz, al)
   | unaligned == 0 = (ibz, (bz + sz, newal))
   | otherwise      = (aligned, ((fromIntegral aligned) + sz, newal))
      where
         newal = max bl al
         aligned = ibz + (ial - unaligned)
         unaligned = ibz `mod` ial
         ial = fromIntegral al
         ibz = fromIntegral bz

isize :: Size -> Int
isize (sz,_) = fromIntegral sz

nextBufIn :: RunState BufferIndex
nextBufIn = do
   ctx <- State.get
   let bin = (ctxNumIn ctx)
   State.put $ ctx { ctxNumIn = (1 + (ctxNumIn ctx)) }
   return bin

nextBufROut :: RunState BufferIndex
nextBufROut = do
   ctx <- State.get
   let bout = (ctxNumROut ctx)
   State.put ctx { ctxNumROut = (1 + ctxNumROut ctx) }
   return bout

currentPrimIn :: RunState (BufferIndex, Size)
currentPrimIn = do
   ctx <- State.get
   return $ (0, (ctxPrimIn ctx))

currentPrimROut :: RunState (BufferIndex, Size)
currentPrimROut = do
   ctx <- State.get
   return $ (0, (ctxPrimROut ctx))

alignAddIn :: Size -> RunState Int
alignAddIn szal = do
   ctx <- State.get
   let   oin = ctxPrimIn ctx
         (offset, newin) = alignAdd oin szal
   State.put $ ctx { ctxPrimIn = newin }
   return offset

alignAddROut :: Size -> RunState Int
alignAddROut szal = do
   ctx <- State.get
   let   rout = ctxPrimROut ctx
         (offset, newrout) = alignAdd rout szal
   State.put $ ctx { ctxPrimROut = newrout }
   return offset

alignAddNative :: SizeNative -> RunState (Int, Int)
alignAddNative (SizeNative is32 is64) = do
   ctx <- State.get
   let   (SizeNative cs32 cs64) = ctxNative ctx
         (o32, ns32) = alignAdd cs32 is32
         (o64, ns64) = alignAdd cs64 is64
   State.put $ ctx { ctxNative = (SizeNative ns32 ns64) }
   return (o32, o64)

nextObjIn :: RunState ObjIndex
nextObjIn = do
   ctx <- State.get
   let bin = (ctxObjIn ctx)
   State.put $ ctx { ctxObjIn = (1 + (ctxObjIn ctx)) }
   return bin

nextObjROut :: RunState ObjIndex
nextObjROut = do
   ctx <- State.get
   let rout = (ctxObjROut ctx)
   State.put $ ctx { ctxObjROut = (1 + (ctxObjROut ctx)) }
   return rout

addOffsetNat :: SizeNative -> RunState Offset
addOffsetNat szn = addOffset szn (0,1) (0,1)

addOffsetNatIn :: Size -> RunState Offset
addOffsetNatIn szal =  addOffset (SizeNative szal szal) szal (0,1)

addOffsetNatROut :: Size -> RunState Offset
addOffsetNatROut szal = addOffset (SizeNative szal szal) (0,1) szal

addOffset :: SizeNative -> Size -> Size -> RunState Offset
addOffset nat szin szrout = do
   ctx <- State.get
   onat <- alignAddNative nat
   oin <- alignAddIn szin
   orout <- alignAddROut szrout
   let bin = (ctxNumIn ctx)
   let bout = (ctxNumROut ctx)
   return (Offset nat onat oin orout bin bout)

lookupType :: TypeRef -> RunState Slim.Type
lookupType (Slim.TypeRef (Val tr)) = return tr
lookupType (Slim.TypeRef (Ref tr)) = do
   ctx <- State.get
   return $ (types (ctxSlim ctx)) !! tr

lookupSequence :: SequenceTypeRef -> RunState SequenceType
lookupSequence (SequenceTypeRef (Val tr)) = return tr
lookupSequence (SequenceTypeRef (Ref tr)) = do
   ctx <- State.get
   return $  (sequenceTypes (ctxSlim ctx)) !! tr

lookupStruct :: StructTypeRef -> RunState StructType
lookupStruct (StructTypeRef (Val tr)) = return tr
lookupStruct (StructTypeRef (Ref tr)) = do
   ctx <- State.get
   return $  (structTypes (ctxSlim ctx)) !! tr

lookupMembers :: Integral a => a -> TypeRefArrayRef -> RunState [Slim.Type]
lookupMembers _ (TypeRefArrayRef (Val _)) = error "internal error: unexpected typerefarray val"
lookupMembers num (TypeRefArrayRef (Ref tr)) = do
   ctx <- State.get
   let sl = (ctxSlim ctx)
   mapM lookupType $ take (fromIntegral num) $ drop tr (typeArrays sl)

typeNativeSize :: Slim.Type -> SizeNative
typeNativeSize (Type _ sz) = sz

typeDesc :: Slim.Type -> TypeDesc
typeDesc (Type ts _) = ts

reflist :: (Data a2, Integral a1) => [a] -> a1 -> a2 -> [a]
reflist ls num ref = take (fromIntegral num) $ drop (unref ref) ls

isDmaHandle :: Slim.TypeDesc -> Bool
isDmaHandle ts@(Slim.TypeDmahandle _) = True
isDmaHandle _ = False

checkString :: Slim.TypeDesc -> Bool
checkString ts@(Slim.TypeString _ "" _) = True
checkString _ = False

checkWString :: Slim.TypeDesc -> Bool
checkWString ts@(Slim.TypeWString _ "" _) = True
checkWString _ = False


-- Removing as part of QAIC Source Code Cleanup

-- type StructName = String

-- data RemoteHandle = RemoteHandle


-- saNativeSZ :: SerealArg -> SizeNative
-- saNativeSZ sa = aNativeSZ $ saArg $ sa

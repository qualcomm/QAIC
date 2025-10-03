-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

module Language.SO.Args where

import Data.Word                                      ( Word8 )
import Data.List                                      ( sortBy )
import Language.SO.ToHeader                           ( commals )
import Language.Slim.Data
import Language.Slim.Serialize
import Text.PrettyPrint
import Prelude hiding ( (<>) )
--import Debug.Trace(trace)
--dbg :: Show a => a -> a
--dbg aa = (show aa) `trace` aa

methodArgsOnly :: [SerealArg] -> [SerealArg]
methodArgsOnly args = sortBy saOrder $ filter (isMethodArg) args

missingArgsOnly :: [SerealArg] -> [SerealArg]
missingArgsOnly args = sortBy saOrder $ filter (not . isMethodArg) args

methodValTypes :: [SerealArg]  -> [Doc]
methodValTypes sereal = map valType (methodvalType)
      where
      methodvalType = methodArgsOnly sereal


refsToMethodVals :: [SerealArg] -> [Doc]
refsToMethodVals sereal = map (\ sa -> refToVal sa (saName sa)) $ methodArgsOnly sereal

valType :: SerealArg -> Doc
valType (ScalarArg (Arg ROut _ _)  (Register isf al) _ nm)  								= 					typeFromAlSl isf al nm <> text "*"
valType (ScalarArg (Arg ROut _ _)  (Scalar sz) _ nm)       									=				 	typeFromAlSl False (snd sz) nm <> text "*"
valType (ScalarArg (Arg ROut _ _)  (Len _) _ _)          									= 					typeFromAlSl False 4 "int"
valType (ScalarArg _ (Register isf al) _ nm)            									=				 	typeFromAlSl isf al nm
valType (ScalarArg _ (Scalar sz) _ nm)                  									=				 	text "const " <> typeFromAlSl False (snd sz) nm <> text "*"
valType (ScalarArg _ (Len _) _ _)                     										= 					typeFromAlSl False 4 "int"
valType (BufferArg arg  (SequenceBuf _ _ (SignedShortType _ _) "--") _)            			= 					isIn arg <> text "short*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedShortType _ _) nm) _)            			= 					isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongType _ _) "--") _)            			    =               isIn arg <> text "int*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongType _ _) nm) _)            			    =               isIn arg <>  text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongLongType _ _) "--") _)         			=                	isIn arg <> text "int64*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongLongType _ _) nm) _)         			=                	isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedCharFixedTType _ _) "--") _)             		=          			isIn arg <> text "int8_t*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedCharFixedTType _ _) nm) _)             		=          			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedShortFixedTType _ _) "--") _)                  =         			isIn arg <> text "int16_t*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedShortFixedTType _ _) nm) _)                  =         			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongFixedTType _ _) "--") _)            	    =          			isIn arg <> text "int32_t*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongFixedTType _ _) nm) _)            	    =          			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongLongFixedTType _ _) "--") _)               =      				isIn arg <> text "int64_t*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongLongFixedTType _ _) nm) _)               =      				isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedCharFixedType _ _) "--") _)           	 		=            		isIn arg <> text "int8*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedCharFixedType _ _) nm) _)           	 		=            		isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedShortFixedType _ _) "--") _)           		=          			isIn arg <> text "int16*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedShortFixedType _ _) nm) _)           		=          			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongFixedType _ _) "--") _)           			=             		isIn arg <> text "int32*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongFixedType _ _) nm) _)           			=             		isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongLongFixedType _ _) "--") _)           		=        			isIn arg <> text "int64*"
valType (BufferArg arg  (SequenceBuf _ _ (SignedLongLongFixedType _ _) nm) _)           		=        			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedShortType _ _) "--") _)                      =                   isIn arg <> text "unsigned short*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedShortType _ _) nm) _)                      =                   isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongType _ _) "--") _)                       =             		isIn arg <> text "unsigned int*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongType _ _) nm) _)                       =             		isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongLongType _ _) "--") _)                   =          			isIn arg <> text "uint64*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongLongType _ _) nm) _)                   =          			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedCharFixedTType _ _) "--") _)                 =       			isIn arg <> text "uint8_t*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedCharFixedTType _ _) nm) _)                 =       			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedShortFixedTType _ _) "--") _)                =       			isIn arg <> text "uint16_t*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedShortFixedTType _ _) nm) _)                =       			isIn arg <> text nm <>  text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongFixedTType _ _) "--") _)                 =        			isIn arg <> text "uint32_t*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongFixedTType _ _) nm) _)                 =        			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongLongFixedTType _ _) "--") _)             =   				isIn arg <> text "uint64_t*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongLongFixedTType _ _) nm) _)             =   				isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedCharFixedType _ _) "--") _)                  =              		isIn arg <> text "uint8*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedCharFixedType _ _) nm) _)                  =              		isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedShortFixedType _ _) "--") _)                 =        			isIn arg <> text "uint16*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedShortFixedType _ _) nm) _)                 =        			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongFixedType _ _) "--") _)                  =         			isIn arg <> text "uint32*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongFixedType _ _) nm) _)                  =         			isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongLongFixedType _ _) "--") _)              =     				isIn arg <> text "uint64*"
valType (BufferArg arg  (SequenceBuf _ _ (UnsignedLongLongFixedType _ _) nm) _)              =     				isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (FloatType _ _) "--") _)                              =                   isIn arg <> text "float*"
valType (BufferArg arg  (SequenceBuf _ _ (FloatType _ _) nm) _)                              =                   isIn arg <> text nm <>  text "*"
valType (BufferArg arg  (SequenceBuf _ _ (DoubleType _ _) "--") _)                             =                    isIn arg <> text "double*"
valType (BufferArg arg  (SequenceBuf _ _ (DoubleType _ _) nm) _)                             =                    isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (CharType _ _) "--") _)                               =                     isIn arg <> text "char*"
valType (BufferArg arg  (SequenceBuf _ _ (CharType _ _) nm) _)                               =                     isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (WideCharType _ _) "--") _)                           =                     isIn arg <> text "_wchar_t*"
valType (BufferArg arg  (SequenceBuf _ _ (WideCharType _ _) nm) _)                           =                     isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (BooleanType _ _) "--") _)                            =                  	isIn arg <> text "boolean*"
valType (BufferArg arg  (SequenceBuf _ _ (BooleanType _ _) nm) _)                            =                  	isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (OctetType _ _) "--") _)                              =                   isIn arg <> text "unsigned char*"
valType (BufferArg arg  (SequenceBuf _ _ (OctetType _ _) nm) _)                              =                   isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (TypeStructure _ _ _) nm) _)                       =                	isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (TypeString 0 _ _) _) _)                               =                   isIn arg <> text "char*"
valType (BufferArg arg  (SequenceBuf _ _ (TypeWString 1 _ _) _) _)                              =                  isIn arg <> text "_wchar_t*"
valType (BufferArg arg  (SequenceBuf _ _ (TypeArray _ _ _ _) _) _)                              =                  isIn arg <> text "int*"
valType (BufferArg arg  (SequenceBuf _ _ (TypeEnum _ _) nm) _)                              =                  isIn arg <> text nm <> text "*"
valType (BufferArg arg  (SequenceBuf _ _ (TypeDmahandle _) _) _)                              =                  isIn arg <> text "_dmahandle1_t*"
valType (ComplexArg arg (ComplexStruct nm) _ _)                                             =  					isIn arg <> text nm <> text "*"
valType (ComplexArg arg (ComplexSeq _ _ True) _ _)                                              =					 isIn arg <> text "_dmahandle1_t*"
valType (ComplexArg arg (ComplexSeq _ nm _) _ _)                                              =					 isIn arg <> text nm <> text "*"
valType (ObjArg (Arg ROut _ _)  _)                                                         = 					text "remote_handle64*"
valType (ObjArg _  _)                                                                     = 					text "remote_handle64"
valType (DmahandleArg _  (Fd _) _)                                                        = 					text "int"
valType (DmahandleArg _  _ _)                                                              = 					text "uint32"


isIn :: Arg -> Doc
isIn (Arg In _ _) = text "const "
isIn (Arg ROut _ _) = text ""


refType :: SerealArg -> Doc
refType sa@(ScalarArg (Arg In _ _)  (Register {}) _ _) = valType sa <> text "*"
refType sa@(ScalarArg (Arg In _ _)  (Len _) _ nm)       = valType  sa <> text "*"
refType sa@(ScalarArg (Arg ROut _ _)  (Len _) _ nm)     = valType  sa <> text "*"
refType sa@(BufferArg _ (SequenceBuf _ _ _ _) _)       = valType  sa <> text "*"
refType sa@(ComplexArg _ (ComplexSeq {}) _ _)      = valType  sa <> text "*"
refType sa@(ObjArg _ _)                            = valType  sa <> text "*"
refType sa@(DmahandleArg _ _ _)                    = valType  sa <> text "*"
refType sa                                         = valType  sa

valToRef :: SerealArg -> Doc ->  Doc
valToRef sa@(ScalarArg (Arg In _ False)  (Register {}) _ _) nm = parens (refType sa) <> text "&" <> nm
valToRef sa@(ScalarArg (Arg In _ _)  (Len _) _ _)       nm = parens (refType sa) <> text "&" <> nm
valToRef sa@(ScalarArg (Arg ROut _ _)  (Len _) _ _)     nm = parens (refType sa) <> text "&" <> nm
valToRef sa@(BufferArg _ (SequenceBuf _ _ _ _) _)       nm = parens (refType sa) <> text "&" <> nm
valToRef sa@(ComplexArg _ (ComplexSeq {}) _ _)      nm = parens (refType sa) <> text "&" <> nm
valToRef sa@(DmahandleArg (Arg _ _ _) _  _)         nm = parens (refType sa) <> text "&" <> nm
valToRef sa                                         nm = parens (refType sa) <> nm

offsetToRef :: SerealArg -> Doc ->  Doc
offsetToRef sa@(ComplexArg _ (ComplexStruct _) _ _)  nm = parens (refType sa) <> text "&" <> nm
offsetToRef sa@(ScalarArg _  (Scalar _) _ _ )        nm = parens (refType sa) <> text "&" <> nm
offsetToRef sa                                     nm = valToRef sa nm

refToVal :: SerealArg -> Doc -> Doc
refToVal sa@(ScalarArg (Arg In _ _)  (Register True _) _ _) nm = text "*" <> nm
refToVal sa@(ScalarArg (Arg In _ _)  (Register {}) _ _) nm = parens (valType sa)  <> text "*" <>  nm
refToVal sa@(ScalarArg (Arg In _ _)  (Len _) _ _)       nm = parens (valType sa)  <> text "*" <> nm
refToVal sa@(ScalarArg (Arg ROut _ _)  (Len _) _ _)     nm = parens (valType sa)  <> text "*" <> nm
refToVal sa@(BufferArg _ (SequenceBuf _ _ _ _) _)       nm = parens (valType sa)  <> text "*" <> nm
refToVal sa@(ComplexArg _ (ComplexSeq {}) _ _)      nm = parens (valType sa)  <> text "*" <> nm
refToVal sa@(ObjArg (Arg In _ _) _)                 nm = parens (valType sa)  <> text "*" <> nm
refToVal sa@(DmahandleArg (Arg _ _ _) _ _)          nm = parens (valType sa)  <> text "*" <> nm
refToVal sa                                       nm = parens (valType sa ) <> nm



 



saOrder :: SerealArg -> SerealArg -> Ordering
saOrder aa bb
   | (saN aa) /= (saN bb) = compare (saN aa) (saN bb)
saOrder aa bb
   | (saIsIn aa) && (not $ saIsIn bb) = LT
saOrder (BufferArg {}) (ScalarArg {}) = LT
saOrder (ScalarArg {}) (BufferArg {}) = GT
saOrder (ComplexArg _ (ComplexSeq {}) _ _) _ = LT
saOrder _ (ComplexArg _ (ComplexSeq {}) _ _) = GT
saOrder aa bb       = compare (saN aa) (saN bb)

isMethodArg :: SerealArg -> Bool
isMethodArg (ScalarArg (Arg In _ _) (Len (Just _))  _ _) = False
isMethodArg _                                        = True

saName :: SerealArg -> Doc
saName so
   | (saIsIn so)              = text "_in" <> int (saN so) <> saConstraintName so
   | otherwise                = text "_rout" <> int (saN so) <> saConstraintName so

saConstraintName :: SerealArg -> Doc
saConstraintName (ScalarArg _ (Len _) _ _)  = text "Len"
saConstraintName (DmahandleArg _ (Fd _) _)      = text "Fd"
saConstraintName (DmahandleArg _ (DOffset _) _) = text "Offset"
saConstraintName (DmahandleArg _ (DLen _) _)    = text "Len"
saConstraintName _                        = empty

ifPtr32 :: Doc -> Doc -> Doc
ifPtr32 is32 is64
   | (render is32) == (render is64) = is32
   | otherwise = text "SLIM_IFPTR32" <> parens (commals $ [is32, is64])

arrayElems :: SizeNative -> Doc
arrayElems (SizeNative s32 s64) = ifPtr32 (int $ arrayElems' s32) (int $ arrayElems' s64)

arrayElems' :: Size -> Int
arrayElems' (_, 0) = error "internal error: arraElems 0 al"
arrayElems' (sz, al)
   | isz `mod` ial == 0 = isz `div` ial
   | otherwise = (isz `div` ial) + 1
   where
      isz = fromIntegral sz
      ial = fromIntegral al

parm :: String -> Int -> Doc
parm pre ii = text pre <> int ii

typeFromAlS :: Bool -> Word8 -> Doc
typeFromAlS isf al = typeFromAl isf (al,al)

typeFromAlSl :: Bool -> Word8 -> String -> Doc
typeFromAlSl isf al "" = typeFromAl isf (al,al)
typeFromAlSl isf al nm = typeFromAll isf (al,al) nm

typeFromAl :: Bool -> (Word8,Word8) -> Doc
typeFromAl True (8,8) = text "double"
typeFromAl True (4,4) = text "float"
typeFromAl _    (4,8) = text "uintptr_t"
typeFromAl _  (a32,a64)
   | a32 == a64 = text "uint" <> (int $ (fromIntegral a32) * 8) <> text "_t"
typeFromAl _  _ = error "internal error: typeFromAl unexpected alignment"

typeFromAll :: Bool -> (Word8,Word8) -> String -> Doc
typeFromAll _ _ nm = text nm
typeFromAll _  _ _ = error "internal error: typeFromAl unexpected alignment"

val :: Doc -> Doc
val pt = pt <> brackets (int 0)

eq :: Doc -> Doc -> Doc
eq aa bb = aa <+> text "=" <+> bb

eqq :: Doc -> Doc -> Doc
eqq aa bb = aa <+> text "==" <+> bb

minus :: Doc -> Doc -> Doc
minus aa bb = parens (aa <+> text "-" <+> bb)

plus :: Doc -> Doc -> Doc
plus aa bb = parens (aa <+> text "+" <+> bb)

mult :: Doc -> Doc -> Doc
mult aa bb = parens (aa <+> text "*" <+> bb)

divd :: Doc -> Doc -> Doc
divd aa bb = parens (aa <+> text "/" <+> bb)

gt :: Doc -> Doc -> Doc
gt aa bb = aa <+> text ">" <+> bb

annd :: Doc -> Doc -> Doc
annd aa bb = aa <+> text "&&" <+> bb

lt :: Doc -> Doc -> Doc
lt aa bb = aa <+> text "<" <+> bb

lteq :: Doc -> Doc -> Doc
lteq aa bb = aa <+> text "<=" <+> bb

gteq :: Doc -> Doc -> Doc
gteq aa bb = aa <+> text ">=" <+> bb

igteq :: Doc -> Doc -> Doc
igteq aa bb = parens aa <+> text ">=" <+> text "(size_t)" <> parens bb

call :: Doc -> [Doc] -> Doc
call ff args = ff <> parens (commals args)

methodArgsOnlyl :: [SerealArg] -> [SerealArg]
methodArgsOnlyl args = sortBy saOrder $ filter (isMethodArg) args

missingArgsOnlyl :: [SerealArg] -> [SerealArg]
missingArgsOnlyl args = sortBy saOrder $ filter (not . isMethodArg) args

declareMethodRefsl :: [SerealArg] -> [Doc]
declareMethodRefsl sereal = declareRefsl $ methodArgsOnlyl sereal

declareRefsl :: [SerealArg] -> [Doc]
declareRefsl sereal = map (\ sa -> declareRefl (saName sa)  sa) $ sortBy saOrder sereal

declareMissingRefsl :: [SerealArg] -> [Doc]
declareMissingRefsl sereal = declareRefsl $ missingArgsOnlyl sereal

methodValTypesl :: [SerealArg] -> [Doc]
methodValTypesl sereal = map valTypel $ methodArgsOnlyl sereal

refsToMethodValsl :: [SerealArg] -> [Doc]
refsToMethodValsl sereal = map (\ sa -> refToVall sa (saName sa)) $ methodArgsOnlyl sereal

valTypel :: SerealArg -> Doc
valTypel (ScalarArg (Arg ROut _ _)  (Register isf al) _ _) = typeFromAlS isf al <> text "*"
valTypel (ScalarArg (Arg ROut _ _)  (Scalar sz) _ _)       = typeFromAlS False (snd sz) <> text "*"
valTypel (ScalarArg (Arg ROut _ _)  (Len _) _ _)           = typeFromAlS False 4
valTypel (ScalarArg _ (Register isf al) _ _)             = typeFromAlS isf al
valTypel (ScalarArg _ (Scalar sz) _ _)                   = typeFromAlS False (snd sz) <> text "*"
valTypel (ScalarArg _ (Len _) _ _)                       = typeFromAlS False 4
valTypel (BufferArg _ (SequenceBuf (_,2) True _ _) _)      = text "_wchar_t*"
valTypel (BufferArg _ (SequenceBuf _ _ _ _) _)             = text "char*"
valTypel (ComplexArg arg (ComplexStruct _)  _ _)            = typeFromAl False (sznAl $ aNativeSZ arg) <> text "*"
valTypel (ComplexArg _ (ComplexSeq {}) _ _)            = text "void*"
valTypel (ObjArg (Arg ROut _ _)  _)                    = text "remote_handle64*"
valTypel (ObjArg _  _)                                 = text "remote_handle64"
valTypel (DmahandleArg _  (Fd _) _)                    = typeFromAlS False 4
valTypel (DmahandleArg _  _ _)                         = typeFromAlS False 4

refTypel :: SerealArg -> Doc
refTypel sa@(ScalarArg (Arg In _ _)  (Register {}) _ _) = valTypel sa <> text "*"
refTypel sa@(ScalarArg (Arg In _ _)  (Len _) _ _)       = valTypel sa <> text "*"
refTypel sa@(ScalarArg (Arg ROut _ _)  (Len _) _ _)     = valTypel sa <> text "*"
refTypel sa@(BufferArg _ (SequenceBuf _ _ _ _) _ )       = valTypel sa <> text "*"
refTypel sa@(ComplexArg _ (ComplexSeq {}) _ _)      = valTypel sa <> text "*"
refTypel sa@(ObjArg _ _)                            = valTypel sa <> text "*"
refTypel sa@(DmahandleArg _ _ _)                    = valTypel sa <> text "*"
refTypel sa                                         = valTypel sa

valToRefl :: SerealArg -> Doc ->  Doc
valToRefl sa@(ScalarArg (Arg In _ False)  (Register {}) _ _) nm = parens (refTypel sa) <> text "&" <> nm
valToRefl sa@(ScalarArg (Arg In _ _)  (Len _) _ _)       nm = parens (refTypel sa) <> text "&" <> nm
valToRefl sa@(ScalarArg (Arg ROut _ _)  (Len _) _ _)     nm = parens (refTypel sa) <> text "&" <> nm
valToRefl sa@(BufferArg _ (SequenceBuf _ _ _ _) _)       nm = parens (refTypel sa) <> text "&" <> nm
valToRefl sa@(ComplexArg _ (ComplexSeq {}) _ _)      nm = parens (refTypel sa) <> text "&" <> nm
valToRefl sa@(DmahandleArg (Arg _ _ _) _  _)         nm = parens (refTypel sa) <> text "&" <> nm
valToRefl sa                                         nm = parens (refTypel sa) <> nm

offsetToRefl :: SerealArg -> Doc ->  Doc
offsetToRefl sa@(ComplexArg _ (ComplexStruct _) _ _)  nm = parens (refTypel sa) <> text "&" <> nm
offsetToRefl sa@(ScalarArg _  (Scalar _) _ _ )        nm = parens (refTypel sa) <> text "&" <> nm
offsetToRefl sa                                     nm = valToRefl sa nm

refToVall :: SerealArg -> Doc -> Doc
refToVall (ScalarArg (Arg In _ _)  (Register {}) _ _) nm = text "*" <> nm
refToVall (ScalarArg (Arg In _ _)  (Len _) _ _)       nm = text "*" <> nm
refToVall (ScalarArg (Arg ROut _ _)  (Len _) _ _)     nm = text "*" <> nm
refToVall (BufferArg _ (SequenceBuf _ _ _ _) _)       nm = text "*" <> nm
refToVall (ComplexArg _ (ComplexSeq {}) _ _)      nm = text "*" <> nm
refToVall (ObjArg (Arg In _ _) _)                 nm = text "*" <> nm
refToVall (DmahandleArg (Arg _ _ _) _ _)          nm = text "*" <> nm
refToVall _                                       nm = nm

declareRefl :: Doc -> SerealArg ->  Doc
declareRefl nm sa@(ScalarArg (Arg In _ _)  (Len _) _ _)          = (valTypel sa) <+> nm <> brackets (int 1)
declareRefl nm sa@(ScalarArg (Arg In _ _)  (Register {}) _ _)    = (valTypel sa) <+> nm <> brackets (int 1)
declareRefl nm (ScalarArg (Arg ROut _ _)  (Register isf al) _ _) = typeFromAlS isf al <+> nm <> brackets (int 1)
declareRefl nm (ScalarArg (Arg ROut _ _)  (Len _) _ _)           = typeFromAlS False 4 <+> nm <> brackets (int 1)
declareRefl nm (ScalarArg (Arg _ _ _)  (Scalar sz) _ _)          = typeFromAlS False (snd sz) <+>  nm <> brackets (int $ arrayElems' sz)
declareRefl nm sa@(BufferArg _ (SequenceBuf _ _ _ _) _)          = (valTypel sa) <+> nm <> brackets (int 1)
declareRefl nm sa@(ComplexArg _ (ComplexSeq {}) _ _)         = (valTypel sa) <+> nm <> brackets (int 1)
declareRefl nm (ComplexArg arg ( ComplexStruct _) _ _)            = typeFromAl False (sznAl $ aNativeSZ arg) <+>  nm <> brackets (arrayElems $ aNativeSZ arg)
declareRefl nm (DmahandleArg (Arg _ _ _) (Fd _) _)           = typeFromAlS False 4 <+> nm <> brackets (int 1)
declareRefl nm (DmahandleArg (Arg _ _ _) _ _)                = typeFromAlS False 4 <+> nm <> brackets (int 1)
declareRefl nm (ObjArg _  _)                              = text "remote_handle64" <+> nm <> brackets (int 1)

-- Removing as part of QAIC Source Code Cleanup

-- mapL :: (a -> b -> c) -> a -> [b] -> [c]
-- mapL f nm [] = []
-- mapL f nm (x:xs) = f nm x : mapL f nm xs


-- neq :: Doc -> Doc -> Doc
-- neq aa bb = aa <+> text "!=" <+> bb


-- orr :: Doc -> Doc -> Doc
-- orr aa bb = aa <+> text "||" <+> bb
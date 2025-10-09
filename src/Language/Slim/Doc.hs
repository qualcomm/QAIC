-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

{-# OPTIONS_GHC -fno-warn-orphans -XFlexibleInstances#-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Slim.Doc(toC,toCif,toH,slimFactory,interfaceArr) where

import Language.Slim.Data
import Language.C.Pretty
import Text.PrettyPrint
import qualified Language.Slim.Ref                    as Ref
import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Language.Idl.ToPalette               as TP
import qualified Language.Idl.Data                    as ID
import qualified Data.List                            as List
import qualified Language.C.Pretty                    as Pretty
import qualified Language.C.TypeDesc                  as CT
import Prelude hiding ( (<>) )
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Char(toUpper)
import Data.Int(Int32)

toC :: Slim -> Doc
toC slm =
         text "#include <stdint.h>"
   $+$   text ""
   $+$   text "#ifndef __QAIC_SLIM"
   $+$   text "#define __QAIC_SLIM(ff) ff"
   $+$   text "#endif"
   $+$   text "#ifndef __QAIC_SLIM_EXPORT"
   $+$   text "#define __QAIC_SLIM_EXPORT"
   $+$   text "#endif"
   $+$   text ""
   $+$   declare "Type types"                         (types                 slm)
   $+$   toDoc' "uint8_t value8s"                       (value8s               slm)
   $+$   toDoc' "uint16_t value16s"                     (value16s              slm)
   $+$   toDoc' "uint32_t value32s"                     (value32s              slm)
   $+$   toDoc' "uint64_t value64s"                     (value64s              slm)
   $+$   toDoc' "Type* const typeArrays"              (typeArrays            slm)
   $+$   toDoc' "UnionType unionTypes"                (unionTypes            slm)
   $+$   toDoc' "StructType structTypes"              (structTypes           slm)
   $+$   toDoc' "SequenceType sequenceTypes"          (sequenceTypes         slm)
   $+$   toDoc' "Type types"                          (types                 slm)
   $+$   toDoc' "Parameter parameters"                (parameters            slm)
   $+$   toDoc' "Parameter* const parameterArrays"    (parameterArrays       slm)
   $+$   toDoc' "Method methods"                      (methods               slm)
   $+$   toDoc' "Method* const methodArrays"          (methodArrays          slm)
   $+$   string "char strings"                        (strings               slm)
   $+$   toDoc' "uint16_t methodStrings"                (methodStrings         slm)
   $+$   toDoc' "uint16_t methodStringsArrays"          (methodStringsArrays   slm)
   $+$   vcat (map declareIface (interfaces slm))
   where
      declareIface ii = text "__QAIC_SLIM_EXPORT const Interface __QAIC_SLIM" <> parens (ifaceName ii <> text "_slim") <+> text "=" <+> pretty cf ii <> semi
      declare _ [] = empty
      declare nm lst = text "static const " <> text nm <> brackets (int (length lst)) <> semi
      extern _ [] = empty
      extern nm lst = text "extern const " <> text nm <> brackets (int (length lst)) <> semi
      cf = Pretty.new
      string _ [] = empty
      string nm list =
         text "static const " <> text nm <> brackets (int (incr (length list))) <> text " = " <> text "\"" <> (hcat $ map (pretty cf) list) <> text "\"" <> semi
      toDoc' _ [] = empty
      toDoc' nm list =
         text "static const " <> text nm <> brackets (int (length list)) <> text " = " <> braces (commas (map (pretty cf) (list))) <> semi
      toDocx' _ [] = empty
      toDocx' nm list =
         text "const " <> text nm <> brackets (int (length list)) <> text " = " <> braces (commas (map (pretty cf) (list))) <> semi

incr :: Int -> Int
incr i = i + 1

interfaceArr :: Slim -> Doc
interfaceArr slm = declareIfaces "Interface* interfaces"        (interfaces            slm)
   where
      declareIfaces _ [] = empty
      declareIfaces nm lst = text "static const " <> text nm <> brackets (int (length lst)) <+> text "=" <+> braces (commas (map (\ ii -> text "&" <> ifaceName ii) lst)) <> semi

ifaceName :: Interface -> Doc
ifaceName (Interface sn _ _ _ _ _) = (hcat $ map text $ List.intersperse "_" $ reverse sn)

slimFactory :: String -> Doc
slimFactory name =
         text "#include \"AEECSlimInterfaceFactory.h\""
   $+$   text "#include \"" <> text name <> text "_slim.h\""
   $+$   text "int __" <> text name <> text "_SlimInterfaceFactory_CreateInterface(IEnv* piEnv, AEECLSID cls, void** ppo) {"
   $+$   text "   return CSlimInterfaceFactory_NewInterface(interfaces, sizeof(interfaces), piEnv, cls, ppo);"
   $+$   text "}"

toCif :: String -> Slim -> Doc
toCif name slm = text "include \"" <> text name <> text "_slim.h\""
             $+$ vcat (map newFunc (moduleIIds slm))
   where
         cf = Pretty.new
         newFunc iid = text "Class { classid = " <> pretty cf iid
                   $+$ text "      , newfunc = __" <> text name <> text "_SlimInterfaceFactory_CreateInterface"
                   $+$ text "      }"
                   $+$ text ""

toH :: String -> Doc
toH name =
         text "#ifndef" <+> text (map toUpper name) <> text "_SLIM_H"
   $+$   text "#define" <+> text (map toUpper name) <> text "_SLIM_H"
   $+$   text "#include \"AEEIEnv.h\""
   $+$   text "int __" <> text name <> text "_SlimInterfaceFactory_CreateInterface(IEnv* piEnv, AEECLSID cls, void** ppo);"
   $+$   text "#endif"


instance Pretty Slim where
   pretty _ slm =
         braces (commas [ toDoc' "interfaces"               (interfaces slm)
                        , toDoc' "methodArrays"             (methodArrays slm)
                        , toDoc' "methods"                  (methods slm)
                        , toDoc' "parameterArrays"          (parameterArrays slm)
                        , toDoc' "parameters"               (parameters slm)
                        , toDoc' "structTypes"              (structTypes slm)
                        , toDoc' "unionTypes"               (unionTypes slm)
                        , toDoc' "sequenceTypes"            (sequenceTypes slm)
                        , toDoc' "typeArrays"               (typeArrays slm)
                        , toDoc' "types"                    (types slm)
                        , toDoc' "value64s"                 (value64s slm)
                        , toDoc' "value32s"                 (value32s slm)
                        , toDoc' "value16s"                 (value16s slm)
                        , toDoc' "value8s"                  (value8s slm)
                        , toDoc' "strings"                  (strings slm)
                        , toDoc' "methodStrings"            (methodStrings slm)
                        , toDoc' "methodStringsArrays"      (methodStringsArrays slm)
                        ])
         where
               toDoc' _ []  = int 0 <> comma <+> int 0
               toDoc' name lst = text name <> comma <+> int (length lst)

instance Pretty Word8 where
   pretty _ ww = CT.showHex' ww ""

instance Pretty Word16 where
   pretty _ ww = CT.showHex' ww ""

instance Pretty Word32 where
   pretty _ ww = CT.showHex' ww ""

instance Pretty Word64 where
   pretty _ ww = CT.showHex' ww ""

instance Pretty Int32 where
   pretty _ ww = CT.showHex' ww ""

instance Pretty Char where
   pretty _ '\0' = text "\\0"
   pretty _ ch   = char ch

instance Pretty Int where
   pretty _ ww = int ww


instance Pretty StringRef where
   pretty cf (StringRef offset) = pretty cf offset

instance Pretty MethodStrings where
   pretty cf (MethodStrings sr pars) = commas ((pretty cf sr):(map (pretty cf) pars))

instance Pretty ParameterStrings where
   pretty cf (ParameterStrings sr tt) = commas [pretty cf sr,pretty cf tt]

instance Pretty TypeStrings where
   pretty _ NoName = empty
   pretty cf (TypeStrings srs tts) = commas ((map (pretty cf) srs) ++ (map (pretty cf) tts))

instance Pretty MethodStringsRef where
   pretty cf (MethodStringsRef offset) = pretty cf offset

instance Pretty MethodStringsArrayRef  where
   pretty cf (MethodStringsArrayRef  offset)  = text "&(methodStringsArrays [" <> pretty cf offset <> text "])"

instance Pretty Interface where
   pretty cf (Interface _ nm mms ni iis mnr) = braces (commas [pretty cf nm
                                                              ,ifnz cf nm mms
                                                              ,pretty cf ni
                                                              ,ifnz cf ni iis
                                                              ,ifnz cf nm mnr
                                                              ,ifnz cf nm ("methodStrings")
                                                              ,ifnz cf nm ("strings")
                                                              ])


ifnz :: (Eq a, Num a, Pretty a1) => Cfg -> a -> a1 -> Doc
ifnz _ 0 _  = int 0
ifnz cf _ vv = pretty cf vv

instance Pretty MethodRefArrayRef where
   pretty cf (MethodRefArrayRef offset) = text "&(methodArrays[" <> pretty cf offset <> text "])"

instance Pretty MethodRef where
   pretty cf (MethodRef offset) = text "&(methods[" <> pretty cf offset <> text "])"

instance Pretty AEEIIDArrayRef where
   pretty cf (AEEIIDArrayRef offset) = text "(const uint32_t*)(&(value32s[" <> pretty cf offset <> text "]))"

instance Pretty Method where
   pretty cf (Method opattr sc (isz,ial) (rsz,rial) numArg numPa pa) =
         braces (commas [fromScalars cf opattr sc
                        ,pretty cf isz
                        ,pretty cf rsz
                        ,int (fromIntegral numArg)
                        ,int (fromIntegral numPa)
                        ,ifnz cf numPa pa
                        ,pretty cf ial
                        ,pretty cf rial
                        ])


instance Pretty ParameterRefArrayRef where
   pretty cf (ParameterRefArrayRef offset) = text "(&(parameterArrays[" <> pretty cf offset <> text "]))"

instance Pretty ParameterRef where
   pretty cf (ParameterRef offset) = text "(&(parameters[" <> pretty cf offset <> text "]))"

instance Pretty Parameter where
   pretty cf (Parameter ty md notnil) = braces (commas (fromType cf ty ++ [pretty cf md, pretty cf notnil]))

instance Pretty Type where
   pretty cf ty = braces (commas (fromType cf ty))

fromType :: Cfg -> Type -> [Doc]
fromType  cf (Type tds sz) = [prettySize cf sz, pretty cf tds, prettyAl cf sz]

prettySize :: Cfg -> SizeNative -> Doc
prettySize cf (SizeNative (s32,a32) (s64,a64))
   | s32 == s64 && a32 == a64 = pretty cf s32
   | otherwise = prettyIfPtr32 cf s32 s64

prettyAl :: Cfg -> SizeNative -> Doc
prettyAl cf (SizeNative (s32,a32) (s64,a64))
   | s32 == s64 && a32 == a64 = pretty cf a32
   | otherwise = prettyIfPtr32 cf a32 a64

prettyIfPtr32 cf s32 s64
   | (s32) == (s64) = pretty cf s32
   | otherwise = text "SLIM_IFPTR32" <> parens (commas [pretty cf s32, pretty cf s64])

void :: Doc
void = text "(const uintptr_t)"

instance Pretty TypeDesc where
   pretty cf (TypeObject iid notnil)      = braces (braces (commas [void <> pretty cf iid,    void <> pretty cf notnil])   ) <> comma <+> int 0x0
   pretty cf (TypeInterface notnil)       = braces (braces (commas [void <> pretty cf notnil, int 0])                      ) <> comma <+> int 0x1
   pretty _  (TypeEnum _ _)                   = braces (braces (commas [int 0,                    int 0])                      ) <> comma <+> int 0x3
   pretty cf (TypeString ml _ _)              = braces (braces (commas [void <> pretty cf ml,     int 0])                      ) <> comma <+> int 0x4
   pretty cf (TypeWString ml _ _)             = braces (braces (commas [void <> pretty cf ml,     int 0])                      ) <> comma <+> int 0x5
   pretty cf (TypeStructure strf _ _)         = braces (braces (commas [void <> pretty cf strf,   int 0])                      ) <> comma <+> int 0x6
   pretty cf (TypeUnion urf _)              = braces (braces (commas [void <> pretty cf urf,    int 0])                      ) <> comma <+> int 0x7
   pretty cf (TypeArray tr ml _ _)            = braces (braces (commas [void <> pretty cf tr,     void <> pretty cf ml])       ) <> comma <+> int 0x8
   pretty cf (TypeSequence tr ml _ _ _)         = braces (braces (commas [void <> pretty cf tr,     void <> pretty cf ml])       ) <> comma <+> int 0x9
   pretty cf (TypeComplexStructure strf _ _)  = braces (braces (commas [void <> pretty cf strf,   int 0])                      ) <> comma <+> int (0x16)
   pretty cf (TypeComplexUnion urf )       = braces (braces (commas [void <> pretty cf urf,    int 0])                      ) <> comma <+> int (0x17)
   pretty cf (TypeComplexArray urf ml _)    = braces (braces (commas [void <> pretty cf urf,    void <> pretty cf ml])       ) <> comma <+> int (0x18)
   pretty cf (TypeComplexSequence seqtyp _ _ _) = braces (braces (commas [void <> pretty cf seqtyp, int 0])                      ) <> comma <+> int (0x19)
   pretty cf (TypeDmahandle ml)           = braces (braces (commas [void <> pretty cf ml,     int 0])                      ) <> comma <+> int (0x20)
   pretty cf _                            = braces (braces (commas [void <> int 0,              void <> int 1])      ) <> comma <+> int 0x2

instance Pretty SequenceTypeRef where
   pretty cf (SequenceTypeRef offset) = text "&(sequenceTypes[" <> pretty cf offset <> text "])"


instance Pretty SequenceType where
   pretty cf (SequenceType tr maxlen (sz,_) (SizeROut (risz,_) (rrsz,_))) = braces (commas [ pretty cf tr
                                                                                , pretty cf maxlen
                                                                                , pretty cf sz
                                                                                , pretty cf risz
                                                                                , pretty cf rrsz
                                                                                ])
instance Pretty TypeRef where
   pretty cf (TypeRef offset) = text "&(types[" <> pretty cf offset <> text "])"

instance Pretty StructTypeRef where
   pretty cf (StructTypeRef offset) = text "&(structTypes[" <> pretty cf offset <> text "])"

instance Pretty UnionTypeRef where
   pretty cf (UnionTypeRef offset) = text "&(unionTypes[" <> pretty cf offset <> text "])"

instance Show a => Pretty (Ref.Ref a) where
   pretty _ (Ref.Ref offset) = int offset
   pretty _ (Ref.Val val)    = error $ "internal error: unexpected value: " ++ (show val)

instance Pretty StructType where
   pretty cf (StructType num tra (isz,ial) (SizeROut (risz, rial) (rrsz,rral))) =
         braces (commas ([ pretty cf num
                         , pretty cf tra
                         , pretty cf isz
                         , pretty cf risz
                         , pretty cf rrsz
                         , pretty cf ial
                         , pretty cf rial
                         , pretty cf rral
                         ]))

instance Pretty TypeRefArrayRef where
   pretty cf (TypeRefArrayRef offset) = text "&(typeArrays[" <> pretty cf offset <> text "])"

instance Pretty UnionType where
   pretty cf (UnionType desc nCases cvo tra (isz,ial) (SizeROut (risz, rial) (rrsz,rral)) (ica, rcai, rcar, nca) bDefCase) =
         braces (commas ([ pretty cf desc
                         , pretty cf nCases
                         , pretty cf cvo
                         , pretty cf tra
                         , pretty cf isz
                         , pretty cf risz
                         , pretty cf rrsz
                         , pretty cf ial
                         , pretty cf rial
                         , pretty cf rral
                         , pretty cf ica
                         , pretty cf rcai
                         , pretty cf rcar
                         , pretty cf nca
                         , pretty cf bDefCase
                         ]))

instance Pretty NativeCaseAlignment where
   pretty cf (s32,s64)
      | s32 == s64 = pretty cf s32
      | otherwise = error "error: internal error: unexpected native case alignemnt"

instance Pretty CaseValueArrayRef where
   pretty cf (CaseValue8ArrayRef  offset) = text "(const uint8*)&(value8s["  <> pretty cf offset <> text "])"
   pretty cf (CaseValue16ArrayRef offset) = text "(const uint8*)&(value16s[" <> pretty cf offset <> text "])"
   pretty cf (CaseValue32ArrayRef offset) = text "(const uint8*)&(value32s[" <> pretty cf offset <> text "])"
   pretty cf (CaseValue64ArrayRef offset) = text "(const uint8*)&(value64s[" <> pretty cf offset <> text "])"


instance Pretty ID.ParameterMode where
   pretty _ (ID.ParameterIn)       = int 0
   pretty _ (ID.ParameterROut)     = int 3
   pretty _ (ID.ParameterInROut)   = int 4

instance Pretty Bool where
   pretty _ True   = int 1
   pretty _ False  = int 0


isDynamic :: Scalars -> Bool
isDynamic (Scalars 0xff _ _ _) = True
isDynamic (Scalars _ 0xff _ _) = True
isDynamic (Scalars _ _ 0x0f _) = True
isDynamic (Scalars _ _ _ 0x0f) = True
isDynamic _ = False

fromScalars :: Cfg -> Maybe (Pos.SourcePos, ID.OpAttr) -> Scalars -> Doc
fromScalars _ optr sc
   | isDynamic sc = text "REMOTE_SCALARS_MAKEX" <> parens (commas [fromOpAttr optr, int 0, int 0xff, int 0xff, int 0x0f, int 0x0f])
fromScalars cf optr (Scalars bi br oi or') = text "REMOTE_SCALARS_MAKEX" <> parens (commas [fromOpAttr optr
                                                                                           ,int 0
                                                                                           ,pretty cf bi
                                                                                           ,pretty cf br
                                                                                           ,pretty cf oi
                                                                                           ,pretty cf or'
                                                                                           ])

fromOpAttr :: Maybe (Pos.SourcePos, ID.OpAttr) -> Doc
fromOpAttr Nothing = int 0
fromOpAttr (Just (_, (ID.Identifier "oneway")))           = int 0x80
fromOpAttr (Just (_, (ID.Identifier "reliable oneway")))  = int 0xA0
fromOpAttr (Just (_, (ID.Identifier "signal")))           = int 0xc0
fromOpAttr (Just (pos, (ID.Identifier op)))               = error $ TP.errorMessage pos $ "unexpected operation attribute: " ++ op

commas :: [Doc] -> Doc
commas ls = hcat $ List.intersperse comma $ filter (not .  isEmpty) ls


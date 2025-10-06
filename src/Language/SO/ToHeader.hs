-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.SO.ToHeader where

import Language.SO.Literal(fromLiteral)
import Control.Monad(liftM)
import Data.Generics                                  ( listify, Data )
import qualified Language.SO.Env                      as Env
import qualified Language.Idl.ToPalette               as TP
import qualified Language.Slim.TypeRefMap             as TD
import qualified Data.Map                             as Map
import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Data.List                            as List
import qualified Data.Omg.Literal                     as L
import qualified Data.Text                            as T
import Data.Char( toUpper, isDigit)
import Prelude hiding ( (<>) )

import Data.Maybe(fromMaybe)
import System.FilePath( takeBaseName)
import Control.Monad.Identity(runIdentity)

import Language.SO.Cfg
import Data.Omg.Prim
import Language.Idl.Data
import Text.PrettyPrint
import Text.ParserCombinators.Parsec.Pos(
     SourcePos
   )
import Debug.Trace(trace)

dbg :: Show a => a -> a
dbg ff = (show ff) `trace` ff

type IsLocal                         = Bool
type IsExtParams                     = Bool
type IsForward                       = Bool
type IsIdlVersionEnabled             = String
data RemotableType = IdlType Type
                   | IdlInterfaceDcl IsExtParams IsLocal IsForward (Maybe InterfaceId) (SourcePos, Maybe ScopedName) [Declaration]
                   deriving (Show, Eq)

internalBlockCode :: Doc -> [Doc] -> Int -> Doc
internalBlockCode nm dd indent = (nm) $+$ (nest indent (text "{")) $+$ (vcat (map (nest (indent+3)) dd)) $+$ (nest indent (text "}"))

isIDLVersion' :: TopLevelDeclaration -> String
isIDLVersion' (TopLevelDeclaration _ (Declaration _ _ _ (Identifier id) (ConstDcl (StringType _) (ConstExpr _ (LiteralExpr (StringLiteral str)))))) = if (id=="IDL_VERSION") then str else ""
isIDLVersion' _ = ""

checkVersionValidity :: [String] -> IsIdlVersionEnabled
checkVersionValidity isidlv =
   let
      ver = if (length isidlv >= 1) then (T.splitOn  (T.pack ".") (T.pack (isidlv!!0)))
            else []
      lst = filter (==False) $ (concat (map (map isDigit) (map (T.unpack) ver)))
      checkOverflow = if (length (filter (\tt -> length tt <=3) (map T.unpack ver)) /= length ver)
                      then 0
                      else 1
   in if ((length lst >= 1) || (length ver > 3) || (checkOverflow==0))
      then error "IDLVERSION format violated. Use \"xxx.xxx.xxx\" format where x is a digit"
      else (if(length ver > 0) then (isidlv!!0) else "")

toHeader ::  Cfg -> [Pos.SourceName] -> Idl -> [(String, Doc)]
toHeader cf files (Idl decs) =
   let
         decSourceName (TopLevelDeclaration fp _) = fp
         (mine,included) = List.partition (\ dd -> (decSourceName dd) `elem` files) decs
         isidlv = filter (/="") (map isIDLVersion' decs)
         isIDLVersion = checkVersionValidity isidlv
         header = (vcat $ map (fromTopComment) mine)
              $+$ text "#include <AEEStdDef.h>"
              $+$ text "#include <remote.h>"
              $+$ text "#include <string.h>"
              $+$ text "#include <stdlib.h>"
              $+$ (vcat $ map toInclude $ List.nub $ map decSourceName included)
              $+$ (if (hasMethod mine) then qidlcDeclare else empty)
              $+$ text Env.source
              $+$ text "#ifdef __cplusplus"
              $+$ text "extern \"C\" {"
              $+$ text "#endif"
              $+$ (if (hasString mine) then cstring1 else empty)
              $+$ (if (hasWString mine) then cwstring1 else empty)
              $+$ (if (hasDmahandle mine) then cdmahandle1 else empty)
              $+$ (vcat $ map (fromTop cf isIDLVersion) mine)
              $+$ text "#ifdef __cplusplus"
              $+$ text "}"
              $+$ text "#endif"
   in    [(pathToHeader $ moduleName cf, header)]

hasAnyString :: Data a => a -> Bool
hasAnyString decs = hasString decs || hasWString decs

hasString :: Data a => a -> Bool
hasString decs =
   let
         is' (StringType {}) = True
         is' _ = False
   in    not $ null $ listify is' decs

hasWString :: Data a => a -> Bool
hasWString decs =
   let
         is' (WideStringType _) = True
         is' _ = False
   in    not $ null $ listify is' decs

hasDmahandle :: Data a => a -> Bool
hasDmahandle decs =
   let
         is' (DmahandleType _) = True
         is' _ = False
   in    not $ null $ listify is' decs

hasMethod :: [TopLevelDeclaration] -> Bool
hasMethod decs =
   let
         is' (OperationDcl {}) = True
         is' _ = False
   in    not $ null $ listify is' decs

toInclude :: FilePath -> Doc
toInclude fp =  case (takeBaseName fp) of
                       "remote" -> text ""
                       "AEEStdDef" -> text ""
                       otherwise ->  text "#include" <+> (doubleQuotes (text (pathToHeader fp)))

pathToHeader :: FilePath -> String
pathToHeader nm = takeBaseName nm ++ ".h"

fromTopComment :: TopLevelDeclaration -> Doc
fromTopComment (TopLevelDeclaration _ (CommentBlock ccs)) = fromPreDoc ccs
fromTopComment _ = empty

fromTop :: Cfg -> IsIdlVersionEnabled -> TopLevelDeclaration -> Doc
fromTop _ isIdlVersion (TopLevelDeclaration _ (CommentBlock _)) = empty
fromTop cf isIdlVersion (TopLevelDeclaration _ dec) = fromDec cf isIdlVersion dec

fromDec :: Cfg -> IsIdlVersionEnabled -> Declaration -> Doc
fromDec _f isIdlVersion (CommentBlock ccs)                         = fromPreDoc ccs
fromDec cf isIdlVersion (Declaration _ pre post nm df)             = fromPreDoc pre
                                                                     $+$ fromDef cf isIdlVersion nm df
                                                                     $+$ fromPostDoc post

fromDef :: Cfg -> IsIdlVersionEnabled -> Identifier -> Definition -> Doc
fromDef cf isIdlVersion nm (TypeDcl tp)                    = fromType cf nm tp
fromDef cf isIdlVersion nm iface@(InterfaceDcl _ _ _ _ parent decs)
   | (null $ getRemoteHandleMethods cf iface) = vcat (map (fromDec (withScope cf nm) isIdlVersion) (concatMap (getDerivedMethods cf) $ lookupIface cf parent))
                                            $+$ vcat (map (fromDec (withScope cf nm) isIdlVersion) decs)
fromDef cf isIdlVersion nm (InterfaceDcl iep _ _ _ parent decs) = vcat (map (fromDec (withScope cf nm) isIdlVersion) (concatMap (getRemoteHandleMethods cf) $ lookupIface cf parent))
                                             $+$ vcat (map (fromDec (setRH $ withScope cf nm) isIdlVersion) (concatMap (getNonRemoteHandleMethods cf) $ lookupIface cf parent))
                                             $+$ vcat (map (fromDec (setRH $ withScope cf nm) isIdlVersion) decs)
                                             $+$ uri cf (render $ scopedName cf nm) iep isIdlVersion
fromDef cf isIdlVersion nm (OperationDcl False _ _ rv pars)
   | (isRemoteHandle cf)                  = funcDeclare "__QAIC_HEADER" (typeName cf rv) (scopedName cf nm) ((text "remote_handle64 _h"):(fromParams cf pars)) <> semi
fromDef cf isIdlVersion nm (OperationDcl True _ _ rv pars)
   | (isRemoteHandle cf)                  = funcDeclare "__QAIC_HEADER" (typeName cf rv) (scopedName cf nm) ((text "remote_handle64 _h"): (text "fastrpc_async_descriptor_t* asyncDesc") : (fromParams cf pars)) <> semi
fromDef cf isIdlVersion nm (OperationDcl False _ _ rv pars)    = funcDeclare "__QAIC_HEADER" (typeName cf rv) (scopedName cf nm) (fromParams cf pars) <> semi
fromDef cf isIdlVersion nm (OperationDcl True _ _ rv pars)    =  funcDeclare "__QAIC_HEADER" (typeName cf rv) (scopedName cf nm) ((text "fastrpc_async_descriptor_t* asyncDesc") : (fromParams cf pars)) <> semi
fromDef cf isIdlVersion nm (ConstDcl ty expr)          = text "#define" <+> scopedName cf nm <+> fromConstExpr cf expr (IdlType ty)
fromDef cf isIdlVersion nm (ModuleDcl decs)            = vcat (map (fromDec (withScope cf nm) isIdlVersion) decs)

defenv :: String
defenv = concat $
	[ "#ifdef __ASYNC_STUB\n"]

endif :: String
endif = concat $
	[ "#endif\n"]


funcDeclare :: String -> Doc -> Doc -> [Doc] -> Doc
funcDeclare "" rv name args   = rv <+> name <> parens (argTypeList args)
funcDeclare dclr rv name args = text dclr <> text "_EXPORT" <+> rv <+> text dclr <> (parens name) <> parens (argTypeList args) <+> text dclr <> text "_ATTRIBUTE"

setRH :: Cfg -> Cfg
setRH cf = cf { isRemoteHandle = True }

interface_uri :: Cfg -> String -> IsExtParams -> IsIdlVersionEnabled -> String
interface_uri cf nm iep isIdlVersion = uri_string
   where
      createUriOnScalar = if (iep==False)
                          then "\"file:///SKELNAME?INTERFACE_skel_handle_invoke&_modver=1.0"
                          else "\"file:///SKELNAME?INTERFACE_skel_handle_invoke_s64&_modver=1.0"
      uri_string = if ((length isIdlVersion)/=0)
                 then (createUriOnScalar++"&_idlver="++isIdlVersion++"\"")
                 else (createUriOnScalar++"\"")

uri :: Cfg -> String -> IsExtParams -> IsIdlVersionEnabled -> Doc
uri cf nm iep isIdlVersion = vcat $ map (text . replace') txt
   where
      replace' = (replace "SKELNAME" (skelName cf)) . (replace "INTERFACE" nm)
      createUriOnScalar = interface_uri cf nm iep isIdlVersion
      txt =[ "#ifndef INTERFACE_URI"
            , "#define INTERFACE_URI "++createUriOnScalar
            , "#endif /*INTERFACE_URI*/"
            ]


argTypeList :: [Doc] -> Doc
argTypeList [] = text "void"
argTypeList args = commals args

getRemoteHandleMethods :: Cfg -> Definition -> [Declaration]
getRemoteHandleMethods cf def
   | isRemoteHandleIface def = getMethods cf def
getRemoteHandleMethods cf (InterfaceDcl _ _ _ _ iface _) = concatMap (getRemoteHandleMethods cf) (lookupIface cf iface)
getRemoteHandleMethods _ _ = []

isRemoteHandleIface :: Definition -> Bool
isRemoteHandleIface (InterfaceDcl _ False False (Just (ConstExpr _ (ConstExprRef ["AEEIID_remote_handle64"]))) _ _) = True
isRemoteHandleIface _ = False

getMethods :: Cfg -> Definition -> [Declaration]
getMethods _ (InterfaceDcl _ _ _ _ _ decs) = filter (isOperationDcl . unDec) decs
getMethods _ _ = []

getNonRemoteHandleMethods :: Cfg -> Definition -> [Declaration]
getNonRemoteHandleMethods _ def
   | isRemoteHandleIface def =  []
getNonRemoteHandleMethods cf ii@(InterfaceDcl _ _ _ _ iface _) =
   let methods = getMethods cf ii
       derived = concatMap (getNonRemoteHandleMethods cf) (lookupIface cf iface)
   in  derived ++ methods
getNonRemoteHandleMethods _ _ = []

getDerivedMethods :: Cfg -> Definition -> [Declaration]
getDerivedMethods cf def = (getRemoteHandleMethods cf def) ++ (getNonRemoteHandleMethods cf def)

fromParams :: Cfg -> [Parameter] -> [Doc]
fromParams cf pps = map (fromParam cf) pps

fromParam :: Cfg -> Parameter -> Doc
fromParam cf (Parameter name ParameterIn ss)
   | isString cf (IdlType ss)              = text "const" <+> (head $ seqFields cf name (IdlType ss))
fromParam cf (Parameter name ParameterIn ss)
   | isSeq cf (IdlType ss)                 = text "const" <+> commals (take 2 $ seqFields cf name (IdlType ss))
fromParam cf (Parameter name _ ss)
   | isdmahandle cf (IdlType ss)           = text "int" <+> fromID name <> text ", uint32" <+> fromID name <> text "Offset, uint32" <+> fromID name <> text "Len"
fromParam cf (Parameter name _ ss)
   | isSeq cf (IdlType ss)                 = commals (seqParams cf name (IdlType ss))
fromParam cf (Parameter name md tp)        = modeType cf md tp <+> fromID name



modeType :: Cfg -> ParameterMode -> Type -> Doc
modeType cf ParameterIn     tp
   | isStructOrUnion cf (IdlType tp)  = text "const" <+> typeName cf tp <> text "*"
modeType cf ParameterIn     tp
   | isArray cf (IdlType tp)          = text "const" <+> typeName cf tp
modeType cf ParameterIn     tp        = typeName cf tp
modeType cf _               tp
   | isArray cf (IdlType tp)          = typeName cf tp
modeType cf ParameterROut   tp        = typeName cf tp <> text "*"
modeType cf ParameterInROut   tp      = typeName cf tp <> text "*"

parmNames :: Cfg -> Parameter -> [Doc]
parmNames cf (Parameter name ParameterIn ss)
   | isString cf (IdlType ss)                = [ fromID name ]
parmNames cf (Parameter name ParameterIn ss)
   | isSeq cf (IdlType ss)                   = [ fromID name , fromID name <> text "Len" ]
parmNames cf (Parameter name _ ss)
   | isdmahandle cf (IdlType ss)               = [ fromID name , fromID name <> text "Offset", fromID name <> text "Len"]
parmNames cf (Parameter name _ ss)
   | isSeq cf (IdlType ss)                  = [ fromID name , fromID name <> text "Len" ]
parmNames _ (Parameter name _ _)            = [ fromID name ]

parmInROutNames :: Cfg -> Parameter -> [Doc]
parmInROutNames cf (Parameter name ParameterInROut ss) =
      (parmNames cf (Parameter name ParameterIn ss))
   ++ (parmNames cf (Parameter name ParameterROut ss))
parmInROutNames cf pp = parmNames cf pp

isArray :: Cfg -> RemotableType -> Bool
isArray cf (IdlType tp) = isArray' cf tp
isArray _ _ = False

isArray' :: Cfg -> Type -> Bool
isArray' cf (TypeRef sp _ sn)  = fromMaybe False $ liftM (isArray cf) $ getType $ snd (lookupTypeRef cf (sp,sn))
isArray' _ (Array {})         = True
isArray' _ _                  = False

isStructOrUnion :: Cfg -> RemotableType -> Bool
isStructOrUnion cf (IdlType tp) = isStructOrUnion' cf tp
isStructOrUnion _ _ = False

isStructOrUnion' :: Cfg -> Type -> Bool
isStructOrUnion' cf (TypeRef sp _ sn)   = fromMaybe False $ liftM (isStructOrUnion cf) $ getType $ snd (lookupTypeRef cf (sp,sn))
isStructOrUnion' _ (Struct {})         = True
isStructOrUnion' _ (Union {})          = True
isStructOrUnion' _ _                   = False

fromConstExpr :: Cfg -> ConstExpr -> RemotableType -> Doc
fromConstExpr cf expr (IdlType tp) = fromConstExpr' cf expr tp
fromConstExpr _ _ sh = error $ ":invalid const expression type: " ++ (show sh)

fromConstExpr' :: Cfg -> ConstExpr -> Type -> Doc
fromConstExpr' cf expr (TypeRef sp _ sn)   = fromConstExpr cf expr $ lookupType cf ":constExpr:" sp sn
fromConstExpr' cf expr ty                  = fromLiteral $ constExprToLit cf [] ty expr

getType :: Declaration -> Maybe RemotableType
getType (Declaration _ _ _ _ (TypeDcl tr)) = Just (IdlType tr)
getType (Declaration _ _ _ _ (InterfaceDcl iep isl isf miid sp decs)) = Just $ IdlInterfaceDcl iep isl isf miid sp decs
getType _ = Nothing

lookupType :: Cfg -> String -> Pos.SourcePos -> ScopedName -> RemotableType
lookupType cf tag  sp sn = fromMaybe (error ("error:" ++ (show sp) ++ tag ++ ":internal error, expected type declaration")) $ getType $ snd (lookupTypeRef cf (sp,sn))


fromType :: Cfg -> Identifier -> Type -> Doc
fromType cf nm (PrimType pp)               = text "typedef" <+> primType pp <+> scopedName cf nm <> semi
fromType cf nm (Struct _ mems)             = text "typedef struct" <+> scopedName cf nm <+> scopedName cf nm <> semi
                                         $+$ fromStruct cf (scopedName cf nm) mems <> semi
fromType cf nm (Union _ _ del cases dc)    = text "typedef struct" <+> scopedName cf nm <+> scopedName cf nm <> semi
                                         $+$ fromUnion cf (scopedName cf nm) del cases dc <> semi
fromType cf nm (Enum _ labels)             = fromEnumType cf nm labels <> semi
                                         $+$ text "typedef enum" <+> scopedName cf nm <+> scopedName cf nm <> semi
fromType cf nm st@(Sequence  _ ty)          = text "typedef struct" <+> seqName cf nm ty <+> seqName cf nm ty <> semi
                                         $+$ text "typedef" <+> seqName cf nm ty <+> scopedName cf nm <> semi
                                         $+$ text "struct" <+> seqName cf nm ty <+> text "{"
                                         $+$ indent cf (semis (seqFields cf (Identifier "data") (IdlType st)))
                                         $+$ text "};"
fromType cf nm (Array ln tp)               = text "typedef" <+> typeName cf tp <+> scopedName cf nm <> brackets (int (constExprToInt32 cf ln)) <> semi
fromType cf nm (Native nat)                = text "typedef" <+> scopedName cf nat <+> scopedName cf nm <> semi
fromType cf nm (Interface)                 = text "typedef interface"  <+> scopedName cf nm <> semi
fromType cf nm tr                          = text "typedef" <+> typeName cf tr <+> scopedName cf nm <> semi


fromEnumType :: Cfg -> Identifier -> [([Comment], String)] -> Doc
fromEnumType cf name labels = text "enum" <+> scopedName cf name <+> text "{"
                          $+$ indent cf (vcat (map fromLabel labels))
                          $+$ indent cf (text "_32BIT_PLACEHOLDER_" <> scopedName cf name <+> text "= 0x7fffffff")
                          $+$ text "}"


fromLabel :: ([Comment], String) -> Doc
fromLabel (pd, ss) = text ss <> comma <+> fromPostDoc pd

fromUnion :: Cfg -> Doc -> Type -> [UnionCase] -> Maybe Member -> Doc
fromUnion cf name del mems dc = text "struct" <+> name <+> text "{"
                            $+$ indent cf (typeName cf del <+> text "_d" <> semi)
                            $+$ indent cf ( text "union" <+> text "{"
                                        $+$ indent cf (vcat $ map (fromCase cf) mems)
                                        $+$ indent cf (fromDefaultCase cf dc)
                                        $+$ text "} _u;"
                                          )
                            $+$ text "}"

fromCase :: Cfg -> UnionCase -> Doc
fromCase cf (UnionCase _ mm) = fromMember cf mm

fromDefaultCase :: Cfg -> Maybe Member -> Doc
fromDefaultCase cf (Just mm) = fromMember cf mm
fromDefaultCase _f Nothing   = empty

fromStruct :: Cfg -> Doc -> [Member] -> Doc
fromStruct cf name mems = text "struct" <+> name <+> text "{"
                      $+$ indent cf (vcat (map (fromStructMember cf) mems))
                      $+$ text "}"

fromStructMember :: Cfg -> Member -> Doc
fromStructMember cf (Member _ pd nm ss)
   | isSeq cf (IdlType ss)    = let ty:rest = (seqFields cf nm $ IdlType ss)
                                in  (vcat ((ty <> semi <+> fromPostDoc pd):(map (\ vv -> vv <> semi) rest)))
fromStructMember cf mm = fromMember cf mm

fromMember :: Cfg -> Member -> Doc
fromMember cf (Member _ pd nm (Array ln tp)) =
   typeName cf tp <+> fromID nm <> brackets (int (constExprToInt32 cf ln)) <> semi <+> fromPostDoc pd
fromMember cf (Member _ pd nm tp) =
   typeName cf tp <+> fromID nm <> semi <+> fromPostDoc pd

typeName :: Cfg -> Type -> Doc
typeName _ (PrimType pp)                     = primType pp
typeName cf (TypeRef sp _ sn)                = fromScopedName $ reverse $ fst $ lookupType
                                                           where lookupType = lookupTypeRef cf (sp,sn)
typeName _ (StringType _)                    = text "_cstring1_t"
typeName _ (WideStringType _)                = text "_wstring1_t"
typeName _ (DmahandleType _)                 = text "_dmahandle1_t"
typeName _ vv                                = error $ "internal error: unexpected type " ++ (show vv)

seqName :: Cfg -> Identifier -> Type -> Doc
seqName cf nm (PrimType pp)  = text "_" <> scopedName cf nm <> text "__seq_" <> primName pp
seqName cf nm ty             = text "_" <> scopedName cf nm <> text "__seq_" <> typeName cf ty

seqParams :: Cfg -> Identifier -> RemotableType -> [Doc]
seqParams cf name ty = (seqFields cf name ty)

sequenceParams :: Cfg -> Identifier -> RemotableType -> [Doc]
sequenceParams cf name ty = (sequenceFields cf name ty)

seqFields :: Cfg -> Identifier -> RemotableType -> [Doc]
seqFields _ _ (IdlInterfaceDcl {}) = error "internal error: expected a sequence string or widestring"
seqFields cf name (IdlType tp) = seqFields' cf name tp

sequenceFields :: Cfg -> Identifier -> RemotableType -> [Doc]
sequenceFields _ _ (IdlInterfaceDcl {}) = error "internal error: expected a sequence string or widestring"
sequenceFields cf name (IdlType tp) = sequenceFields' cf name tp

seqFields' :: Cfg -> Identifier -> Type -> [Doc]
seqFields' cf name (Sequence  _ ty)     =  [ typeName cf ty <> text "*" <+> fromID name
                                         , text "int" <+> fromID name <> text "Len"]
seqFields' cf name (StringType ml)     = seqFields' cf name (Sequence  ml (PrimType CharType))
seqFields' cf name (WideStringType ml) = seqFields' cf name (Sequence  ml (PrimType WideCharType ))
seqFields' cf name (TypeRef sp  _ sn)  = seqFields cf name (lookupType cf "seqFields:" sp sn)
seqFields' _ _ _ = error "internal error: expected a sequence string or widestring"

sequenceFields' :: Cfg -> Identifier -> Type -> [Doc]
sequenceFields' cf name  (Sequence _ ty)     = [ typeName cf ty <> text "*"
                                         , text "int"]
sequenceFields' cf name  (StringType ml)     = sequenceFields' cf name (Sequence  ml (PrimType CharType ))
sequenceFields' cf name (WideStringType ml) = sequenceFields' cf name (Sequence   ml (PrimType WideCharType ))
sequenceFields' cf name  (TypeRef sp  _ sn)  = sequenceFields cf name (lookupType cf "seqFields:" sp sn)
sequenceFields' _ _ _ = error "internal error: expected a sequence string or widestring"

isString :: Cfg -> RemotableType -> Bool
isString _ (IdlInterfaceDcl {}) = False
isString cf (IdlType tp) = isString' cf tp

isString' :: Cfg -> Type -> Bool
isString' _ (StringType _)      = True
isString' _ (WideStringType _)  = True
isString' cf (TypeRef sp _ sn)   = isString cf (lookupType cf ":isString:" sp sn)
isString' _ _                    = False

isSeq :: Cfg -> RemotableType -> Bool
isSeq _ (IdlInterfaceDcl {}) = False
isSeq cf (IdlType tp) = isSeq' cf tp

isSeq' :: Cfg -> Type -> Bool
isSeq' _ (Sequence  _ _)      = True
isSeq' _ (StringType _)      = True
isSeq' _ (WideStringType _)  = True
isSeq' cf (TypeRef sp _ sn)   = isSeq cf (lookupType cf ":isSeq:" sp sn)
isSeq' _ _                    = False

isdmahandle :: Cfg -> RemotableType -> Bool
isdmahandle _ (IdlInterfaceDcl {}) = False
isdmahandle cf (IdlType tp) = isdmahandle' cf tp

isdmahandle' :: Cfg -> Type -> Bool
isdmahandle' _ (DmahandleType _)   = True
isdmahandle' cf (TypeRef sp _ sn)  = isdmahandle cf (lookupType cf ":isdmahandle:" sp sn)
isdmahandle' _ _                   = False

fromScopedName :: [String] -> Doc
fromScopedName sn = hcat (map text $ List.intersperse "_" (reverse sn))

fromPreDoc :: [Comment] -> Doc
fromPreDoc  = vcat . map fromPreDoc'

fromPostDoc :: [Comment] -> Doc
fromPostDoc = vcat . map fromPostDoc'

fromPreDoc' :: Comment -> Doc
fromPreDoc' = fromPostDoc'

fromPostDoc' :: Comment -> Doc
fromPostDoc' (Comment False ss) = text "//" <> text ss
fromPostDoc' (Comment True ss)  = text "/*" <> text ss <> text "*/"

fromID :: Identifier -> Doc
fromID (Identifier nm) = text nm

primType :: Prim -> Doc
primType SignedShortType      = text "short"
primType SignedLongType       = text "int"
primType SignedLongLongType   = text "int64"
primType UnsignedShortType    = text "unsigned short"
primType UnsignedLongType     = text "unsigned int"
primType UnsignedLongLongType = text "uint64"
primType SignedCharFixedTType      = text "int8_t"
primType SignedShortFixedTType     = text "int16_t"
primType SignedLongFixedTType      = text "int32_t"
primType SignedLongLongFixedTType  = text "int64_t"
primType UnsignedCharFixedTType    = text "uint8_t"
primType UnsignedShortFixedTType   = text "uint16_t"
primType UnsignedLongFixedTType    = text "uint32_t"
primType UnsignedLongLongFixedTType= text "uint64_t"
primType SignedCharFixedType      = text "int8"
primType SignedShortFixedType     = text "int16"
primType SignedLongFixedType      = text "int32"
primType SignedLongLongFixedType  = text "int64"
primType UnsignedCharFixedType    = text "uint8"
primType UnsignedShortFixedType   = text "uint16"
primType UnsignedLongFixedType    = text "uint32"
primType UnsignedLongLongFixedType= text "uint64"
primType BooleanType          = text "boolean"
primType FloatType            = text "float"
primType DoubleType           = text "double"
primType OctetType            = text "unsigned char"
primType CharType             = text "char"
primType WideCharType         = text "_wchar_t"
primType (EnumType _)         = error "internal error: primType: unexpected enum"

primName:: Prim -> Doc
primName UnsignedShortType    = text "unsignedShort"
primName UnsignedLongType     = text "unsignedLong"
primName UnsignedLongLongType = text "unsignedLongLong"
primName SignedShortType      = text "short"
primName SignedLongType       = text "long"
primName SignedLongLongType   = text "longLong"
primName UnsignedCharFixedTType    = text "uint8_t"
primName UnsignedShortFixedTType   = text "uint16_t"
primName UnsignedLongFixedTType    = text "uint32_t"
primName UnsignedLongLongFixedTType= text "uint64_t"
primName SignedCharFixedTType      = text "int8_t"
primName SignedShortFixedTType     = text "int16_t"
primName SignedLongFixedTType      = text "int32_t"
primName SignedLongLongFixedTType  = text "int64_t"
primName UnsignedCharFixedType    = text "uint8"
primName UnsignedShortFixedType   = text "uint16"
primName UnsignedLongFixedType    = text "uint32"
primName UnsignedLongLongFixedType= text "uint64"
primName SignedCharFixedType      = text "int8"
primName SignedShortFixedType     = text "int16"
primName SignedLongFixedType      = text "int32"
primName SignedLongLongFixedType  = text "int64"
primName BooleanType          = text "boolean"
primName FloatType            = text "float"
primName DoubleType           = text "double"
primName OctetType            = text "octet"
primName CharType             = text "char"
primName WideCharType         = text "wideChar"
primName (EnumType _)         = error "internal error: primName: unexpected enum"

indent :: Cfg -> Doc -> Doc
indent cfg = nest (indentSize cfg)

scopedName :: Cfg -> Identifier -> Doc
scopedName cf nm = fromScopedName (scope $ withScope cf nm)

withScope :: Cfg -> Identifier -> Cfg
withScope cfg (Identifier ss) = cfg { scope = ss : (scope cfg) }

isOperationDcl :: Definition -> Bool
isOperationDcl (OperationDcl _ _ _ _ _) = True
isOperationDcl _                    = False

isInterfaceDcl :: Definition -> Bool
isInterfaceDcl (InterfaceDcl {}) = True
isInterfaceDcl _ = False

lookupIface :: Cfg -> (Pos.SourcePos, Maybe ScopedName) -> [Definition]
lookupIface _  (_,Nothing) = []
lookupIface cfg (p,Just n) = [lookupDef cfg (p,n)]

lookupDef :: Cfg -> (Pos.SourcePos, ScopedName) -> Definition
lookupDef cf ref = unDec $ snd $ lookupTypeRef cf ref

unDec :: Declaration -> Definition
unDec (Declaration _ _ _ _ def) = def
unDec (CommentBlock _) = (DummyDef "Comment Block")
unDec _ = error "internal error: unDec: expected declaration"

lookupTypeRef :: Cfg -> (Pos.SourcePos, ScopedName) -> (ScopedName, Declaration)
lookupTypeRef cfg (sp,sn) =
   let   from = fromMaybe (error $ "internal error: unresolved type ref" ++ (show (sp,sn)))
   in    from $ Map.lookup (sp,sn) $ (typeRefMap cfg)

constExprToLit :: Cfg -> ScopedName -> Type -> ConstExpr -> L.Literal
constExprToLit cfg scn ty expr =
      let (types,consts) = (paletteDics cfg)
      in runIdentity $ do
               ty' <- TP.inlineType (types,consts) scn ty
               TP.constExpr consts scn ty' expr

constExprToInt32 :: Cfg -> ConstExpr -> Int
constExprToInt32 cfg ml = TD.literalToIntegral $ constExprToLit cfg [] (PrimType SignedLongType ) ml

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace key val source
   | key == (take (length key) source) = val ++ replace key val (drop (length key) source)
replace key val source = (head source) : replace key val (tail source)

defScopedName :: Cfg -> Identifier -> ScopedName
defScopedName cf (Identifier ss) = ss : (scope cf)

commals :: [Doc] -> Doc
commals ll = hcat (List.intersperse (comma <> space) ll)

semis :: [Doc] -> Doc
semis ll = vcat (map (\ tt -> tt <> semi) (filter (not . isEmpty) ll))

cstring1 :: Doc
cstring1 =vcat $ map text $ [
    "#if !defined(__QAIC_STRING1_OBJECT_DEFINED__) && !defined(__STRING1_OBJECT__)"
   ,"#define __QAIC_STRING1_OBJECT_DEFINED__"
   ,"#define __STRING1_OBJECT__"
   ,"typedef struct _cstring1_s {"
   ,"   char* data;"
   ,"   int dataLen;"
   ,"} _cstring1_t;"
   ,""
   ,"#endif /* __QAIC_STRING1_OBJECT_DEFINED__ */"]

cwstring1 :: Doc
cwstring1 =vcat $ map text $ [
    "#if !defined(__QAIC_WSTRING1_OBJECT_DEFINED__) && !defined(__WSTRING1_OBJECT__)"
   ,"#define __QAIC_WSTRING1_OBJECT_DEFINED__"
   ,"#define __WSTRING1_OBJECT__"
   ,"typedef struct _wstring1_s {"
   ,"   _wchar_t* data;"
   ,"   int dataLen;"
   ,"} _wstring1_t;"
   ,"#endif /* __QAIC_WSTRING1_OBJECT_DEFINED__ */"]

cdmahandle1 :: Doc
cdmahandle1 =vcat $ map text $ [
    "#if !defined(__QAIC_DMAHANDLE1_OBJECT_DEFINED__) && !defined(__DMAHANDLE1_OBJECT__)"
   ,"#define __QAIC_DMAHANDLE1_OBJECT_DEFINED__"
   ,"#define __DMAHANDLE1_OBJECT__"
   ,"typedef struct _dmahandle1_s {"
   ,"   int fd;"
   ,"   uint32 offset;"
   ,"   uint32 len;"
   ,"} _dmahandle1_t;"
   ,"#endif /* __QAIC_DMAHANDLE1_OBJECT_DEFINED__ */"]

qidlcDeclare :: Doc
qidlcDeclare = vcat $ map text $ [
      "#ifndef __QAIC_HEADER"
    , "#define __QAIC_HEADER(ff) ff"
    , "#endif //__QAIC_HEADER"
    , ""
    , "#ifndef __QAIC_HEADER_EXPORT"
    , "#define __QAIC_HEADER_EXPORT"
    , "#endif // __QAIC_HEADER_EXPORT"
    , ""
    , "#ifndef __QAIC_HEADER_ATTRIBUTE"
    , "#define __QAIC_HEADER_ATTRIBUTE"
    , "#endif // __QAIC_HEADER_ATTRIBUTE"
    , ""
    , "#ifndef __QAIC_IMPL"
    , "#define __QAIC_IMPL(ff) ff"
    , "#endif //__QAIC_IMPL"
    , ""
    , "#ifndef __QAIC_IMPL_EXPORT"
    , "#define __QAIC_IMPL_EXPORT"
    , "#endif // __QAIC_IMPL_EXPORT"
    , ""
    , "#ifndef __QAIC_IMPL_ATTRIBUTE"
    , "#define __QAIC_IMPL_ATTRIBUTE"
    , "#endif // __QAIC_IMPL_ATTRIBUTE"]

ifdef :: (String, Doc) -> (String, Doc)
ifdef (path, txt) =
    let   name = takeBaseName path
          valid = ['A'..'Z'] ++ ['0'..'9']

          validate cc
            | cc `elem` valid = cc
            | otherwise = '_'

          ifdefname = text "_" <> text (map (validate . toUpper) name) <> text "_H"
          ifdefstart = text "#ifndef" <+> ifdefname
                   $+$ text "#define" <+> ifdefname
          ifdefend = text "#endif //" <> ifdefname
    in   (path, ifdefstart $+$ txt $+$ ifdefend)



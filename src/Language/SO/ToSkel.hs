-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.SO.ToSkel where

import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Language.SO.Skel                     as Skel
import qualified Language.Slim.Doc                    as Slim
import qualified Language.Slim.Data                   as Slim
import qualified Language.SO.Env                      as Env
import Prelude hiding ( (<>) )
import Language.SO.ToHeader(scopedName, typeName, withScope, fromParam, toInclude, indent, replace, parmNames, defScopedName, getDerivedMethods, commals, lookupIface)

import Language.SO.Cfg
import Language.Idl.Data
import Text.PrettyPrint


toSkel ::  Cfg -> [Pos.SourceName] -> Idl -> [(String, Doc)]
toSkel cf files (Idl decs) =
   let
         decSourceName (TopLevelDeclaration fp _) = fp
         mine = filter (\ dd -> (decSourceName dd) `elem` files) decs
   in    concatMap (fromTop cf) mine

fromTop :: Cfg -> TopLevelDeclaration -> [(String, Doc)]
fromTop cf (TopLevelDeclaration _ dec) = fromDec cf dec

fromDec :: Cfg -> Declaration -> [(String, Doc)]
fromDec cf dec@(Declaration _ _ _ nm df) =
   fromInterface cf nm (toSlim cf ((defScopedName cf nm), dec)) df
fromDec _  _                         = []

fromInterface :: Cfg -> Identifier -> Slim.Slim -> Definition -> [(String, Doc)]
fromInterface cf nm sl (InterfaceDcl _ _ _ _ iface mydecs) =
   let
         name = render $ scopedName cf nm
         slimFile = name ++ "_slim.h"
         slimDoc = (Slim.toC sl)
         interfaceFile = name ++ "_skel.c"
         alldecs = (concatMap (getDerivedMethods cf) (lookupIface cf iface)) ++ mydecs
         pslim = text $ "&" ++ name
         docs = (toInclude $ moduleName cf)
            $+$ text "#include \"remote.h\""
            $+$ text "#include" <+> doubleQuotes (text slimFile)
            $+$ text Env.source
            $+$ (skel name)
            $+$ vcat (map (onDec $ declareFunc (withScope cf nm)) alldecs)
            $+$ text "SO_OBJECT_START(" <> (scopedName cf nm) <> text ")"
            $+$ indent cf (vcat (map (onDec $ funcRefs (withScope cf nm)) alldecs))
            $+$ text "SO_OBJECT_END" <> (parens (commals [scopedName cf nm, pslim]))
   in    [(slimFile, slimDoc)
         ,(interfaceFile, docs)]

fromInterface cf nm _ (ModuleDcl decs)       = concatMap (fromDec (withScope cf nm)) decs
fromInterface _  _  _ _                      = []

onDec :: (Identifier -> Definition -> Doc) -> Declaration -> Doc
onDec ff (Declaration _ _ _ nm df) = ff nm df
onDec _ _ = empty

declareFunc :: Cfg -> Identifier -> Definition -> Doc
declareFunc cf nm (OperationDcl _ _ _ rv pars)  =
      text "static"
  <+> typeName cf rv
  <+> objFunc cf nm
   <> parens (commals $ (text "void* _pif") : (map (fromParam cf) pars))
  <+> text "{"
  $+$ (indent cf) (callFunc cf nm pars)
  $+$ text "}"

declareFunc _ _ _ = empty

objFunc :: Cfg -> Identifier -> Doc
objFunc cf nm =  text "_skel_" <> scopedName cf nm

funcRefs :: Cfg -> Identifier -> Definition -> Doc
funcRefs cf nm (OperationDcl _ _ _ _ _)  = comma <> text "(Function)" <> objFunc cf nm
funcRefs _ _ _ = empty

callFunc :: Cfg -> Identifier -> [Parameter] -> Doc
callFunc cf nm pars  =
      text "return"
  <+> text "SO_SKEL_FUNC" <> parens (scopedName cf nm)
   <> parens (commals $ concatMap (parmNames cf) pars) <> semi


skel :: String -> Doc
skel iface = text $ replace "INTERFACE" iface $ Skel.source

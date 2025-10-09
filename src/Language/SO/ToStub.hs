-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.SO.ToStub where

import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Data.List                            as List
import qualified Language.SO.Stub                     as Stub
import qualified Language.Slim.Doc                    as Slim
import qualified Language.Slim.Data                   as Slim
import qualified Language.SO.Env                      as Env
import Prelude hiding ( (<>) )
import Language.SO.ToHeader(scopedName, typeName, withScope, toInclude, indent, fromScopedName, replace, parmNames, defScopedName, getDerivedMethods, commals, fromParams, lookupIface)

import Language.SO.Cfg
import Language.Idl.Data
import Text.PrettyPrint

toStub ::  Cfg -> [Pos.SourceName] -> Idl -> [(String, Doc)]
toStub cf files (Idl decs) =
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
         name = (render $ scopedName cf nm)
         slimFile = name ++ "_slim.h"
         slimDoc = (Slim.toC sl)
         interfaceFile = name ++ "_stub.c"
         alldecs :: [Declaration]
         alldecs = (concatMap (getDerivedMethods cf) (lookupIface cf iface)) ++ mydecs
         docs = (toInclude $ moduleName cf)
            $+$ text "#include \"slim.h\""
            $+$ text "#include \"remote.h\""
            $+$ text "#include \"shared.h\""
            $+$ text "#include" <+> doubleQuotes (text slimFile)
            $+$ text Env.source
            $+$ (stub name)
            $+$ vcat (zipWith (declareFunc (withScope cf nm)) [0..] alldecs)

   in    [(slimFile, slimDoc)
         ,(interfaceFile, docs)]

fromInterface cf nm _ (ModuleDcl decs)       = concatMap (fromDec (withScope cf nm)) decs
fromInterface _  _  _ _                      = []

declareFunc :: Cfg -> Int -> Declaration -> Doc
declareFunc cf ii (Declaration _ _ _ nm df) = declareFunc' cf nm ii df
declareFunc _  _  _                         = empty

declareFunc' :: Cfg -> Identifier -> Int -> Definition -> Doc
declareFunc' cf nm nn (OperationDcl _ _ _ rv pars) = typeName cf rv
                                            <+> scopedName cf nm
                                             <> parens (commals (fromParams cf pars))
                                            <+> text "{"
                                            $+$ indent cf (implement cf nn pars)
                                            $+$ text "}"

declareFunc' _ _ _ _ = empty

stub :: String -> Doc
stub iface = text $ replace "INTERFACE" iface $ Stub.source

implement :: Cfg -> Int -> [Parameter] -> Doc
implement cf nn pars = text "return slim_stub_method_invoke" <> (parlist (env:iro:iface:(int nn):(concatMap (parmNames cf) pars))) <> semi
   where
      iname = (fromScopedName $ scope cf)
      env = text "stdlib_IRealloc()"
      iro = iname <> text "_IRemoteObject()"
      parlist ll =  parens $ hcat (List.intersperse (comma <> space) ll)
      iface = text "&" <> iname


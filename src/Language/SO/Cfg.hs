-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.SO.Cfg where

import qualified Language.Idl.Data                    as ID
import qualified Language.Idl.ToPalette               as TP
import qualified Language.Slim.TypeRefMap             as TD
import qualified Language.Slim.Data                   as Slim
import qualified Language.Slim.ToSlim                 as Slim
import qualified Language.Slim.State                  as State

import Control.Monad.State(runState)

data Cfg = Cfg { indentSize :: Int
               , scope :: [String]
               , typeRefMap :: TD.TypeRefMap
               , paletteDics :: (TP.TypeDictionary , TP.ConstDictionary)
               , slimDics ::  (TD.TypeRefMap, TD.ConstWordMap)
               , moduleName :: String
               , skelName :: String
               , moduleFiles :: [FilePath]
               , isRemoteHandle :: Bool
               , useStandardTypes :: Bool
               } deriving Show

toSlim :: Cfg -> (ID.ScopedName, ID.Declaration) -> Slim.Slim
toSlim cf dec = Slim.fromDecs (moduleFiles cf) (paletteDics cf) (slimDics cf) [dec]

runSlim :: Cfg -> State.StateM a -> a
runSlim cf pp =
   let   palds = (paletteDics cf)
         (trm, iidmap) = (slimDics cf)
   in    fst $ runState pp (State.new palds trm iidmap)

new :: Int -> String -> [FilePath] -> ID.Idl -> Bool -> Cfg
new idt modName modFiles idl useStdTypes =
   let
         dictionaries = case TP.idlToDictionaries Nothing idl :: Maybe TP.Dictionaries of
                           Just dicts -> dicts
                           Nothing -> error "Failed to parse IDL dictionaries"
         (types,consts,_,_) = dictionaries
         (trfm,_,iidm) =  TD.typeRefMap (types, consts) [idl]
   in    Cfg idt [] trfm (types,consts)  (trfm,iidm) modName ("lib" ++ modName ++ "_skel.so") modFiles False useStdTypes

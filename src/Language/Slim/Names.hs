-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

module Language.Slim.Names where

import qualified Language.Slim.Data                   as Slim
import qualified Language.Slim.State                  as State
import qualified Language.Idl.Data                    as ID
import qualified Language.Slim.Ref                    as Ref
import Control.Monad(liftM)


getMethodStrings :: ID.Declaration -> State.StateM Slim.MethodStrings
getMethodStrings (ID.Declaration _ _ _ (ID.Identifier name) (ID.OperationDcl _ _ _ _ params)) = do
   paramNames <- mapM getParameterName params
   return $ Slim.MethodStrings (Slim.StringRef (Ref.Val name)) paramNames
getMethodStrings _ = error "internal error: pattern match"

getParameterName :: ID.Parameter -> State.StateM Slim.ParameterStrings
getParameterName (ID.Parameter (ID.Identifier name) _ ty) = do
   typeName <- getType ty
   return $ Slim.ParameterStrings (Slim.StringRef (Ref.Val name)) typeName

typeFromDef :: ID.Definition -> State.StateM Slim.TypeStrings
typeFromDef (ID.InterfaceDcl _ _ _ _ _ _)    = return $ Slim.NoName
typeFromDef (ID.TypeDcl ty)                = getType ty
typeFromDef _                              = error "internal error: unexpected definition"

getType :: ID.Type -> State.StateM Slim.TypeStrings
getType (ID.TypeRef sp _ sn)               = do
   def <- State.lookupDef (sp,sn)
   typeFromDef  def

getType (ID.Struct _ mms)                   = do
   (names,types) <- liftM unzip $ mapM fromMem mms
   return $ Slim.TypeStrings names types

getType (ID.Union _ _ _ cases Nothing)      = do
   (names,types) <- liftM unzip $ mapM fromCase cases
   return $ Slim.TypeStrings names types

getType (ID.Union _ _ _ cases (Just dc))      = do
   (dn,dt) <- fromMem dc
   (names,types) <- liftM unzip $ mapM fromCase cases
   return $ Slim.TypeStrings (names ++ [dn]) (types ++ [dt])

getType (ID.Array _ tp)                       = do
   getType tp

getType (ID.Sequence  _ tp)                    = do
   getType tp

getType _                                        = return $ Slim.NoName

fromMem :: (ID.Member) -> State.StateM (Slim.StringRef, Slim.TypeStrings)
fromMem (ID.Member _ _ (ID.Identifier name) tp) = do
   tp' <- getType tp
   return (Slim.StringRef (Ref.Val name),tp')

fromCase :: (ID.UnionCase) -> State.StateM (Slim.StringRef,  Slim.TypeStrings)
fromCase (ID.UnionCase _ mm) = fromMem mm

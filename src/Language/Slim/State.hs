-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.Slim.State where

import qualified Language.Idl.ToPalette               as TP
import qualified Data.Omg.Literal                     as L
import qualified Language.Slim.TypeRefMap             as TD
import qualified Language.Slim.Data                   as Slim
import qualified Control.Monad.State                  as State
import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Language.Idl.Data                    as ID
import qualified Data.Map                             as Map
import qualified Data.Omg.Prim                        as OP

import Data.Int(Int32)
import Data.Word(Word32)
import Control.Monad.Except(runExceptT)
import Data.Maybe(fromMaybe)
import Control.Monad(liftM)

type StateM = State.State Slim.State


new :: (TP.TypeDictionary , TP.ConstDictionary) -> TD.TypeRefMap -> TD.ConstWordMap -> Slim.State
new palds trm iidmap = Slim.State palds trm iidmap (Slim.Scalars 0 0 0 0) ID.ParameterIn

lookupDef :: (Pos.SourcePos, ID.ScopedName) -> StateM ID.Definition
lookupDef (sp,sn) = do
   let from = fromMaybe (error $ "internal error: unresolved type ref" ++ (show (sp,sn)))
   decl <- liftM (snd . from . Map.lookup (sp,sn) . Slim.typeRefMap) $ State.get
   case decl of
      ID.Declaration _ _ _ _ df -> return df
      _ -> error "internal error: unexpected declaration type"


constExprToLit :: ID.ScopedName -> ID.Type -> ID.ConstExpr -> StateM L.Literal
constExprToLit  scn ty expr = do
      (types,consts) <- liftM Slim.paletteDics $ State.get
      -- In GHC 9.10, we use ExceptT String Maybe which has MonadFail instance
      let maybeResult = runExceptT $ do
               ty' <- TP.inlineType (types,consts) scn ty
               TP.constExpr consts scn ty' expr
      case maybeResult of
            Nothing -> error "Failed to evaluate constant expression"
            Just (Left err) -> error err
            Just (Right lit) -> return lit

constExprToInt32 :: ID.ConstExpr -> StateM Int32
constExprToInt32 ml = do
   ml' <- (liftM TD.literalToIntegral) $ constExprToLit [] (ID.PrimType OP.SignedLongType) ml
   return ml'

constExprToWord32 :: ID.ConstExpr -> StateM Word32
constExprToWord32 ml = do
   ml' <- (liftM TD.literalToIntegral) $ constExprToLit [] (ID.PrimType OP.UnsignedLongType) ml
   return ml'

constLookupWord32 :: ID.ConstExpr -> StateM (Word32)
constLookupWord32 expr = liftM (fromMaybe (error "internal error: constLookupWord32 failed")) (lookupIId (Just expr))

lookupIId :: Maybe ID.ConstExpr -> StateM (Maybe Word32)
lookupIId Nothing = return Nothing
lookupIId (Just expr) = do
   ml' <- liftM (Map.lookup expr . Slim.constMap) $ State.get
   return ml'



-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.Slim.TypeRefMap where

import qualified Language.Idl.Data                    as ID
import qualified Language.Idl.ToPalette               as TP
import qualified Data.Omg.Literal                     as L
import qualified Data.Omg.Prim                        as OP
import qualified Data.Map                             as Map
import qualified Data.List                            as List
import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import Data.Word(Word32)
import Control.Monad.Identity(runIdentity)
import Control.Monad.Error(runErrorT)

type TypeRef = (Pos.SourcePos, ID.ScopedName)
type TypeRefMap = Map.Map TypeRef (ID.ScopedName, ID.Declaration)
type ConstWordMap = Map.Map ID.ConstExpr Word32

typeRefMap :: (TP.TypeDictionary , TP.ConstDictionary) -> [ID.Idl] -> (TypeRefMap, DeclarationMap, ConstWordMap)
typeRefMap pd idls =
   let res = List.foldl' fromIdl (new pd) idls
   in (typeRefDict res, decMap res, constMap res)

type DeclarationMap = Map.Map ID.ScopedName ID.Declaration
type Scope = (String, ID.ScopedName)
data State = State { typeRefDict :: TypeRefMap
                   , scope :: [Scope]
                   , decMap :: DeclarationMap
                   , constMap :: ConstWordMap
                   , paletteDics :: (TP.TypeDictionary , TP.ConstDictionary)
                    } deriving Show

fromIdl :: State -> ID.Idl -> State
fromIdl st (ID.Idl tlds) = List.foldl' fromTopDec st tlds

fromTopDec ::  State -> ID.TopLevelDeclaration -> State
fromTopDec st (ID.TopLevelDeclaration _ dc) = fromDec st dc

fromDec ::  State -> ID.Declaration -> State
fromDec st (ID.CommentBlock {}) = st
fromDec st dd@(ID.Declaration _ _ _ id' df) = fromDef id' df (addDec dd st)


fromDef :: ID.Identifier -> ID.Definition -> State -> State
fromDef (ID.Identifier nm) (ID.InterfaceDcl _ _ _ iid (sp, (Just sn)) decs) st =
   let   derivedIface = resolveName (scope st) (decMap st) sn
         st' =  addTypeRef (sp,sn) derivedIface $ addIId iid st
   in    List.foldl' (withDerivedScope (nm,sn) fromDec) st' decs
fromDef (ID.Identifier nm) (ID.InterfaceDcl _ _ _ iid (_, Nothing) decs) st   =
   let   st' = addIId iid st
   in    List.foldl' (withScope nm fromDec) st' decs
fromDef (ID.Identifier nm) (ID.ModuleDcl decs) st                           = List.foldl' (withScope nm fromDec) st decs

fromDef _ (ID.TypeDcl tp) st                = fromType tp st
fromDef _ (ID.ConstDcl tp _) st             = fromType tp st
fromDef _ (ID.OperationDcl _ _ _ tp pars) st    = List.foldl' fromParam (fromType tp st) pars

addIId :: (Maybe ID.ConstExpr) -> State -> State
addIId Nothing st = st
addIId (Just expr) st = addConst' (ID.PrimType OP.UnsignedLongType) expr st

addConst :: ID.ConstExpr -> State -> State
addConst expr st = addConst' (ID.PrimType OP.UnsignedLongType) expr st

addConst' :: ID.Type -> ID.ConstExpr -> State -> State
addConst' tp expr st =
      let
            scn = currentScopeName $ scope st
            (types,consts) = paletteDics st
            lit = runIdentity $ runErrorT $ do
                  ty' <- TP.inlineType (types,consts) scn tp
                  TP.constExpr consts scn ty' expr
            add :: Either String L.Literal -> State
            add (Left _) = st
            add (Right lit') =
               let   wrd = literalToIntegral lit'
                     iids = Map.insert expr wrd (constMap st)
               in    st { constMap = iids }
      in    add lit

literalToIntegral :: Integral a => L.Literal -> a
literalToIntegral (L.IntLiteral _ (L.SignedShortLiteral ii))      =   fromIntegral ii
literalToIntegral (L.IntLiteral _ (L.SignedLongLiteral ii))       =   fromIntegral ii
literalToIntegral (L.IntLiteral _ (L.SignedLongLongLiteral ii))   =   fromIntegral ii
literalToIntegral (L.IntLiteral _ (L.UnsignedShortLiteral ii))    =   fromIntegral ii
literalToIntegral (L.IntLiteral _ (L.UnsignedLongLiteral ii))     =   fromIntegral ii
literalToIntegral (L.IntLiteral _ (L.UnsignedLongLongLiteral ii)) =   fromIntegral ii
literalToIntegral (L.IntLiteral _ (L.OctetLiteral ii))            =   fromIntegral ii
literalToIntegral ll = error $ "internal error:literalToIntegral:unexpected type: " ++ (show ll)



fromType :: ID.Type -> State -> State
fromType (ID.PrimType {}) st                = st
fromType (ID.Struct _ mms) st               = List.foldl' fromMem st mms
fromType (ID.Union _ _ tp ucs dc) st        = List.foldl' (fromCase tp) (fromDefCase dc $ fromType tp st) ucs
fromType (ID.Enum {}) st                    = st
fromType (ID.Sequence  _ tp) st              = fromType tp st
fromType (ID.StringType {}) st              = st
fromType (ID.WideStringType {}) st          = st
fromType (ID.DmahandleType {}) st           = st
fromType (ID.Array _ tp) st                 = fromType tp st
fromType (ID.Interface) st                  = st
fromType (ID.Native _) st                   = st
fromType (ID.TypeRef sp _ sn) st            = addTypeRef (sp,sn) (resolveName (scope st) (decMap st) sn) st

fromMem :: State -> (ID.Member) -> State
fromMem st (ID.Member _ _ _ tp) = fromType tp st

fromCase :: ID.Type -> State -> (ID.UnionCase) -> State
fromCase desc st (ID.UnionCase css mm) = List.foldl' (flip (addConst' desc)) (fromMem st mm) css

fromDefCase :: (Maybe ID.DefaultCase) -> State -> State
fromDefCase Nothing st     = st
fromDefCase (Just mm) st   = fromMem st mm

fromParam :: State -> ID.Parameter -> State
fromParam st (ID.Parameter _ _ tp) = fromType tp st

addTypeRef :: (Pos.SourcePos, ID.ScopedName) -> (Maybe (ID.ScopedName, ID.Declaration)) -> State -> State
addTypeRef tr (Just dec) st = st { typeRefDict = Map.insert tr (dec) (typeRefDict st) }
addTypeRef (sp, sn) Nothing _ = error $ TP.errorMessage sp $ "internal error not in scope: " ++ (concat $ List.intersperse "::" sn)

resolveName :: [Scope] -> DeclarationMap -> ID.ScopedName -> Maybe (ID.ScopedName, ID.Declaration)
resolveName [] dm sn = do
   rv <- Map.lookup sn dm
   return (sn, rv)
resolveName sc@((_,[]):rest) dm sn = (((currentScopeName sc) ++ sn) `dmLookup` dm)
                                    <|> resolveName rest dm sn
resolveName sc@((_,dsn):rest) dm sn =
   do { (((currentScopeName sc) ++ sn) `dmLookup` dm)
    <|> ((dsn ++ sn) `dmLookup` dm)                      --try derived interfaces scope
    <|> resolveName rest dm sn
      }

dmLookup :: Ord t => t -> Map.Map t t1 -> Maybe (t, t1)
dmLookup sn dm = do
   rv <- Map.lookup sn dm
   return (sn, rv)

currentScopeName :: [Scope] -> ID.ScopedName
currentScopeName sc = (reverse $ fst $ unzip sc)

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) Nothing other  = other
(<|>) this    _      = this

withScope :: String -> (State -> b -> State) -> State -> b -> State
withScope nm ff st b =
   let
         oc = scope st
         nst = ff (pushScope nm st) b
   in    nst { scope = oc }

withDerivedScope :: (String, ID.ScopedName) -> (State -> b -> State) -> State -> b -> State
withDerivedScope nm ff st b =
   let
         oc = scope st
         nst = ff (pushDerivedScope nm st) b
   in    nst { scope = oc }

pushScope :: String -> State -> State
pushScope nm st =
   st { scope = (nm, []) : (scope st) }

pushDerivedScope :: (String, ID.ScopedName) -> State -> State
pushDerivedScope (nm,sn) st  =
   st { scope = (nm,sn) : (scope st) }


addDec :: ID.Declaration -> State -> State
addDec dd@(ID.Declaration _ _ _ (ID.Identifier nm) _) st =
   let
         sn = reverse (nm : (fst $ unzip $ scope st))
   in    st { decMap = Map.insert sn dd (decMap st) }

addDec (ID.CommentBlock _ ) st = st

new :: (TP.TypeDictionary , TP.ConstDictionary) -> State
new pd = State Map.empty [] Map.empty Map.empty pd


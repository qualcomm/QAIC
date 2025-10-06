-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Language.SO.Code where
import Prelude hiding ( (<>) )
import Text.PrettyPrint
import Data.List(nubBy, nub, elemIndex, sortBy, groupBy)
import Data.Function(on)
import Data.Maybe(fromMaybe)
import Language.SO.ToHeader ( commals, funcDeclare )
import Data.Generics ( listify, Typeable, Data )
import qualified Data.Map as Map

data Code = D Declr
          | E Expr
          | F String
          deriving (Data, Typeable, Eq, Show)

data Declr = SD String
           | CD Contextual --only declared when not a function argument
           deriving (Data, Typeable, Eq, Show)

data Expr = SE String
          | CE Function [Expr]
          | BE String [Code]
          deriving (Data, Typeable, Eq, Show)

data Function = Function String String String [FuncArgType] [Code]
           deriving (Data, Typeable, Eq, Show)

data FuncArgType = AS String
                 | AV String Contextual
                 deriving (Data, Typeable, Eq, Show)

data Contextual = Contextual [Code]
               deriving (Data, Typeable, Eq, Show)

tes :: [String] -> [Expr]
tes docs = map SE docs

er :: Doc -> Code
er doc = E $ SE $ render $ doc

ser :: Doc -> Expr
ser doc = SE $ render $ doc

es :: String -> Code
es doc = E $ SE $ doc

ds :: String -> Code
ds doc = D $ SD $ doc

dr :: Doc -> Code
dr doc = D $ SD $ render $ doc

drs :: Doc -> Code
drs doc = D $ SD $ render $ doc <> semi

compile :: [Function] -> Doc
compile codes =
   let   funcs = allFuncs codes
   in    vcat (map (funcImplement (toFuncMap funcs)) funcs)

cCall :: FuncMap -> Function -> [Expr] -> Doc
cCall funcs ff [expr] | ff == (returnF)  = text "return " <> (hcat $ cExpr funcs expr)
cCall funcs ff codes                     = funcName funcs ff <> parens (commals $ concatMap (cExpr funcs) codes)

returnF :: Function
returnF = Function "" "" "return" [] []

returnE :: Expr -> Expr
returnE expr = returnF `CE` [expr]

cExpr :: FuncMap -> Expr -> [Doc]
cExpr _ (SE dd) = [text dd]
cExpr fm (BE nm dd) = [cBlock nm $ cSections $ cCodes fm [] dd]
cExpr funcs (CE func exprs) = [cCall funcs func exprs]

funcImplement :: FuncMap -> Function -> Doc
funcImplement _ (Function "" "" _ [] []) = empty
funcImplement funcs ff@(Function dclr rv _ types imp) =
         (funcDeclare dclr (text rv) (funcName funcs ff) (map funcArgType types)) <+> text "{"
   $+$   nest 3 (cSections $ cCodes funcs (concatMap toContextual types) imp)
   $+$   text "}"

cSections :: Sections -> Doc
cSections (aa,bb,cc) = vcat $  aa ++ bb ++ cc

toContextual :: FuncArgType -> [Contextual]
toContextual (AS _) = []
toContextual (AV _ vv) = [vv]

funcArgType :: FuncArgType -> Doc
funcArgType (AS str) = text str
funcArgType (AV str _) = text str

funcName :: FuncMap -> Function -> Doc
funcName _ (Function "" "" name [] []) = text name
funcName funcs ff@(Function _ _ name _ _) = text name <> ix (funcIndex funcs ff)
   where
      ix 0 = empty
      ix ix' = text "_" <> int ix'

allFuncs :: [Function] -> [Function]
allFuncs codes =
   let is' (Function {}) = True
   in  nub $ reverse $ listify is' codes

type FuncMap = Map.Map String [Function]

funcIndex :: FuncMap -> Function -> Int
funcIndex funcmap func = fromMaybe 0 $ do
   funcs <- Map.lookup (toName func) funcmap
   ix <- func `elemIndex` funcs
   return $ ix

toName :: Function -> String
toName (Function _ _ name _ _) = name

toFuncMap :: [Function] -> FuncMap
toFuncMap funcs =
   let
         sort' = sortBy (compare `on` toName)
         group' = groupBy ((==) `on` toName)
         pair ls = (toName (head ls), ls)
   in    Map.fromList (map pair $ group' $ sort' $ funcs)

type Sections =  ([Doc], [Doc], [Doc])

cDeclr :: FuncMap -> [Contextual] -> Declr -> Sections
cDeclr _ _ (SD str)    = ([text str],[],[])
cDeclr funcs vars (CD var@(Contextual codes))
   | var `elem` vars = ([],[],[])
   | otherwise = cCodes funcs vars codes

cCodes :: FuncMap -> [Contextual] -> [Code] -> Sections
cCodes funcs fargs codes =
   let
         eqDoc aa bb = (render aa) == (render bb)

         nub' = nubBy eqDoc

         (dds, ees, ffs)  = concatSections $ map toSections codes
         toSections :: Code ->  Sections
         toSections (D dd)         = cDeclr funcs fargs dd
         toSections (E (BE nm inner)) =
            let   (decs, expr, fins) = cCodes funcs fargs inner
            in    (decs, [(cBlock nm $ vcat $ nub' expr)], fins)
         toSections (E ee@(CE {})) = ([], map (\ expr -> expr <> semi) (cExpr funcs ee), [])
         toSections (E ee)         = ([], (cExpr funcs ee), [])
         toSections (F ff)         = ([], [], [text ff])

   in    ((nub' dds), (nub' ees), (nub' ffs))

concatSections :: [Sections] -> Sections
concatSections sects =
   let (aas,bbs,ccs) = unzip3 sects
   in  (concat aas, concat bbs, concat ccs)

cBlock :: String -> Doc -> Doc
cBlock nm dd = (text nm) <> text "{" $+$  (nest 3 dd) $+$ text "}"

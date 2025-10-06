-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XTypeSynonymInstances -fno-warn-orphans -XFlexibleInstances #-}
module Language.C.TypeDesc where

--literal C Type
--an idl Union would be a (StructOrUnion Struct [ TypeDesc [Member "_d"] (Primitive desc)
--                                              , TypeDesc [Member "_u"] (AnonStructOrUnion (Union [members]))
--                                              ]
--
--import Language.C.Util(dbg')
import Data.Omg.Literal(Literal ( Nil
                                ,CharLiteral
                                ,WCharLiteral
                                ,StringLiteral
                                ,WStringLiteral
                                ,IntLiteral
                                ,RealLiteral
                                ,BoolLiteral
                                )
                       ,Base(Hex, Dec, Oct)
                       ,IntegerLiteral(SignedShortLiteral
                                      ,SignedLongLiteral
                                      ,SignedLongLongLiteral
                                      ,UnsignedShortLiteral
                                      ,UnsignedLongLiteral
                                      ,UnsignedLongLongLiteral
                                      ,OctetLiteral
                                      )
                       ,RealLit(FloatLit, DoubleLit)
                       )
import Data.Generics( everywhere
                    , mkT
                    )
import Prelude hiding ( (<>) )
import Numeric(showInt, showHex, showOct)
import Data.Omg.Prim( Prim( UnsignedShortType
                          , UnsignedLongType
                          , UnsignedLongLongType
                          , SignedShortType
                          , SignedLongType
                          , SignedLongLongType
                          , UnsignedCharFixedTType
                          , UnsignedShortFixedTType
                          , UnsignedLongFixedTType
                          , UnsignedLongLongFixedTType
                          , SignedCharFixedTType
                          , SignedShortFixedTType
                          , SignedLongFixedTType
                          , SignedLongLongFixedTType
                          , UnsignedCharFixedType
                          , UnsignedShortFixedType
                          , UnsignedLongFixedType
                          , UnsignedLongLongFixedType
                          , SignedCharFixedType
                          , SignedShortFixedType
                          , SignedLongFixedType
                          , SignedLongLongFixedType
                          , BooleanType
                          , FloatType
                          , DoubleType
                          , OctetType
                          , CharType
                          , WideCharType
                          )
                     , primSize
                     )
import Data.Maybe( isJust
                 , listToMaybe
                 , fromJust
                 , fromMaybe
                 , isNothing
                 )

import Language.C.Pretty( Cfg(Cfg)
                        , indentSize
                        , pretty
                        , Pretty
                        , lang
                        , Lang (CPlusPlus, C89)
                        )

import Language.Idl.Data(Comment(Comment), PostDoc)

import Data.Generics( everything
                    , mkQ
                    )
import Data.Data( Data
                )
import Data.Function(on)
import Data.List( intercalate
                , nubBy
                , isPrefixOf
                )
import Data.Typeable( Typeable
                    )

import qualified Data.Typeable as T(typeOf)

import System.FilePath( takeBaseName)
import Text.PrettyPrint( text
                       , (<>)
                       , (<+>)
                       , ($+$)
                       , brackets
                       , Doc
                       , parens
                       , int
                       , char
                       , vcat
                       , hcat
                       , semi
                       , comma
                       , empty
                       , nest
                       , float
                       , double
                       --, render
                       , lbrace
                       , rbrace
                       , quotes
                       )
class CType a where
   typeDef :: Cfg -> a -> Doc
   typeOf :: Cfg -> a -> Doc
   sizeOf :: a -> Int
   alignmentOf :: a -> Int

data TypeDesc = TypeDesc { descId :: DescId
                         , descAttr :: [Attribute]
                         , descType :: Type
                         }
              deriving (Typeable, Data, Eq, Show)

data IId = IId TypeName Literal
         deriving (Typeable, Data, Show, Eq)

data Type = StructOrUnion     TypeName Container
          | AnonStructOrUnion Container
          | Primitive         Prim
          | Array             Int TypeDesc
          | Enum              TypeName [([Comment], String)]
          | Object            (Either DescId (TypeName, IId))
          | Native            TypeName
          deriving (Typeable, Data, Show, Eq)

data Container = Struct [TypeDesc]
               | Union [TypeDesc]
               deriving (Typeable, Data, Show, Eq)

data Attribute = MemberNameAttr  MemberName
               | ParentIdAttr    ParentId
               | UnionCaseAttr   UnionCase
               | SeqLenAttr      SeqLen
               | SeqLenReqAttr   SeqLenReq
               | SeqAttr         Seq
               | PositionAttr    Position
               | ModeAttr        Mode
               | NotNilAttr      NotNil
               | ObjIIdAttr      ObjIId
               | ForwardObjAttr  ForwardObj
               | CPtrAttr        CPtr
               | CConstAttr      CConst
               | ParamATPCSAttr  ParamATPCS
               | NotArgAttr      NotArg --input string lengths are not arguments but we have to marshall them
               | ShadowAttr      Shadow   --in types for the inrout parameters are "shadow" types
               | ShadowedAttr    Shadowed --rout types for the inrout parameters are "shadow" types
               | StringConAttr   StringCon
               | SeqConAttr      SeqCon
               | StructATPCSAttr StructATPCS
               | TypeDefAttr     TypeDef
               | UnionDescAttr   UnionDesc
               | RUnionAttr      RUnion
               | TypeCommentAttr TypeComment
               deriving (Typeable, Data, Show, Eq)

data Scope = Scope { fromScope :: [String] }
           deriving (Typeable, Data, Show, Eq)

data Declaration = Declaration { decIncPath :: IncludePath
                               , decComment :: ([Comment],[Comment])
                               , decScope :: Scope
                               , decType :: DeclarationType
                               }
                 deriving (Typeable, Data, Show, Eq)

data DeclarationType = TypeDcl      TypeDesc
                     | TypeDefDcl   TypeName (Either TypeName TypeDesc)
                     | Const        TypeName TypeDesc Literal
                     | CommentDcl
                     | InterfaceDcl Interface
                     | LocalInterfaceDcl Interface
                     | ForwardInterfaceDcl Interface
                     deriving (Typeable, Data, Show, Eq)

data Interface = Interface TypeDesc (Maybe Interface) TypeName [Method]
               deriving (Typeable, Data, Show, Eq)

data MethodType = TwoWayMethod
                | ReliableOneWayMethod
                | OneWayMethod
                | SignalMethod
                deriving (Typeable, Data, Show, Eq)

data TypeComment = TypeComment { typeComment :: PostDoc }
                 deriving (Typeable, Data, Show, Eq)


data Method = Method ([Comment],[Comment]) MethodType TypeName TypeDesc [Parameter]
            deriving (Typeable, Data, Show, Eq)

data Parameter = Parameter { fromParameter :: TypeDesc }
               deriving (Typeable, Data, Show, Eq)

data TypeDef   =  TypeDef { fromTypeDef :: [TypeName] }
               deriving (Typeable, Data, Show, Eq)

data CaseLabel = Constant [Literal]
               | Default
               deriving (Typeable, Data, Show, Eq)

data IncludePath  = IncludePath     { includePath :: FilePath }
                  deriving (Typeable, Data, Show, Eq)
data MemberName   = MemberName     { memberName :: String }
                  deriving (Typeable, Data, Show, Eq)
data ParentId     = ParentId         { parentId :: DescId }
                  deriving (Typeable, Data, Show, Eq)
data UnionCase    = UnionCase       { caseLabel :: CaseLabel }
                  deriving (Typeable, Data, Show, Eq)
data UnionDesc    = UnionDesc
                  deriving (Typeable, Data, Show, Eq)
data RUnion       = RUnion       { unionDesc :: DescId }
                  deriving (Typeable, Data, Show, Eq)
data Position     = Position        { position :: Int }
                  deriving (Typeable, Data, Show, Eq)
data ObjIId       = ObjIId
                  deriving (Typeable, Data, Show, Eq)
data ForwardObj   = ForwardObj
                  deriving (Typeable, Data, Show, Eq)
data ParamATPCS   = ParamATPCS
                  deriving (Typeable, Data, Show, Eq)
data StructATPCS  = StructATPCS Int String
                  deriving (Typeable, Data, Show, Eq)
data NotNil       = NotNil
                  deriving (Typeable, Data, Show, Eq)
data NotArg       = NotArg
                  deriving (Typeable, Data, Show, Eq)
data Shadow       = Shadow
                  deriving (Typeable, Data, Show, Eq)
data Shadowed     = Shadowed
                  deriving (Typeable, Data, Show, Eq)
data StringCon    = StringCon { stringBuf :: DescId }
                  deriving (Typeable, Data, Show, Eq)
data SeqLen       = SeqLen
                   deriving (Typeable, Data, Show, Eq)
data SeqLenReq    = SeqLenReq
                  deriving (Typeable, Data, Show, Eq)
data SeqCon       = SeqCon { seqBuf :: DescId }
                  deriving (Typeable, Data, Show, Eq)
data Seq      = Seq { seqLen :: DescId
                    , seqLenReq :: DescId
                    , nullT :: Bool
                    , maxLen :: Int
                    }
                  deriving (Typeable, Data, Show, Eq)

data CPtr         = CPtr
                  deriving (Typeable, Data, Show, Eq)

data CConst       = CConst
                  deriving (Typeable, Data, Show, Eq)

type DescId  = Int
data TypeName = TypeName { fromTypeName :: [String] }
              deriving (Typeable, Data, Show, Eq)

class Primary a where
   primary :: Mode -> a -> Maybe a

data Mode = In
          | ROut
          | Out
          deriving (Typeable, Data, Show, Eq)

isTypeDef :: TypeDesc -> Bool
isTypeDef (TypeDesc _ attr _ ) = not $ null $ typeDefNames attr

arraySizes :: Type -> [Int]
arraySizes ty =
   let
         tdSizes td
            | isTypeDef td = []
         tdSizes (TypeDesc _ _ tt) = sizes tt
         sizes (Array mx td) = mx : tdSizes td
         sizes _ = []
   in    sizes ty

prettyArraySizes :: [Int] -> Doc
prettyArraySizes sizes = (hcat (map (brackets . int) sizes))

typeName :: Cfg -> TypeDesc -> Doc
typeName cf td@(TypeDesc _ attr tt@(Array {}))
   | not $ isTypeDef td = (pretty cf $ (\ ss@(StructATPCS {}) -> ss) `query` attr)
                      $+$ typeOf cf td
                      <+> (pretty cf $ memberName `query` attr)
                       <> (prettyArraySizes (arraySizes tt))

typeName cf td@(TypeDesc _ attr _)  = (pretty cf $ (\ ss@(StructATPCS {}) -> ss) `query` attr)
                                  $+$ typeOf cf td
                                  <+> (pretty cf $ memberName `query` attr)

structIfC :: Cfg -> Type -> [Attribute] -> Doc

structIfC (Cfg { lang = C89 }) (Object (Right _)) attr
   | (ForwardObj `is` attr) = text "struct"
   | (isJust $ seqLen `query` attr) = text "struct"

structIfC _ _ _ = empty


instance CType TypeDesc where
   typeDef cf (TypeDesc _ attr tt@(Object {}))  =  (pretty cf $ CConst `find` attr)
                                              <+> structIfC cf tt attr
                                              <+> typeDef cf tt
                                               <> text "*"
                                               <> (pretty cf $ CPtr `find` attr)
                                              <+> (pretty cf $ memberName `query` attr)

   typeDef cf (TypeDesc _ attr tt)  =  (pretty cf $ CConst `find` attr)
                                  <+> typeDef cf tt
                                   <> (pretty cf $ CPtr `find` attr)
                                  <+> (pretty cf $ memberName `query` attr)


   typeOf cf (TypeDesc _ attr tt@(AnonStructOrUnion {}))  = (pretty cf $ CConst `find` attr)
                                                        <+> (typeOf cf tt)
                                                         <>  (pretty cf $ CPtr `find` attr)


   typeOf cf (TypeDesc _ attr tt@(Object {}))  = (pretty cf $ CConst `find` attr)
                                              <+> structIfC cf tt attr
                                              <+> ((typeOf cf tt) `maybeHead` (map (pretty cf) $ typeDefNames attr))
                                               <> text "*"
                                               <>  (pretty cf $ CPtr `find` attr)


   typeOf cf (TypeDesc _ attr tt)  = (pretty cf $ CConst `find` attr)
                                 <+> ((typeOf cf tt) `maybeHead` (map (pretty cf) $ typeDefNames attr))
                                  <> (pretty cf $ CPtr `find` attr)

   sizeOf (TypeDesc _ attr (Array _ _))
      | (isJust $ position `query` attr) = ptrSize
   sizeOf (TypeDesc _ attr tt)
      | (CPtr `is` attr) = ptrSize
      | otherwise = sizeOf tt
   alignmentOf (TypeDesc _ attr (Array _ _))
      | (isJust $ position `query` attr) = ptrSize
   alignmentOf (TypeDesc _ attr tt)
      | (CPtr `is` attr) = ptrSize
      | otherwise = alignmentOf tt

instance CType Type where
   typeDef cf (Native tn) =  pretty cf tn
   typeDef cf (Primitive pp) =  typeDef cf pp
   typeDef cf (AnonStructOrUnion su) = (typeOf cf su) <+> lbrace
                                   $+$ nest (indentSize cf) (typeDef cf su)
                                   $+$ rbrace
   typeDef cf (StructOrUnion name su) = (typeOf cf su) <+> (pretty cf name) <+> lbrace
                                    $+$ nest (indentSize cf) (typeDef cf su)
                                    $+$ rbrace
   typeDef cf (Enum name labels) =
            text "enum" <+> (pretty cf name) <+> lbrace
      $+$   (nest (indentSize cf) (vcat $ map (prettyLabel cf name) labels))
      $+$   (nest (indentSize cf) (text "_32BIT_PLACEHOLDER_" <> pretty cf name <+> text "= 0x7fffffff"))
      $+$   rbrace
   typeDef cf (Array _ tp) = typeOf cf tp
   typeDef _ (Object (Left _)) = text "void"
   typeDef cf (Object (Right (tn,_))) = (pretty cf tn)

   typeOf cf@(Cfg { lang = CPlusPlus }) (Native (TypeName [name,""])) =  pretty cf (TypeName [name]) --this is to make native int; work in c++
   typeOf cf (Native tn) =  pretty cf tn
   typeOf cf (Primitive pp) =  typeOf cf pp
   typeOf cf as@(AnonStructOrUnion _) = typeDef cf as
   typeOf cf  (StructOrUnion name su) = (typeOf cf su) <+> pretty cf name
   typeOf cf (Array _ tp) = typeOf cf tp
   typeOf cf  (Enum tp _) = text "enum" <+> pretty cf tp
   typeOf cf tp@(Object {}) = typeDef cf tp

   sizeOf (Primitive pp) =  sizeOf pp
   sizeOf (AnonStructOrUnion su) = sizeOf su
   sizeOf (StructOrUnion _ su) = sizeOf su
   sizeOf (Array mx tp) = (sizeOf tp) * mx
   sizeOf (Enum _ _) = intSize
   sizeOf (Object _) = ptrSize
   sizeOf (Native _) =  ptrSize

   alignmentOf (Primitive pp) =  alignmentOf pp
   alignmentOf (AnonStructOrUnion su) = alignmentOf su
   alignmentOf (StructOrUnion _ su) = alignmentOf su
   alignmentOf (Array _ tp) = (alignmentOf tp)
   alignmentOf (Enum _ _) = intSize
   alignmentOf (Object _) = ptrSize
   alignmentOf (Native _ ) = ptrSize

prettyLabel :: Cfg -> TypeName -> ([Comment], String) -> Doc
prettyLabel cf name (cm, lbl) = pretty cf (enumLabel name lbl) <> comma <+> vcat (map comment cm)
   where
      comment (Comment True s)  = text ("/*" ++ s ++ "*/")
      comment (Comment False s) = text ("//" ++ s)

typeNameWithComment :: Cfg -> TypeDesc -> Doc
typeNameWithComment cf td = typeName cf td <> semi <+> pretty cf (typeComment `query` (descAttr td))

instance Pretty [Comment] where
   pretty cf ccs = vcat $ map (pretty cf) ccs

instance Pretty Comment where
   pretty cf cc =
      let
            comment (Comment True s)  = text "/*" <> vcat (map text (chopWhiteSpace (str cf s))) <> text "*/"
            comment (Comment False s) = text (unlines (map ("//"++) (lines (str cf s))))

            chopWhiteSpace :: String -> [String]
            chopWhiteSpace = map (dropWhile ((==) ' ')) . lines

            str (Cfg { lang = C89 }) s = replace "::" "_" s
            str _                    s = s

            replace                     :: (Eq a) => [a] -> [a] -> [a] -> [a]
            replace []  _   _            = error "internal error: replace nothing with something?"
            replace _   _   []           = []
            replace old new xs
               | isPrefixOf old xs       = new ++ replace old new (drop (length old) xs)
               | otherwise               = head xs : replace old new (tail xs)
      in comment cc

instance CType Container where
   typeDef cf ss@(Struct mms)
      | (snd (structSizeOf (alignmentOf ss) mms)) /= 0 =
            let      pad = (snd (structSizeOf (alignmentOf ss) mms))
            in       (vcat $  map (typeNameWithComment cf) mms)
                 $+$ (pretty cf (StructATPCS pad "__pad"))

   typeDef cf (Struct mms) = vcat $ map (typeNameWithComment cf) mms
   typeDef cf (Union mms) = vcat $ map (typeNameWithComment cf) mms
   typeOf _ (Union _) = text "union"
   typeOf _ (Struct _) = text "struct"

   sizeOf ss@(Struct mms) = fst $ structSizeOf (alignmentOf ss) mms

   sizeOf (Union mms) = maximum $ map sizeOf mms

   alignmentOf (Struct mms) = maximum $ map alignmentOf mms
   alignmentOf (Union mms) = maximum $ map alignmentOf mms

structSizeOf :: Int -> [TypeDesc] -> (Int,Int)
structSizeOf al mms =
      let   size zz mm = structSizeAccum zz (alignmentOf mm) (sizeOf mm)
            size' = foldl size 0 mms
            offset = structMemberOffset size' al
      in    (size' + offset,offset)

structMemberOffset :: Int -> Int -> Int
structMemberOffset total alignment
   | total `mod` alignment == 0 = 0
   | otherwise = alignment - (total `mod` alignment)

structSizeAccum :: Int -> Int -> Int -> Int
structSizeAccum total alignment size  = total + (structMemberOffset total alignment) + size

instance CType Prim where
   typeDef _ UnsignedShortType    = (text "unsigned short")
   typeDef _ UnsignedLongType     = (text "unsigned int")
   typeDef _ UnsignedLongLongType = (text "unsigned long long")
   typeDef _ SignedShortType      = (text "short")
   typeDef _ SignedLongType       = (text "int")
   typeDef _ SignedLongLongType   = (text "long long")
   typeDef _ UnsignedCharFixedTType    = (text "uint8_t")
   typeDef _ UnsignedShortFixedTType   = (text "uint16_t")
   typeDef _ UnsignedLongFixedTType    = (text "uint32_t")
   typeDef _ UnsignedLongLongFixedTType= (text "uint64_t")
   typeDef _ SignedCharFixedTType      = (text "int8_t")
   typeDef _ SignedShortFixedTType     = (text "int16_t")
   typeDef _ SignedLongFixedTType      = (text "int32_t")
   typeDef _ SignedLongLongFixedTType  = (text "int64_t")
   typeDef _ UnsignedCharFixedType    = (text "uint8")
   typeDef _ UnsignedShortFixedType   = (text "uint16")
   typeDef _ UnsignedLongFixedType    = (text "uint32")
   typeDef _ UnsignedLongLongFixedType= (text "uint64")
   typeDef _ SignedCharFixedType      = (text "int8")
   typeDef _ SignedShortFixedType     = (text "int16")
   typeDef _ SignedLongFixedType      = (text "int32")
   typeDef _ SignedLongLongFixedType  = (text "int64")
   typeDef _ BooleanType          = (text "boolean")
   typeDef _ FloatType            = (text "float")
   typeDef _ DoubleType           = (text "double")
   typeDef _ OctetType            = (text "unsigned char")
   typeDef _ CharType             = (text "char")
   typeDef _ WideCharType         = (text "_wchar_t")
   typeDef _ pp                   = error $ "internal error: unsupported prim type" ++ (show pp)

   typeOf cf pp = typeDef cf pp

   sizeOf p = primSize p

   alignmentOf p = primSize p

instance Primary TypeDesc where
   primary mm (TypeDesc tid attr tt)
      | (Shadow `is` attr) = do
            pin <- primary mm (TypeDesc tid (Shadow `rm` attr) tt)
            return $ pin `setMemberName` ("_sh_" ++ (memberName `get` attr))
   primary _ td
      | (isJust $ seqLen `query` (descAttr td)) = Nothing
   primary mm (TypeDesc tid attr tt@(AnonStructOrUnion {})) = (primary mm tt) >>= (return . (TypeDesc tid attr))
   primary mm (TypeDesc tid attr tt@(StructOrUnion {}))     = (primary mm tt) >>= (return . (TypeDesc tid attr))
   primary In td@(TypeDesc tid attr tt)
      | (In `is` attr) = (primary In tt) >>= (return . (TypeDesc tid attr))
      | (ROut `is` attr) = Nothing
      | (Out `is` attr) = error $ "internal error: out mode is not supported: " ++ (show td)
      | otherwise = Nothing
   primary ROut td@(TypeDesc tid attr tt)
      | (ROut `is` attr) = (primary ROut tt) >>= (return . (TypeDesc tid attr))
      | (In `is` attr) = Nothing
      | (Out `is` attr) = error $ "internal error: out mode is not supported: " ++ (show td)
      | otherwise = Nothing
   primary Out _ = error $ "internal error: out mode is not supported: "


addMode :: Mode -> TypeDesc -> TypeDesc
addMode mode param =
   let   modeAdd :: TypeDesc -> Mode -> TypeDesc
         modeAdd td ROut  --add the constraint types as In types
            | UnionDesc `is` (descAttr td) = modeAdd' ROut $ modeAdd' In $ td
            | SeqLen `is` (descAttr td) = modeAdd' ROut $ modeAdd' In td
            | ObjIId `is` (descAttr td) = modeAdd' In td
         modeAdd td In
            | SeqLenReq `is` (descAttr td) = td
         modeAdd td mm = modeAdd' mm td
         modeAdd' mm td = td { descAttr = (ModeAttr mm) : (descAttr td) }

   in    everywhere (mkT (\ td -> modeAdd td mode)) param


instance Primary Type where
   primary mm (Primitive pp) =  primary mm pp >>= (return . Primitive)
   primary mm (AnonStructOrUnion su) = primary mm su >>= (return . AnonStructOrUnion)
   primary mm tt@(StructOrUnion name su) = do
      psu <- primary mm su
      let check aa bb = (sizeOf $ aa) == (sizeOf $ bb) && (alignmentOf $ aa) == (alignmentOf $ bb)
      case(isCompatibleType tt && (check su psu)) of
         True ->  return $ StructOrUnion name psu
         False -> return $ AnonStructOrUnion $ psu
   primary mm (Array mx tp) = primary mm tp >>= (return . (Array mx))
   primary _ (Native _) = Nothing
   primary _ (Object _) = Nothing
   primary _ ee@(Enum _ _) = return ee

instance Primary Container where
   primary ROut (Struct mms) = do
      lst <- (maybeList $ map (primary ROut) mms)
      let   swapLenAndReq (aa:bb:rest) --common wire format marshals rout lenreq first then the sequence length as the wire length
               |  (SeqLen `is` (descAttr aa)) && (SeqLenReq `is` (descAttr bb)) = bb : aa : swapLenAndReq rest
            swapLenAndReq (aa:rest) = aa : swapLenAndReq rest
            swapLenAndReq [] = []
      return $ (Struct . setStructATPCS . (map (rmCPtrAttr . rmConstAttr))) $ swapLenAndReq lst
   primary mo (Struct mms) = (maybeList $ map (primary mo) mms) >>= (return . Struct . setStructATPCS . (map (rmCPtrAttr . rmConstAttr)))
   primary mo (Union mms) = (maybeList $ map (primary mo) mms) >>= (return . Union . (map (rmCPtrAttr . rmConstAttr)))

instance Primary Prim where
   primary _ p = return p

instance Pretty TypeName where
   pretty (Cfg { lang = CPlusPlus }) (TypeName nn) = text (intercalate "::" (reverse nn))
   pretty _ tn = text (cTypeName tn)

cTypeName :: TypeName -> String
cTypeName = sepTypeName "_"

cppTypeName :: TypeName -> String
cppTypeName = sepTypeName "::"

sepTypeName :: String -> TypeName -> String
sepTypeName sep (TypeName nn) = intercalate sep (dropheadnull (reverse nn))
   where
      dropheadnull ("":rest) = rest
      dropheadnull name      = name

instance Pretty a => Pretty (Maybe a) where
   pretty _ Nothing = empty
   pretty cf (Just aa) = pretty cf aa

instance Pretty CConst where
   pretty _ _ = text "const"

instance Pretty CPtr where
   pretty _ _ = text "*"

instance Pretty String where
   pretty _ str = text str

instance Pretty StructATPCS where
   pretty _ (StructATPCS off name) = text "AEEINTERFACE_PADMEMBERS" <> parens ( text name <> comma <+> int off)

instance Pretty Literal where
   pretty _  Nil = empty
   pretty _  (CharLiteral    ch)             = quotes $ char ch
   pretty _  (WCharLiteral   ch)             = text "L" <> quotes (char ch)
   pretty _  (StringLiteral  str)            = text $ show str
   pretty _  (WStringLiteral str)            = text $ "L" ++ (show str)

   pretty _  (IntLiteral Dec (UnsignedLongLongLiteral ii))  = showInt' ii "ull"
   pretty _  (IntLiteral Dec (UnsignedLongLiteral ii))      = showInt' ii "u"
   pretty _  (IntLiteral Dec (UnsignedShortLiteral ii))     = showInt' ii ""
   pretty _  (IntLiteral Dec (SignedLongLongLiteral ii))    = showInt' ii "ll"
   pretty _  (IntLiteral Dec (SignedLongLiteral ii))        = showInt' ii ""
   pretty _  (IntLiteral Dec (SignedShortLiteral ii))       = showInt' ii ""
   pretty _  (IntLiteral Dec (OctetLiteral ii))             = showInt' ii ""

   pretty _  (IntLiteral Hex (UnsignedLongLongLiteral ii))  = showHex' ii ""
   pretty _  (IntLiteral Hex (UnsignedLongLiteral ii))      = showHex' ii ""
   pretty _  (IntLiteral Hex (UnsignedShortLiteral ii))     = showHex' ii ""
   pretty _  (IntLiteral Hex (SignedLongLongLiteral ii))    = showHex' ii ""
   pretty _  (IntLiteral Hex (SignedLongLiteral ii))        = showHex' ii ""
   pretty _  (IntLiteral Hex (SignedShortLiteral ii))       = showHex' ii ""
   pretty _  (IntLiteral Hex (OctetLiteral ii))             = showHex' ii ""

   pretty _  (IntLiteral Oct (UnsignedLongLongLiteral ii))  = showOct' ii ""
   pretty _  (IntLiteral Oct (UnsignedLongLiteral ii))      = showOct' ii ""
   pretty _  (IntLiteral Oct (UnsignedShortLiteral ii))     = showOct' ii ""
   pretty _  (IntLiteral Oct (SignedLongLongLiteral ii))    = showOct' ii ""
   pretty _  (IntLiteral Oct (SignedLongLiteral ii))        = showOct' ii ""
   pretty _  (IntLiteral Oct (SignedShortLiteral ii))       = showOct' ii ""
   pretty _  (IntLiteral Oct (OctetLiteral ii))             = showOct' ii ""

   pretty _  (RealLiteral   (FloatLit ff))   = float ff
   pretty _  (RealLiteral   (DoubleLit ff))  = double ff
   pretty _  (BoolLiteral    True)  = text "TRUE"
   pretty _  (BoolLiteral    False) = text "FALSE"


showInt' :: (Show a, Integral a) => a -> String -> Doc
showInt' ii ss
   | ii >= 0       = text (showInt (ii) ss)
   | negate ii < 0 = text (show ii ++ ss)  -- 0x80000000 :: Int32
   | otherwise     = text "-" <> text (showInt (negate ii) ss)


showHex' :: (Show a, Integral a) => a -> String -> Doc
showHex' ii ss
   | ii >= 0       = text "0x" <> text (showHex (ii) ss)
   | negate ii < 0 = text (show ii ++ ss)  -- 0x80000000 :: Int32
   | otherwise     = text "-0x" <> text (showHex (negate ii) ss)

showOct' :: (Show a, Integral a) => a -> String -> Doc
showOct' ii ss
   | ii >= 0       = text "0" <> text (showOct (ii) ss)
   | negate ii < 0 = text (show ii ++ ss)  -- 0x80000000 :: Int32
   | otherwise     = text "-0" <> text (showOct (negate ii) ss)

enumLabel :: TypeName -> String -> TypeName
enumLabel (TypeName nms) lbl = TypeName (lbl : drop 1 nms)

addAttr::TypeDesc -> Attribute -> TypeDesc
addAttr td attr = td { descAttr = attr:(descAttr td) }

setPosition' :: [TypeDesc] -> [TypeDesc]
setPosition' tds =
   let
       isPos (PositionAttr _) = True
       isPos  _ = False
       rmPos td = td { descAttr = filter (not . isPos) (descAttr td) }
       setPos :: Int -> [TypeDesc] -> [TypeDesc]
       setPos _ [] = []
       setPos pp (td:rest)
         | NotArg `is` (descAttr td) = td : setPos pp rest
         | otherwise = (td `addAttr` (PositionAttr $ Position pp)) : setPos (pp + 1) rest
   in  setPos 0 $ map rmPos tds

rmConstAttr :: TypeDesc -> TypeDesc
rmConstAttr td =
   let is' (CConstAttr _) = True
       is'  _ = False
   in  td { descAttr = filter (not . is') (descAttr td) }

rmCPtrAttr' :: [Attribute] -> [Attribute]
rmCPtrAttr' attr =
   let is' (CPtrAttr _) = True
       is'  _ = False
   in  filter (not . is') attr

rmTypeDef' :: [Attribute] -> [Attribute]
rmTypeDef' attr =
   let is' (TypeDefAttr _) = True
       is'  _ = False
   in  filter (not . is') attr

rmCPtrAttr :: TypeDesc -> TypeDesc
rmCPtrAttr td = td { descAttr = rmCPtrAttr' (descAttr td) }

setMemberName :: TypeDesc -> String -> TypeDesc
setMemberName td name =
   let
       is' (MemberNameAttr _) = True
       is'  _ = False
       rm' td' = td' { descAttr = filter (not . is') (descAttr td') }
   in  (rm' td) `addAttr` (MemberNameAttr $ MemberName name)


addTypeDef :: TypeDesc -> TypeDef -> TypeDesc
addTypeDef td (TypeDef []) = td
addTypeDef td newDef =
   let is' (TypeDefAttr _) = True
       is'  _ = False
       rm' td' = td' { descAttr = filter (not . is') (descAttr td') }
       oldDef td' =  (TypeDef []) `fromMaybe` (id `query` (descAttr td'))
       add (TypeDef aa) (TypeDef bb) = (TypeDef $ aa ++ bb)
   in  (rm' td) `addAttr` (TypeDefAttr $ (newDef) `add` (oldDef td))

setParamATPCS :: [TypeDesc] -> [TypeDesc]
setParamATPCS tds =
   let atpcsAttr td
            | ((alignmentOf td) == 8) && (((position `get` (descAttr td)) `mod` 2) == 1) = td `addAttr` ParamATPCSAttr ParamATPCS
            | otherwise = td
       rm' td = td { descAttr = filter (not . ((==) (ParamATPCSAttr ParamATPCS))) (descAttr td) }
   in  map atpcsAttr $ map rm' tds

setStructATPCS :: [TypeDesc] -> [TypeDesc]
setStructATPCS tds =
   let   atpcsAttr (size,zz) td =
               let   ntd =
                        case (alignmentOf td, offset) of
                           (_, 0) -> td
                           (_,off) -> td `addAttr` (StructATPCSAttr $ StructATPCS off (memberName `get` (descAttr td)))
                     offset = structMemberOffset size (alignmentOf td)
                     nsize = structSizeAccum size (alignmentOf td) (sizeOf td)
               in    (nsize, ntd:zz)
         is' (StructATPCSAttr _) = True
         is'  _ = False
         rm' td = td { descAttr = filter (not . is') (descAttr td) }
   in   (reverse $ snd $ foldl atpcsAttr (0, []) $ map rm' tds)

maybeList :: [Maybe a] -> Maybe [a]
maybeList lst =
   case (filter isJust lst) of
      [] -> Nothing
      some -> Just $ map fromJust some

ptrSize :: Int
ptrSize = primSize $ SignedLongType

intSize :: Int
intSize = primSize $ SignedLongType

listify :: (Data a1, Typeable b) => (b -> [a]) -> a1 -> [a]
listify ff lst = everything (++) ([] `mkQ` ff) lst

remAttr::TypeDesc -> Attribute -> TypeDesc
remAttr td attr = td { descAttr = filter (not . ((==) attr)) (descAttr td) }

singleton :: Show a => String -> [a] -> a
singleton _ [a] = a
singleton err [] = error (err ++ ": empty")
singleton err ll = error (err ++ ": to many options:" ++ (show ll))

get :: (Typeable b, Typeable a, Show a) => (b -> a) -> [Attribute] -> a
get ff attr = singleton ("internal error: get failed to find a unique element: "
                      ++ (show $ T.typeOf ff)
                      ++ ": "
                      ++ (show $ attr)
                        ) $ listify (\ aa -> [ff aa]) attr

query :: (Typeable b) => (b -> a) -> [Attribute] -> Maybe a
query ff attr = listToMaybe $ listify (\ aa -> [ff aa]) attr

is :: (Typeable b, Eq b) => b -> [Attribute] -> Bool
is ct attr = or $ listify (\ aa -> [ct == aa]) attr

rm :: (Typeable b, Eq b) => b -> [Attribute] -> [Attribute]
rm ct attr =
   let   nota aa
            | ct `is` [aa] = []
            | otherwise = [aa]
   in    listify nota attr

find :: (Typeable b, Eq b) => b -> [Attribute] -> Maybe b
find ct attr =
   let   find' aa
            | aa == ct = [ct]
            | otherwise = []
   in    listToMaybe $ listify find' attr

derivedMethods :: Interface -> [(Interface, [Method])]
derivedMethods (Interface _ Nothing _ _) = []
derivedMethods (Interface _ (Just ii@(Interface _ _ _ mms)) _ _) = (ii,mms):(derivedMethods ii)

derive :: Interface -> (Interface, [Method]) -> [Method]
derive (Interface iface _ itn _) (_,mms) =
   let
         derive' (Method cm mty mtn tds (_:params)) =
            let mn = head $ fromTypeName mtn
                nmn = TypeName $ mn : fromTypeName itn
            in  setMethodATPCS $ Method cm mty nmn tds ((Parameter iface):params)
         derive' mm = mm
   in    map derive' mms

setMethodATPCS :: Method -> Method
setMethodATPCS (Method cm mty fqn rv params) =
   let   pps = map fromParameter $ params
   in    Method cm mty fqn rv (map Parameter $ setParamATPCS $ setPosition' $ pps)

isArg :: Parameter -> Bool
isArg pp = isArg' $ fromParameter pp

isArg' :: TypeDesc -> Bool
isArg' pp = not (NotArg `is` (descAttr pp))

maybeHead :: a -> [a] -> a
maybeHead def ll = fromMaybe def $ listToMaybe ll

typeDefNames :: [Attribute] -> [TypeName]
typeDefNames attrs
   | (isNothing $ fromTypeDef `query` (attrs)) = []
   | otherwise = fromTypeDef `get` attrs

filterIncluded :: [FilePath] -> [Declaration] -> ([FilePath],[Declaration])
filterIncluded myfiles decs =
   let
         myfiles' = map (takeBaseName) myfiles
         sources = listify (\ aa -> [takeBaseName $ includePath aa]) decs
         incFile aa
            | null myfiles' = False
            | aa `elem` myfiles'  = False
            | otherwise = True
         includes = filter incFile sources
         incdb = includes
         decs' = filter (not . isIncluded incdb . includePath . decIncPath) decs
         incs' = nubBy ((==) `on` takeBaseName)  $ filter (not . null) includes
   in    (incs', decs')


isIncluded :: [FilePath] -> FilePath -> Bool
isIncluded includes ii = (takeBaseName ii) `elem` includes

--instance Show TypeDesc where
--   show td = "TypeDesc " ++ (show (descId td)) ++ " " ++ (render $ typeName (Cfg 0) td)

isCompatibleType :: Type -> Bool
isCompatibleType (Object {}) =  False
isCompatibleType aa =
   let is' (TypeDesc _ _ (Object {})) = [False]
       is' (TypeDesc _ attr _)
         | isJust (seqLen `query` attr) = [False]
         | otherwise = [True]
   in and $ listify is' aa

isCompatibleTypeDesc :: TypeDesc -> Bool
isCompatibleTypeDesc aa =
   let is' (TypeDesc _ _ (Object {})) = [False]
       is' (TypeDesc _ attr _)
         | isJust (seqLen `query` attr) = [False]
         | otherwise = [True]
   in and $ listify is' aa


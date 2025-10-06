-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

-- | A module for converting the IDL datatype to the Palette datatype.
module Language.Idl.ToPalette where

import qualified Language.Idl.Data as Idl
import Language.Palette.Data
import Text.ParserCombinators.Parsec.Pos(
     SourcePos
   , newPos
   )
import Text.ParserCombinators.Parsec.Error(
     newErrorMessage
   , Message(Message)
   )
import Data.List(
     intercalate
   , inits
   , elemIndex
   , delete
   )
import System.FilePathUtils(
     forceForwardSlashes
   )
import Data.Word(
     Word8
   , Word16
   , Word32
   , Word64
   )
import Data.Int(
     Int8
   , Int16
   , Int32
   , Int64
   )
import Data.Maybe(
     fromJust
   , isJust
   )
import Language.Palette.Alignment(
     size
   , structAlignReq
   , unionAlignReq
   , unionCaseSize
   )
import Control.Arrow(
     second
   )
import Control.Monad.Error(runErrorT)
import Control.Monad(
     liftM
   , when
   , foldM
   )
import Control.MonadUtils(
     concatMapM
   , mapAccumM
   )
import Data.Bits(
     shiftL
   , shiftR
   , complement
   , xor
   , (.|.)
   , (.&.)
   )

type Dictionaries                    = (TypeDictionary, ConstDictionary, InterfaceDictionary, Int32)
type TypeDictionary                  = Dictionary Type
type ConstDictionary                 = Dictionary (Type, Literal)
type InterfaceDictionary             = Dictionary (Bool, [Declaration])
type Dictionary a                    = [(Idl.ScopedName, a)]


-- | Converts the IDL datatype to the Palette datatype.  If no errors occur,
--   the IDL document is considered to be "well-formed."
--
--   How it works:
--   As the Idl data structure is traversed, it attempts to inline
--   any references to other parts of Idl.  When it finds type, constant, and
--   interface declarations, it tucks them away in dictionaries.
--   Then, as TypeRef's, ConstExprRef's, and
--   interface base classes are encountered, they are looked up in the
--   dictionaries using the scoping rules defined by OMG IDL.
--   If successful, the references is replaced by its value.
paletteFromIdl                      :: Monad m
                                    => Bool
                                    -> [FilePath]
                                    -> Maybe String
                                    -> Idl.Idl
                                    -> m Palette

paletteFromIdl warnUndefs filepaths rootIface (Idl.Idl topDecls)
                                     = do
                                          ((tys,_,ifaces,_), _:inlinedDeclss) <- declarations [] rootIface ([],[],[],1) [] decls
                                          when warnUndefs (validateForwardDeclarations ifaces tys)
                                          let pathsAndInlined  =  zip paths inlinedDeclss
                                              localDeclss      =  [declss | (declPath, declss) <- pathsAndInlined
                                                                  , null filepaths || declPath `elem` filepaths'
                                                                  ]
                                          return (Palette (concat localDeclss))
   where
      (paths, decls)                 = unzip [(path, decl) | Idl.TopLevelDeclaration path decl@(Idl.Declaration{}) <- topDecls]
      filepaths'                     = map forceForwardSlashes filepaths

idlToDictionaries :: Monad m => Maybe String -> Idl.Idl -> m Dictionaries
idlToDictionaries rootIface (Idl.Idl topDecls) = liftM fst $ declarations [] rootIface ([],[],[],1) [] decls
   where
      (_, decls)                 = unzip [(path, decl) | Idl.TopLevelDeclaration path decl <- topDecls]

-- | Verify that all forward-declared interfaces have been defined
validateForwardDeclarations         :: Monad m
                                    => InterfaceDictionary
                                    -> TypeDictionary
                                    -> m ()

validateForwardDeclarations is ts    = mapM_ die undefinedIfaces
   where
                                       -- TODO: issue a warning instead of error
      die nm                         = fail ("error: forward-declared interface '"++(intercalate "::" nm)++"' was never defined")
      undefinedIfaces                = filter isUndefined ifaceTypes
      isUndefined fName              = null [nm | (nm, _) <- is, fName == nm]
      ifaceTypes                     =      [nm | (nm, Interface{}) <- ts]

declarations                        :: Monad m
                                    => Idl.ScopedName
                                    -> Maybe String
                                    -> Dictionaries
                                    -> [Declaration]
                                    -> [Idl.Declaration]
                                    -> m (Dictionaries, [[Declaration]])

declarations scope rootIface acc parentDs ds
                                     = do
                                          acc'         <- prependDecls scope acc parentDs'
                                          (acc'',dss') <- mapAccumM (declaration scope rootIface) acc' ds
                                          return (acc'', parentDs':dss')
   where
      -- Remove duplicate definitions from parent if OMG IDL says they are save to shadow.
      -- If they aren't safe to shadow, leave them in and the user will get an error
      -- when that duplicate is resolved.
      parentDs'                      = [dcl | dcl@(Declaration nm def) <- parentDs
                                            , nm `notElem` names || isIllegal def]
      names                          = [nm  | Idl.Declaration _ _ _ (Idl.Identifier nm) _ <- ds]

      -- OMG IDL 3.8.5: A derived interface may redefine any of the type,
      --                constant and exception names that have been inherited.
      isIllegal                     :: Definition -> Bool
      isIllegal (TypeDcl{})          = False
      isIllegal (ConstDcl{})         = False
      isIllegal _                    = True

-- | Push each declaration into the appropriate dictionary
prependDecls                        :: Monad m
                                    => Idl.ScopedName
                                    -> Dictionaries
                                    -> [Declaration]
                                    -> m Dictionaries

prependDecls                         = foldM . prependDecl


-- | Push a declaration into the appropriate dictionary
prependDecl                         :: Monad m
                                    => Idl.ScopedName
                                    -> Dictionaries
                                    -> Declaration
                                    -> m Dictionaries

prependDecl scope acc (Declaration nm def)
                                     = prependDef (newPos "" 0 0) scope nm acc def


-- | Push a defintion into the appropriate dictionary
prependDef                          :: Monad m
                                    => SourcePos
                                    -> Idl.ScopedName
                                    -> String
                                    -> Dictionaries
                                    -> Definition
                                    -> m Dictionaries

prependDef pos scope nm (ts,cs,is,u) (TypeDcl ty)
                                     = do
                                          ts' <- scopedInsert (ts,cs,is) pos ts scope nm ty
                                          return (ts', cs, is, u)
prependDef pos scope nm (ts,cs,is,u) (OperationDcl _ _)
                                     = do
                                          ts' <- scopedInsert (ts,cs,is) pos ts scope nm OperationType
                                          return (ts', cs, is, u)
prependDef pos scope nm (ts,cs,is,u) (ConstDcl ty lit)
                                     = do
                                          cs' <- scopedInsert (ts,cs,is) pos cs scope nm (ty,lit)
                                          return (ts, cs', is, u)
prependDef pos scope nm (ts,cs,is,u) (InterfaceDcl isLocal _ ds)
                                     = do
                                          is' <- scopedInsert (ts,cs,is) pos is scope nm (isLocal, ds)
                                          return (ts, cs, is', u)
prependDef _ _ _ xs (ModuleDcl _)    = return xs



declaration                         :: Monad m
                                    => Idl.ScopedName
                                    -> Maybe String
                                    -> Dictionaries
                                    -> Idl.Declaration
                                    -> m (Dictionaries, [Declaration])

declaration _ (Just rootNm) _ (Idl.Declaration _ _ _ (Idl.Identifier nm) (Idl.InterfaceDcl _ False False _ (pos, Nothing) _))
                                       -- Match against a non-local interface with no base
                                       -- interface that is not the root interface.
                                       -- All forward declarations, local interfaces,
                                       -- and derived interfaces may pass.
   | nm /= rootNm                    = failAtPosition pos ("interface '"++nm++"' must either be local or inherit from '"++rootNm++"'")
declaration scope rootIface acc (Idl.Declaration pos _ _ (Idl.Identifier nm) def)
                                     = do
                                          (acc', def') <- definition acc pos scope nm rootIface def
                                          return (acc', [Declaration nm def'])
declaration _ _ acc (Idl.CommentBlock _ )
                                     = return (acc, [])



definition                          :: Monad m
                                    => Dictionaries
                                    -> SourcePos
                                    -> Idl.ScopedName
                                    -> String
                                    -> Maybe String
                                    -> Idl.Definition
                                    -> m (Dictionaries, Definition)

definition (types,consts,ifaces,uniq) _pos scope nm rootIface (Idl.InterfaceDcl _ isLocal isForward iid (pos,mayBase) ds)
                                     = do
                                          let scope' = scope++[nm]
                                          (isParentLocal, parentDecls) <- maybe (return (False,[])) (scopedLookup pos ifaces scope') mayBase
                                          when (isParentLocal && not isLocal) (failAtPosition pos "nonlocal interface cannot be derived from a local interface")
                                          let
                                              fromRight :: Either String a -> Maybe a
                                              fromRight (Left _) = Nothing
                                              fromRight (Right rr) = Just rr
                                              resolveIId Nothing = return Nothing
                                              resolveIId (Just ii) = liftM fromRight $ runErrorT $ liftM snd $ uintExpr consts scope' ii
                                          iid' <- resolveIId iid
                                          let forwardDecl = Interface 0 False iid'
                                          let types'   = delete (scope', forwardDecl) types -- Overwrite forward-declarations
                                          types''     <- scopedInsert (types',consts,ifaces) pos types' scope nm forwardDecl
                                          ((types''',consts',ifaces',uniq'),dss')
                                                      <- declarations scope' rootIface (types'',consts,ifaces,uniq) parentDecls ds
                                          let ds'      = concat dss'
                                          ifaces''    <- if isForward
                                                            then return ifaces'
                                                            else do
                                                               let types'''' = delete (scope', forwardDecl) types''' -- Delete our forward-declaration
                                                               scopedInsert (types'''',consts',ifaces') pos ifaces' scope nm (isLocal, ds')
                                          let acc      = (types''', consts', ifaces'', uniq')
                                          return $ (acc, InterfaceDcl isLocal iid' ds')

definition (types,consts,ifaces,uniq) pos scope nm _ (Idl.TypeDcl ty)
                                     = do
                                          let scope' = scope++[nm]
                                          recTypes <- scopedInsert (types,consts,ifaces) pos types scope nm (RecursiveTypeRef uniq)
                                          ty' <- inlineType (recTypes,consts) scope' ty
                                          let ty'' = if isRecursive uniq ty' then TypeLambda uniq ty' else ty'
                                          let types' = case ty'' of
                                                          Interface 0 False (Just iid')
                                                             -> delete (scope', Interface 0 False (Just iid')) types -- Overwrite forward-declarations
                                                          _  -> types
                                          types'' <- scopedInsert (types',consts,ifaces) pos types' scope nm ty''
                                          consts' <- case ty of
                                                          Idl.Enum _ xs
                                                             -> enumConsts scope pos (types'',consts,ifaces) (map snd xs)
                                                          _  -> return consts
                                          return ((types'',consts',ifaces,uniq+1), TypeDcl ty'')

definition (types,consts,ifaces,uniq) pos scope nm _ (Idl.ConstDcl ty expr)
                                     = do
                                          let scope' = scope++[nm]
                                          ty' <- inlineType (types,consts) scope' ty
                                          lit <- constExpr consts scope' ty' expr
                                          consts' <- scopedInsert (types,consts,ifaces) pos consts scope nm (ty',lit)
                                          return ((types, consts',ifaces,uniq), ConstDcl ty' lit)

definition (types,consts,ifaces,uniq) pos scope nm _ (Idl.OperationDcl _ mayOp _ returnType ps)
                                     = do
                                          let scope' = scope++[nm]
                                          _      <- inlineType (types,consts) scope' returnType
                                          ps'    <- concatMapM (parameter (types,consts) scope') ps
                                          opAttr <- maybe (return OpAttrSync) (opAttribute ps') mayOp
                                          types' <- scopedInsert (types,consts,ifaces) pos types scope nm OperationType
                                          return ((types',consts,ifaces,uniq), OperationDcl opAttr ps')
definition acc _pos scope nm rootIface (Idl.ModuleDcl ds)
                                     = do
                                          (acc', dss') <- declarations (scope++[nm]) rootIface acc [] ds
                                          return (acc', ModuleDcl (concat dss'))


opAttribute                         :: Monad m
                                    => [Parameter]
                                    -> (SourcePos, Idl.OpAttr)
                                    -> m OpAttr

opAttribute ps (p, Idl.Identifier attr)
   | any isOut ps                        = failAtPosition p ("'" ++ attr ++ "' method may only have [In] parameters")
   where
      isOut (Parameter _ ParameterInROut _) = True
      isOut (Parameter _ ParameterROut _) = True
      isOut _                            = False

opAttribute _  (_, Idl.Identifier "signal") = return OpAttrSignal
opAttribute _  (_, Idl.Identifier "oneway") = return OpAttrOneway
opAttribute _  (p, _)                        = failAtPosition p "unrecognized method attribute"


isRecursive                         :: Int32 -> Type -> Bool
isRecursive fid (RecursiveTypeRef recFid)
                                     = fid == recFid
isRecursive fid (Struct _ ms)        = any (isMemberRecursive fid) ms
isRecursive fid (Union _ _ _ cs mayDef)
                                     = any (isMemberRecursive fid . snd) cs || maybe False (isMemberRecursive fid) mayDef
isRecursive fid (Sequence _ _ (Buffer _ ty))
                                     = isRecursive fid ty
isRecursive fid (Array _ ty)         = isRecursive fid ty
isRecursive _ _                      = False

isMemberRecursive                   :: Int32 -> Member -> Bool
isMemberRecursive fid (Member _ ty)  = isRecursive fid ty


literal                             :: Monad m
                                    => SourcePos
                                    -> Type
                                    -> Idl.Literal
                                    -> m Literal

literal pos (PrimType prim) lit      = primLiteral pos prim lit
literal _ (Sequence StringSeq _ _) (Idl.StringLiteral str)
                                     = return (StringLiteral str)
literal _ (Sequence WideStringSeq _ _) (Idl.WStringLiteral str)
                                     = return (WStringLiteral str)
literal pos ty lit                   = typeMismatch pos (typeName ty) (literalName lit)


primLiteral                        :: Monad m
                                    => SourcePos
                                    -> Prim
                                    -> Idl.Literal
                                    -> m Literal

primLiteral _ BooleanType          (Idl.BoolLiteral b)      = return (BoolLiteral b)
primLiteral _ CharType             (Idl.CharLiteral ch)     = return (CharLiteral ch)
primLiteral _ WideCharType         (Idl.WCharLiteral ch)    = return (WCharLiteral ch)
primLiteral pos OctetType            (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . OctetLiteral)            (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedShortType    (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedShortLiteral)    (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedLongType     (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedLongLiteral)     (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedLongLongType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedLongLongLiteral) (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedShortType      (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedShortLiteral)     (boundsCheckIntM pos minBound maxBound (mkSigned16 i))
primLiteral pos SignedShortType      (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedShortLiteral)      (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedLongType       (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedLongLiteral)      (boundsCheckIntM pos minBound maxBound (mkSigned32 i))
primLiteral pos SignedLongType       (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedLongLiteral)       (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedCharFixedTType    (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . OctetLiteral)            (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedCharFixedType     (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . OctetLiteral)            (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedShortFixedTType   (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedShortLiteral)    (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedShortFixedType    (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedShortLiteral)    (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedLongFixedTType    (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedLongLiteral)     (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedLongFixedType     (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedLongLiteral)     (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedLongLongFixedTType(Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedLongLongLiteral) (boundsCheckIntM pos minBound maxBound i)
primLiteral pos UnsignedLongLongFixedType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . UnsignedLongLongLiteral) (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedCharFixedTType (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . OctetLiteral)           (boundsCheckIntM pos minBound maxBound (mkSigned8 i))
primLiteral pos SignedCharFixedTType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . OctetLiteral)            (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedCharFixedType  (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . OctetLiteral)           (boundsCheckIntM pos minBound maxBound (mkSigned8 i))
primLiteral pos SignedCharFixedType  (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . OctetLiteral)            (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedShortFixedTType(Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedShortLiteral)     (boundsCheckIntM pos minBound maxBound (mkSigned16 i))
primLiteral pos SignedShortFixedTType(Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedShortLiteral)      (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedShortFixedType (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedShortLiteral)     (boundsCheckIntM pos minBound maxBound (mkSigned16 i))
primLiteral pos SignedShortFixedType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedShortLiteral)      (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedLongFixedTType (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedLongLiteral)      (boundsCheckIntM pos minBound maxBound (mkSigned32 i))
primLiteral pos SignedLongFixedTType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedLongLiteral)       (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedLongFixedType  (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedLongLiteral)      (boundsCheckIntM pos minBound maxBound (mkSigned32 i))
primLiteral pos SignedLongFixedType  (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedLongLiteral)       (boundsCheckIntM pos minBound maxBound i)
primLiteral _ (EnumType es)        (Idl.EnumLiteral str)    | str `elem` es
                                                            = return (IntLiteral Dec (SignedLongLiteral (fromIntegral (fromJust (str `elemIndex` es)))))
primLiteral pos SignedLongLongType (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedLongLongLiteral)    (boundsCheckIntM pos minBound maxBound (mkSigned64 i))
primLiteral pos SignedLongLongType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedLongLongLiteral)     (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedLongLongFixedTType (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedLongLongLiteral)    (boundsCheckIntM pos minBound maxBound (mkSigned64 i))
primLiteral pos SignedLongLongFixedTType (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedLongLongLiteral)     (boundsCheckIntM pos minBound maxBound i)
primLiteral pos SignedLongLongFixedType  (Idl.IntLiteral Hex i) = liftM (IntLiteral Hex . SignedLongLongLiteral)    (boundsCheckIntM pos minBound maxBound (mkSigned64 i))
primLiteral pos SignedLongLongFixedType  (Idl.IntLiteral bs i)  = liftM (IntLiteral bs . SignedLongLongLiteral)     (boundsCheckIntM pos minBound maxBound i)
primLiteral _ FloatType            (Idl.FloatLiteral a b c) = return (RealLiteral (FloatLit  (read (a++"."++b++"e"++c))))
primLiteral _ DoubleType           (Idl.FloatLiteral a b c) = return (RealLiteral (DoubleLit (read (a++"."++b++"e"++c))))
primLiteral pos ty lit                                      = typeMismatch pos (primTypeName ty) (literalName lit)

mkSigned8                          :: Integer -> Integer
mkSigned8 i
   | i8 < 0 && i <= u8Max          = fromIntegral i8
   | otherwise                       = i
   where
      u8Max                         = fromIntegral (maxBound :: Word8)
      i8                            = fromIntegral i :: Int8

mkSigned16                          :: Integer -> Integer
mkSigned16 i
   | i16 < 0 && i <= u16Max          = fromIntegral i16
   | otherwise                       = i
   where
      u16Max                         = fromIntegral (maxBound :: Word16)
      i16                            = fromIntegral i :: Int16

mkSigned32                          :: Integer -> Integer
mkSigned32 i
   | i32 < 0 && i <= u32Max          = fromIntegral i32
   | otherwise                       = i
   where
      u32Max                         = fromIntegral (maxBound :: Word32)
      i32                            = fromIntegral i :: Int32

mkSigned64                          :: Integer -> Integer
mkSigned64 i
   | i64 < 0 && i <= u64Max          = fromIntegral i64
   | otherwise                       = i
   where
      u64Max                         = fromIntegral (maxBound :: Word64)
      i64                            = fromIntegral i :: Int64

boundsCheckIntM                     :: (Show a, Integral a, Monad m) => SourcePos -> a -> a -> Integer -> m a
boundsCheckIntM pos low high i       = either (failAtPosition pos) return (boundsCheckInt low high i)


boundsCheckInt                      :: (Show a, Integral a) => a -> a -> Integer -> Either String a
boundsCheckInt low high i
      | i < fromIntegral low         = Left ("Integer \""++show i++"\" is below the lower bound, "++show low ++", allowed by type.")
      | i > fromIntegral high        = Left ("Integer \""++show i++"\" is above the upper bound, "++show high++", allowed by type.")
      | otherwise                    = Right (fromInteger i)


typeMismatch                        :: Monad m
                                    => SourcePos
                                    -> String
                                    -> String
                                    -> m a

typeMismatch pos tyNm litNm          = failAtPosition pos ("type mismatch, expected \""++tyNm++"\" but got \""++litNm++"\"")

typeName                            :: Type -> String
typeName Void                        = "void"
typeName (PrimType prim)             = primTypeName prim
typeName (Struct _ _)                = "struct"
typeName (Union _ _ _ _ _)           = "union"
typeName (Sequence Seq _ _)          = "sequence"
typeName (Sequence StringSeq _ _)    = "string"
typeName (Sequence WideStringSeq _ _)= "wstring"
typeName (SequenceConstraint Seq _ _ _)
                                     = "sequence constraint"
typeName (SequenceConstraint StringSeq _ _ _)
                                     = "string constraint"
typeName (SequenceConstraint WideStringSeq _ _ _)
                                     = "wstring constraint"
typeName (TaggedSequence Seq _ _)    = "rout sequence"
typeName (TaggedSequence StringSeq _ _)
                                     = "rout string"
typeName (TaggedSequence WideStringSeq _ _)
                                     = "rout wstring"
typeName (Array _ _)                 = "array"
typeName (Interface _ False _)       = "interface"
typeName (Interface _ True _)        = "notnil interface"
typeName (InterfaceConstraint _)     = "interface constraint"
typeName (Native nm)                 = nm
typeName (OperationType)             = "function"
typeName (TypeLambda _ _)            = "recursive type"
typeName (RecursiveTypeRef _)        = "recursive type reference"
typeName (TypeRef _)                 = "type reference"

primTypeName                        :: Prim -> String
primTypeName BooleanType             = "boolean"
primTypeName OctetType               = "octet"
primTypeName UnsignedShortType       = "unsigned short"
primTypeName UnsignedLongType        = "unsigned long"
primTypeName UnsignedLongLongType    = "unsigned long long"
primTypeName SignedShortType         = "short"
primTypeName SignedLongType          = "long"
primTypeName SignedLongLongType      = "long long"
primTypeName UnsignedCharFixedTType       = "uint8_t"
primTypeName UnsignedShortFixedTType      = "uint16_t"
primTypeName UnsignedLongFixedTType       = "uint32_t"
primTypeName UnsignedLongLongFixedTType   = "uint64_t"
primTypeName SignedCharFixedTType         = "int8_t"
primTypeName SignedShortFixedTType        = "int16_t"
primTypeName SignedLongFixedTType         = "int32_t"
primTypeName SignedLongLongFixedTType     = "int64_t"
primTypeName UnsignedCharFixedType       = "uint8"
primTypeName UnsignedShortFixedType      = "uint16"
primTypeName UnsignedLongFixedType       = "uint32"
primTypeName UnsignedLongLongFixedType   = "uint64"
primTypeName SignedCharFixedType         = "int8"
primTypeName SignedShortFixedType        = "int16"
primTypeName SignedLongFixedType         = "int32"
primTypeName SignedLongLongFixedType     = "int64"
primTypeName (EnumType ms)           = "member from enum: [" ++ intercalate ", " ms ++ "]"
primTypeName FloatType               = "float"
primTypeName DoubleType              = "double"
primTypeName CharType                = "char"
primTypeName WideCharType            = "wchar"

literalName                         :: Idl.Literal -> String
literalName (Idl.BoolLiteral _)      = "boolean"
literalName (Idl.CharLiteral _)      = "char"
literalName (Idl.WCharLiteral _)     = "wchar"
literalName (Idl.StringLiteral _)    = "string"
literalName (Idl.WStringLiteral _)   = "wstring"
literalName (Idl.IntLiteral _ _)     = "number"
literalName (Idl.FloatLiteral _ _ _) = "float or double"
literalName (Idl.EnumLiteral _)      = "enum member"


constExpr                           :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> Idl.ConstExpr
                                    -> m Literal

constExpr _      _     ty (Idl.ConstExpr pos (Idl.LiteralExpr x))     = literal pos ty x
constExpr consts scope ty (Idl.ConstExpr pos (Idl.ConstExprRef n))    = scopedLookup pos consts scope n >>= coerce pos ty
constExpr consts scope ty@(PrimType FloatType)  (Idl.ConstExpr pos x) = foldFloat consts scope ty pos x
constExpr consts scope ty@(PrimType DoubleType) (Idl.ConstExpr pos x) = foldFloat consts scope ty pos x
constExpr consts scope ty (Idl.ConstExpr pos x)                       = foldInteger consts scope ty pos x


foldInteger                         :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> SourcePos
                                    -> Idl.Const
                                    -> m Literal

foldInteger consts scope ty pos (Idl.Add a b)        = binary pos consts scope ty (+) a b
foldInteger consts scope ty pos (Idl.Sub a b)        = binary pos consts scope ty (-) a b
foldInteger consts scope ty pos (Idl.Mul a b)        = binary pos consts scope ty (*) a b
foldInteger consts scope ty pos (Idl.Div a b)        = binary pos consts scope ty div a b
foldInteger consts scope ty pos (Idl.Modulo a b)     = binary pos consts scope ty mod a b
foldInteger consts scope ty pos (Idl.LeftShift a b)  = shift pos consts scope ty shiftL a b
foldInteger consts scope ty pos (Idl.RightShift a b) = shift pos consts scope ty shiftR a b
foldInteger _ _ (PrimType prim) pos (Idl.Negate (Idl.ConstExpr _ (Idl.LiteralExpr (Idl.IntLiteral bs i))))
                                                     = primLiteral pos prim (Idl.IntLiteral bs (-i))
foldInteger consts scope ty pos (Idl.Negate a)       = unary  pos consts scope ty negate a
foldInteger consts scope ty pos (Idl.Complement a)   = unary  pos consts scope ty complement a
foldInteger consts scope ty pos (Idl.BitwiseAnd a b) = binary pos consts scope ty (.&.) a b
foldInteger consts scope ty pos (Idl.BitwiseOr a b)  = binary pos consts scope ty (.|.) a b
foldInteger consts scope ty pos (Idl.BitwiseXor a b) = binary pos consts scope ty xor a b
foldInteger _ _ _ _ (Idl.LiteralExpr _)              = error "internal error: unexpected LiteralExpr in foldInteger"
foldInteger _ _ _ _ (Idl.ConstExprRef _)             = error "internal error: unexpected ConstExprRef in foldInteger"


foldFloat                           :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> SourcePos
                                    -> Idl.Const
                                    -> m Literal

foldFloat consts scope ty pos (Idl.Add a b)  = fbinary pos consts scope ty (+) a b
foldFloat consts scope ty pos (Idl.Sub a b)  = fbinary pos consts scope ty (-) a b
foldFloat consts scope ty pos (Idl.Mul a b)  = fbinary pos consts scope ty (*) a b
foldFloat consts scope ty pos (Idl.Div a b)  = fbinary pos consts scope ty (/) a b
foldFloat consts scope ty pos (Idl.Negate a) = funary  pos consts scope ty negate a
foldFloat _ _ _ pos (Idl.Modulo _ _)         = failAtPosition pos "type mismatch: A floating point number cannot be passed to the modulo operator."
foldFloat _ _ _ pos (Idl.LeftShift _ _)      = failAtPosition pos "type mismatch: A floating point number cannot be passed to the bitwise left shift operator."
foldFloat _ _ _ pos (Idl.RightShift _ _)     = failAtPosition pos "type mismatch: A floating point number cannot be passed to the bitwise right shift operator."
foldFloat _ _ _ pos (Idl.Complement _)       = failAtPosition pos "type mismatch: The bitwise complement operator cannot operate on floating point numbers."
foldFloat _ _ _ pos (Idl.BitwiseAnd _ _)     = failAtPosition pos "type mismatch: A floating point number cannot be passed to the bitwise and operator."
foldFloat _ _ _ pos (Idl.BitwiseOr _ _)      = failAtPosition pos "type mismatch: A floating point number cannot be passed to the bitwise or operator."
foldFloat _ _ _ pos (Idl.BitwiseXor _ _)     = failAtPosition pos "type mismatch: A floating point number cannot be passed to the bitwise exclusive or operator."
foldFloat _ _ _ _ (Idl.LiteralExpr _)        = error "internal error: unexpected LiteralExpr in foldInteger"
foldFloat _ _ _ _ (Idl.ConstExprRef _)       = error "internal error: unexpected ConstExprRef in foldInteger"

fbinary                             :: (Monad m)
                                    => SourcePos
                                    -> ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> (Double -> Double -> Double)
                                    -> Idl.ConstExpr
                                    -> Idl.ConstExpr
                                    -> m Literal

fbinary pos consts scope ty f a b    = do
                                          aDoub <- doubleExpr consts scope ty a
                                          bDoub <- doubleExpr consts scope ty b
                                          r     <- fromDoubleM pos ty (f aDoub bDoub)
                                          return (RealLiteral r)

doubleExpr                         :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> Idl.ConstExpr
                                    -> m Double

doubleExpr consts scope ty x@(Idl.ConstExpr pos _)
                                     = constExpr consts scope ty x >>= unlit
   where
      unlit (RealLiteral (FloatLit i))  = return (realToFrac i)
      unlit (RealLiteral (DoubleLit i)) = return i
      unlit _                           = failAtPosition pos "Non-float encountered during constant folding when only floats expected"

-- | Morph from Double if possible
fromDoubleM                         :: Monad m
                                    => SourcePos
                                    -> Type
                                    -> Double
                                    -> m RealLit

fromDoubleM _ (PrimType FloatType)  i = return (FloatLit (fromRational (toRational i)))
fromDoubleM _ (PrimType DoubleType) i = return (DoubleLit i)
fromDoubleM pos ty                  _ = typeMismatch pos (typeName ty) "floating point number"


funary                              :: (Monad m)
                                    => SourcePos
                                    -> ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> (Double -> Double)
                                    -> Idl.ConstExpr
                                    -> m Literal

funary pos consts scope ty f a       = do
                                          aDoub <- doubleExpr consts scope ty a
                                          r     <- fromDoubleM pos ty (f aDoub)
                                          return (RealLiteral r)


shift                              :: (Monad m)
                                    => SourcePos
                                    -> ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> (Integer -> Int -> Integer)
                                    -> Idl.ConstExpr
                                    -> Idl.ConstExpr
                                    -> m Literal

shift pos consts scope ty f a b      = do
                                          (aBase, aInt) <- integerExpr consts scope ty a
                                          (_    , bInt) <- integerExpr consts scope ty b
                                          bInt'         <- boundsCheckIntM pos 0 maxBound bInt
                                          r             <- fromIntegerM pos ty (f aInt bInt')
                                          return (IntLiteral aBase r)

-- | Morph to requested type if possible
coerce                              :: Monad m
                                    => SourcePos
                                    -> Type
                                    -> (Type, Literal)
                                    -> m Literal

coerce _ requestedType (ty,lit)
   | requestedType == ty             = return lit
coerce pos requestedType (ty@(PrimType (EnumType _)), _)
                                     = typeMismatch pos (typeName requestedType) (typeName ty)
coerce pos ty (_, IntLiteral bs lit) = liftM (IntLiteral bs) (fromIntegerM pos ty (intLitToInteger lit))
coerce pos requestedType (ty,_)      = typeMismatch pos (typeName requestedType) (typeName ty)


binary                              :: (Monad m)
                                    => SourcePos
                                    -> ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> (Integer -> Integer -> Integer)
                                    -> Idl.ConstExpr
                                    -> Idl.ConstExpr
                                    -> m Literal

binary pos consts scope ty f a b     = do
                                          (aBase, aInt) <- integerExpr consts scope ty a
                                          (_    , bInt) <- integerExpr consts scope ty b

                                          -- 'fromIntegerM' boundsChecks every subexpression, which
                                          -- is a bit more strict than necessary (when compile-time
                                          -- performance doesn't matter).  For example, you can't do this:
                                          --
                                          --    unsigned long a = -1 + 1;
                                          --
                                          -- But the restriction is implemented because OMG says we should:
                                          --
                                          -- OMG IDL 3.10.2: "It is an error if any subexpression
                                          --                  values exceed the precision of
                                          ---                 the assigned type"
                                          r             <- fromIntegerM pos ty (f aInt bInt)
                                          return (IntLiteral aBase r)


unary                               :: (Monad m)
                                    => SourcePos
                                    -> ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> (Integer -> Integer)
                                    -> Idl.ConstExpr
                                    -> m Literal

unary pos consts scope ty f a        = do
                                          (aBase, aInt) <- integerExpr consts scope ty a
                                          r             <- fromIntegerM pos ty (f aInt)
                                          return (IntLiteral aBase r)


-- | Morph from Integer if possible
fromIntegerM                        :: Monad m
                                    => SourcePos
                                    -> Type
                                    -> Integer
                                    -> m IntegerLiteral

fromIntegerM pos (PrimType OctetType)            i = liftM OctetLiteral            (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedShortType)    i = liftM UnsignedShortLiteral    (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedLongType)     i = liftM UnsignedLongLiteral     (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedLongLongType) i = liftM UnsignedLongLongLiteral (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedShortType)      i = liftM SignedShortLiteral      (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedLongType)       i = liftM SignedLongLiteral       (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedLongLongType)   i = liftM SignedLongLongLiteral   (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedCharFixedTType)     i = liftM UnsignedCharLiteral     (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedShortFixedTType)    i = liftM UnsignedShortLiteral    (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedLongFixedTType)     i = liftM UnsignedLongLiteral     (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedLongLongFixedTType) i = liftM UnsignedLongLongLiteral (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedCharFixedTType)       i = liftM SignedCharLiteral       (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedShortFixedTType)      i = liftM SignedShortLiteral      (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedLongFixedTType)       i = liftM SignedLongLiteral       (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedLongLongFixedTType)   i = liftM SignedLongLongLiteral   (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedCharFixedType)      i = liftM UnsignedCharLiteral     (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedShortFixedType)     i = liftM UnsignedShortLiteral    (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedLongFixedType)      i = liftM UnsignedLongLiteral     (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType UnsignedLongLongFixedType)  i = liftM UnsignedLongLongLiteral (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedCharFixedType)        i = liftM SignedCharLiteral       (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedShortFixedType)       i = liftM SignedShortLiteral      (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedLongFixedType)        i = liftM SignedLongLiteral       (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos (PrimType SignedLongLongFixedType)    i = liftM SignedLongLongLiteral   (boundsCheckIntM pos minBound maxBound i)
fromIntegerM pos ty                              _ = typeMismatch pos (typeName ty) "integer"


-- | Promote IntegerLiteral to Integer
intLitToInteger                            :: IntegerLiteral -> Integer
intLitToInteger (OctetLiteral i)            = fromIntegral i
intLitToInteger (UnsignedCharLiteral i)     = fromIntegral i
intLitToInteger (UnsignedShortLiteral i)    = fromIntegral i
intLitToInteger (UnsignedLongLiteral i)     = fromIntegral i
intLitToInteger (UnsignedLongLongLiteral i) = fromIntegral i
intLitToInteger (SignedCharLiteral i)       = fromIntegral i
intLitToInteger (SignedShortLiteral i)      = fromIntegral i
intLitToInteger (SignedLongLiteral i)       = fromIntegral i
intLitToInteger (SignedLongLongLiteral i)   = fromIntegral i



integerExpr                         :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Type
                                    -> Idl.ConstExpr
                                    -> m (Base, Integer)

integerExpr consts scope ty x@(Idl.ConstExpr pos _)
                                     = constExpr consts scope ty x >>= unlit
   where
      unlit (IntLiteral bs iLit)     = return (bs, (intLitToInteger iLit))
      unlit _                        = failAtPosition pos "Non-integer encountered during constant folding"


intExpr                             :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Int32
                                    -> Idl.ConstExpr
                                    -> m (Base, Int32)

intExpr consts scope low expr@(Idl.ConstExpr pos _)
                                     = do
                                          (bs,i) <- integerExpr consts scope (PrimType SignedLongType) expr
                                          i'     <- boundsCheckIntM pos low maxBound i
                                          return (bs, i')

uintExpr                            :: Monad m
                                    => ConstDictionary
                                    -> Idl.ScopedName
                                    -> Idl.ConstExpr
                                    -> m (Base, Word32)

uintExpr consts scope expr           = constExpr consts scope (PrimType UnsignedLongType) expr >>= unlit
   where
      unlit                         :: Monad m => Literal -> m (Base, Word32)
      unlit (IntLiteral bs (UnsignedLongLiteral i))
                                     = return (bs,i)
      unlit lit                      = error ("Internal error: Expected unsigned long constant but got " ++ show lit)


parameter                           :: Monad m
                                    => (TypeDictionary, ConstDictionary)
                                    -> Idl.ScopedName
                                    -> Idl.Parameter
                                    -> m [Parameter]

parameter acc scope (Idl.Parameter (Idl.Identifier nm) Idl.ParameterIn ty)
                                     = do
                                          ty' <- inlineType acc scope ty
                                          return [Parameter nm ParameterIn ty']
parameter acc scope (Idl.Parameter (Idl.Identifier nm) Idl.ParameterInROut ty)
                                     = do
                                          ty' <- inlineType acc scope ty
                                          return [Parameter nm ParameterInROut ty']
parameter acc scope (Idl.Parameter (Idl.Identifier nm) Idl.ParameterROut ty)
                                     = do
                                          ty' <- inlineType acc scope ty
                                          return [Parameter nm ParameterROut (rout ty')]
inlineNullableType                  :: Monad m
                                    => (TypeDictionary, ConstDictionary)
                                    -> Idl.ScopedName
                                    -> Idl.Type
                                    -> Bool
                                    -> m Type

inlineNullableType acc scope t False = inlineType acc scope t
inlineNullableType acc scope t True  = do
                                          ty <- inlineType acc scope t
                                          case ty of
                                             Interface sz _ iid -> return (Interface sz True iid)
                                             x                  -> return x  -- TODO: 'notnil' is only meaningful for interfaces.  Error message?

inlineType                          :: Monad m
                                    => (TypeDictionary, ConstDictionary)
                                    -> Idl.ScopedName
                                    -> Idl.Type
                                    -> m Type

inlineType _ _ (Idl.PrimType x)      = return (PrimType x)
inlineType _ _ (Idl.Enum _ xs)       = return (PrimType (EnumType (map snd xs)))
inlineType (types,_) scope (Idl.TypeRef pos isNotNil nms)
                                     = do
                                          ty <- scopedLookup pos types scope nms
                                          case ty of
                                             OperationType      -> failAtPosition pos "function references are not supported"
                                             Interface sz _ iid -> return (Interface sz isNotNil iid)
                                             x                  -> if isNotNil
                                                                     then failAtPosition pos "'notnil' keyword may only be used in conjunction with interfaces"
                                                                     else return x
inlineType acc scope (Idl.Struct _ xs)
                                     = do
                                          xs' <- mapM (member acc scope) xs
                                          return (Struct (structAlignReq xs') xs')
inlineType (types,consts) scope (Idl.Union pos _ dis xs mayDef)
                                     = do
                                          dis' <- inlineType (types,consts) scope dis
                                          -- If discriminator is an enum, its members need to be in scope for the union cases
                                          (consts', disPrim) <- case dis' of
                                             PrimType SignedShortType      -> return (consts, SignedShortType)
                                             PrimType SignedLongType       -> return (consts, SignedLongType)
                                             PrimType SignedLongLongType   -> return (consts, SignedLongLongType)
                                             PrimType UnsignedShortType    -> return (consts, UnsignedShortType)
                                             PrimType UnsignedLongType     -> return (consts, UnsignedLongType)
                                             PrimType UnsignedLongLongType -> return (consts, UnsignedLongLongType)
                                             PrimType SignedCharFixedTType       -> return (consts, SignedCharFixedTType)
                                             PrimType SignedShortFixedTType      -> return (consts, SignedShortFixedTType)
                                             PrimType SignedLongFixedTType       -> return (consts, SignedLongFixedTType)
                                             PrimType SignedLongLongFixedTType   -> return (consts, SignedLongLongFixedTType)
                                             PrimType UnsignedCharFixedTType     -> return (consts, UnsignedCharFixedTType)
                                             PrimType UnsignedShortFixedTType    -> return (consts, UnsignedShortFixedTType)
                                             PrimType UnsignedLongFixedTType     -> return (consts, UnsignedLongFixedTType)
                                             PrimType UnsignedLongLongFixedTType -> return (consts, UnsignedLongLongFixedTType)
                                             PrimType SignedCharFixedType        -> return (consts, SignedCharFixedType)
                                             PrimType SignedShortFixedType       -> return (consts, SignedShortFixedType)
                                             PrimType SignedLongFixedType        -> return (consts, SignedLongFixedType)
                                             PrimType SignedLongLongFixedType    -> return (consts, SignedLongLongFixedType)
                                             PrimType UnsignedCharFixedType      -> return (consts, UnsignedCharFixedType)
                                             PrimType UnsignedShortFixedType     -> return (consts, UnsignedShortFixedType)
                                             PrimType UnsignedLongFixedType      -> return (consts, UnsignedLongFixedType)
                                             PrimType UnsignedLongLongFixedType  -> return (consts, UnsignedLongLongFixedType)
                                             PrimType (EnumType x)         -> do
                                                                                 cs <- enumConsts scope pos (types,consts,[]) x -- TODO: feed the InterfaceDictionary in too
                                                                                 return (cs, EnumType x)
                                             PrimType CharType             -> return (consts, CharType)
                                             PrimType BooleanType          -> return (consts, BooleanType)
                                             _ -> failAtPosition pos "union discriminator must be an integer, char, boolean or enum type"
                                          xs'     <- concatMapM (unionCase (types,consts') scope dis') xs
                                          mayDef' <- maybe (return Nothing) (liftM Just . member (types,consts') scope) mayDef
                                          return (Union (unionAlignReq disPrim xs' mayDef') (unionCaseSize xs' mayDef') disPrim xs' mayDef')
inlineType (types,consts) scope (Idl.Sequence x ty)
                                     = do
                                          (_,x') <- intExpr consts scope 0 x
                                          ty' <- inlineType (types,consts) scope ty
                                          return (Sequence Seq x' (Buffer (size ty') ty'))
inlineType (_,consts) scope (Idl.StringType x)
                                     = do
                                          (_,x') <- intExpr consts scope 0 x
                                          return (Sequence StringSeq x' (Buffer 1 (PrimType CharType)))
inlineType (_,consts) scope (Idl.WideStringType x)
                                     = do
                                          (_,x') <- intExpr consts scope 0 x
                                          return (Sequence WideStringSeq x' (Buffer 2 (PrimType WideCharType)))
inlineType (_,consts) scope (Idl.DmahandleType x)
                                     = do
                                          (_,x') <- intExpr consts scope 0 x
                                          return (PrimType SignedLongType)
inlineType _ _ (Idl.Interface)       = return (Interface 4 False Nothing)
inlineType _ _ (Idl.Native (Idl.Identifier nm))
                                     = return (Native nm)
inlineType (types,consts) scope (Idl.Array x ty)
                                     = do
                                          (_,x') <- intExpr consts scope 1 x
                                          ty' <- inlineType (types,consts) scope ty
                                          return (Array x' ty')


enumConsts                          :: Monad m
                                    => Idl.ScopedName
                                    -> SourcePos
                                    -> (TypeDictionary, ConstDictionary, InterfaceDictionary)
                                    -> [String]
                                    -> m ConstDictionary

enumConsts scope pos (ts,cs,is) mems = foldM (enumConst scope pos mems ts is) cs mems


enumConst                           :: Monad m
                                    => Idl.ScopedName
                                    -> SourcePos
                                    -> [String]
                                    -> TypeDictionary
                                    -> InterfaceDictionary
                                    -> ConstDictionary
                                    -> String
                                    -> m ConstDictionary

enumConst scope pos mems ts is cs nm = scopedInsert (ts,cs,is) pos cs scope nm (ty, lit)
   where
      ty                             = PrimType (EnumType mems)
      lit                            = int (fromJust (nm `elemIndex` mems))
      int                            = IntLiteral Dec . SignedLongLiteral . fromIntegral


-- | Add a value to the dictionary at the current scope.
scopedInsert                        :: Monad m
                                    => (TypeDictionary, ConstDictionary, InterfaceDictionary)
                                    -> SourcePos
                                    -> Dictionary a
                                    -> Idl.ScopedName
                                    -> String
                                    -> a
                                    -> m (Dictionary a)

scopedInsert (ts,cs,is) pos dict scope nm val
                                     = do
                                          validateName (ts,cs,is) pos scope nm
                                          return ((nms, val) : dict)
   where
      nms                            = scope++[nm]


validateName                        :: Monad m
                                    => (TypeDictionary, ConstDictionary, InterfaceDictionary)
                                    -> SourcePos
                                    -> Idl.ScopedName
                                    -> String
                                    -> m ()

validateName (ts,cs,is) pos scope nm
      | isJust (lookup nms ts)       = failAtPosition pos ("identifier '" ++ nm ++ "' clashes with an earlier type definition")
      | isJust (lookup nms cs)       = failAtPosition pos ("identifier '" ++ nm ++ "' clashes with an earlier constant definition")
      | isJust (lookup nms is)       = failAtPosition pos ("identifier '" ++ nm ++ "' clashes with an earlier interface definition")
      | otherwise                    = return ()
   where
      nms                            = scope++[nm]


-- | Lookup a value from the dictionary using the current scope.  If not
--   there, lookup value in parent scope.
scopedLookup                        :: Monad m
                                    => SourcePos
                                    -> Dictionary a
                                    -> Idl.ScopedName
                                    -> Idl.ScopedName
                                    -> m a

scopedLookup pos dict _ ("":nms)     = scopedLookup' pos dict [[]] nms -- Only check global scope
scopedLookup pos dict scope nms      = scopedLookup' pos dict (reverse (inits scope)) nms


scopedLookup'                       :: Monad m
                                    => SourcePos
                                    -> Dictionary a
                                    -> [Idl.ScopedName]
                                    -> Idl.ScopedName
                                    -> m a

scopedLookup' pos _ [] nms           = failAtPosition pos ("Not in scope: " ++ intercalate "::" nms)
scopedLookup' pos dict (scope:ss) nms= -- First try to lookup the type ref in the current scope
                                       case lookup nms' dict of
                                          Just ty -> return ty
                                          -- If not in the current scope, check the parent scope
                                          Nothing -> scopedLookup' pos dict ss nms
   where
      nms'                           = scope++nms


unionCase                           :: Monad m
                                    => (TypeDictionary, ConstDictionary)
                                    -> Idl.ScopedName
                                    -> Type
                                    -> Idl.UnionCase
                                    -> m [(Literal, Member)]

unionCase (types,consts) scope ty (Idl.UnionCase cs m)
                                     = do
                                          lits <- mapM (constExpr consts scope ty) cs
                                          m'   <- member (types,consts) scope m
                                          return (map (\x -> (x, m')) lits)


member                              :: Monad m
                                    => (TypeDictionary, ConstDictionary)
                                    -> Idl.ScopedName
                                    -> Idl.Member
                                    -> m Member

member (types,consts) scope (Idl.Member pos _ (Idl.Identifier n) ty)
                                     = do
                                          validateName (types,consts,[]) pos scope n  -- TODO: feed the InterfaceDictionary in too
                                          liftM (Member n) (inlineType (types,consts) scope ty)


-- | Replace Size values with the size of type in the context of an outbound parameter
rout                                :: Type -> Type
rout (Struct al ms)                  = Struct al (map routMember ms)
rout (Union al _ d xs def)           = routUnion al d xs def
rout (Sequence s i buf)              = TaggedSequence s i (routBuffer buf)
rout (Array szs ty)                  = Array szs (rout ty)
rout (TypeLambda fid ty)             = TypeLambda fid (rout ty)
rout (Interface _ b Nothing)         = Interface 0 b Nothing
rout x                               = x

routUnion                           :: Alignment -> Prim -> [(Literal, Member)] -> Maybe Member -> Type
routUnion al d xs def                = Union al (unionCaseSize xs' def') d xs' def'
   where
      xs'                            = map (second routMember) xs
      def'                           = fmap routMember def

routBuffer                          :: Buffer -> Buffer
routBuffer (Buffer _ ty)             = Buffer (size (rout ty)) (rout ty)

routMember                          :: Member -> Member
routMember (Member nm ty)            = Member nm (rout ty)


failAtPosition                      :: Monad m => SourcePos -> String -> m a
failAtPosition pos                   = fail . errorMessage pos

errorMessage                        :: SourcePos -> String -> String
errorMessage pos msg                 = show (newErrorMessage (Message msg) pos)


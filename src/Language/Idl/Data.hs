-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

{-# LANGUAGE DeriveDataTypeable #-}

module Language.Idl.Data(
     module Data.Omg.Literal
   , module Data.Omg.Prim
   , Idl(..)
   , TopLevelDeclaration(..)
   , Declaration(..)
   , Definition(..)
   , Type(..)
   , ConstExpr(..)
   , Const(..)
   , Literal(..)
   , Member(..)
   , Parameter(..)
   , ParameterMode(..)
   , UnionCase(..)
   , ScopedName
   , InterfaceId
   , DefaultCase
   , OpAttr
   , Identifier(..)
   , Comment(..)
   , PreDoc
   , PostDoc
   ) where

import Text.ParserCombinators.Parsec.Pos(
     SourcePos
   )
import Language.Idl.SourcePos()
import Data.Generics(
     Typeable
   , Data
   )
import Data.Omg.Literal(
     Base(..)
   , IntegerLiteral(..) -- export constructors
   , Exponent
   )
import Data.Omg.Prim

data Idl                             = Idl [TopLevelDeclaration]
                                     deriving (Show, Eq, Data, Typeable)

data TopLevelDeclaration             = TopLevelDeclaration FilePath Declaration
                                     deriving (Show, Eq, Data, Typeable)

data Declaration                     = Declaration SourcePos PreDoc PostDoc Identifier Definition
                                     | CommentBlock [Comment]
                                     deriving (Show, Eq, Data, Typeable)

data Definition                      = TypeDcl Type
                                     | InterfaceDcl CheckExtParams IsLocal IsForward (Maybe InterfaceId) (SourcePos, Maybe ScopedName) [Declaration]
                                     | ConstDcl Type ConstExpr
                                     | OperationDcl IsAsync (Maybe (SourcePos, OpAttr)) (Maybe InterfaceId) Type [Parameter]
                                     | ModuleDcl [Declaration]
                                     | DummyDef String
                                     deriving (Show, Eq, Data, Typeable)

data Type                            = PrimType Prim
                                     | Struct String [Member]
                                     | Union SourcePos String Type [UnionCase] (Maybe DefaultCase)
                                     | Enum String [(PostDoc, String)]
                                     | Sequence ConstExpr Type
                                     | StringType ConstExpr
                                     | WideStringType ConstExpr
                                     | DmahandleType ConstExpr
                                     | Array ConstExpr Type
                                     | Interface
                                     | Native Identifier
                                     | TypeRef SourcePos IsNotNil ScopedName
                                     deriving (Show, Eq, Data, Typeable)

data ConstExpr                       = ConstExpr SourcePos Const
                                     deriving (Show, Eq, Ord, Data, Typeable)

data Const                           = LiteralExpr Literal
                                     | ConstExprRef ScopedName
                                     | Add ConstExpr ConstExpr
                                     | Sub ConstExpr ConstExpr
                                     | Mul ConstExpr ConstExpr
                                     | Div ConstExpr ConstExpr
                                     | Modulo ConstExpr ConstExpr
                                     | LeftShift ConstExpr ConstExpr
                                     | RightShift ConstExpr ConstExpr
                                     | Negate ConstExpr
                                     | Complement ConstExpr
                                     | BitwiseAnd ConstExpr ConstExpr
                                     | BitwiseOr ConstExpr ConstExpr
                                     | BitwiseXor ConstExpr ConstExpr
                                     deriving (Show, Eq, Ord, Data, Typeable)

data Literal                         = CharLiteral    Char
                                     | WCharLiteral   Char
                                     | StringLiteral  String
                                     | WStringLiteral String
                                     | IntLiteral Base Integer
                                     | FloatLiteral   String String Exponent
                                     | BoolLiteral    Bool
                                     | EnumLiteral    String
                                     deriving (Show, Eq, Ord, Data, Typeable)

data Member                          = Member SourcePos PostDoc Identifier Type
                                     deriving (Show, Eq, Data, Typeable)

data Parameter                       = Parameter Identifier ParameterMode Type
                                     deriving (Show, Eq, Data, Typeable)

data ParameterMode                   = ParameterIn
                                     -- | ParameterOut
                                     | ParameterROut
                                     -- | ParameterInOut
                                     | ParameterInROut
                                     deriving (Show, Eq, Ord, Data, Typeable)

data UnionCase                       = UnionCase [ConstExpr] Member
                                     deriving (Show, Eq, Data, Typeable)

data Comment                         = Comment IsMulti String
                                     deriving (Show, Eq, Data, Typeable)

newtype Identifier                   = Identifier String
                                     deriving (Show, Eq, Data, Typeable)

type ScopedName                      = [String]
type InterfaceId                     = ConstExpr
type DefaultCase                     = Member
type PreDoc                          = [Comment]
type PostDoc                         = [Comment]
type OpAttr                          = Identifier
type IsLocal                         = Bool
type IsForward                       = Bool
type IsNotNil                        = Bool
type IsMulti                         = Bool
type IsAsync                        = Bool
type CheckExtParams                = Bool


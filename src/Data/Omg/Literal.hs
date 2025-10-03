-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveDataTypeable #-}
-- | Data types for numeric, character, and string literals
module Data.Omg.Literal where

import Data.Generics(
     Data
   , Typeable
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

data Literal                         = Nil
                                     | CharLiteral    Char
                                     | WCharLiteral   Char
                                     | StringLiteral  String
                                     | WStringLiteral String
                                     | IntLiteral Base IntegerLiteral
                                     | RealLiteral    RealLit
                                     | BoolLiteral    Bool
                                     deriving (Show, Eq, Ord, Data, Typeable)

data RealLit                         = FloatLit Float
                                     | DoubleLit Double
                                     deriving (Show, Eq, Ord, Data, Typeable)

data IntegerLiteral                  = SignedCharLiteral       Int8
                                     | SignedShortLiteral      Int16
                                     | SignedLongLiteral       Int32
                                     | SignedLongLongLiteral   Int64
                                     | UnsignedCharLiteral     Word8
                                     | UnsignedShortLiteral    Word16
                                     | UnsignedLongLiteral     Word32
                                     | UnsignedLongLongLiteral Word64
                                     | OctetLiteral            Word8
                                     deriving (Show, Eq, Ord, Data, Typeable)

data Base                            = Dec
                                     | Hex
                                     | Oct
                                     deriving (Show, Eq, Ord, Data, Typeable)

type Exponent                        = String


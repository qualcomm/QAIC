-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveDataTypeable #-}

-- | Data type for primitive types
module Data.Omg.Prim where

import Data.Generics(
     Typeable
   , Data
   )

data Prim                            = SignedShortType
                                     | SignedLongType
                                     | SignedLongLongType
                                     | UnsignedShortType
                                     | UnsignedLongType
                                     | UnsignedLongLongType
                                     | UnsignedCharFixedTType
                                     | UnsignedShortFixedTType
                                     | UnsignedLongFixedTType
                                     | UnsignedLongLongFixedTType
                                     | SignedCharFixedTType
                                     | SignedShortFixedTType
                                     | SignedLongFixedTType
                                     | SignedLongLongFixedTType
                                     | UnsignedCharFixedType
                                     | UnsignedShortFixedType
                                     | UnsignedLongFixedType
                                     | UnsignedLongLongFixedType
                                     | SignedCharFixedType
                                     | SignedShortFixedType
                                     | SignedLongFixedType
                                     | SignedLongLongFixedType
                                     | BooleanType
                                     | FloatType
                                     | DoubleType
                                     | OctetType
                                     | CharType
                                     | WideCharType
                                     | EnumType [String]
                                     deriving (Show, Eq, Ord, Data, Typeable)

primSize                            :: Num a => Prim -> a
primSize BooleanType                 = 1
primSize OctetType                   = 1
primSize CharType                    = 1
primSize SignedCharFixedTType        = 1
primSize UnsignedCharFixedTType      = 1
primSize SignedCharFixedType         = 1
primSize UnsignedCharFixedType       = 1
primSize WideCharType                = 2
primSize SignedShortType             = 2
primSize UnsignedShortType           = 2
primSize SignedShortFixedTType       = 2
primSize UnsignedShortFixedTType     = 2
primSize SignedShortFixedType        = 2
primSize UnsignedShortFixedType      = 2
primSize SignedLongType              = 4
primSize UnsignedLongType            = 4
primSize SignedLongFixedTType        = 4
primSize UnsignedLongFixedTType      = 4
primSize SignedLongFixedType         = 4
primSize UnsignedLongFixedType       = 4
primSize (EnumType _)                = 4
primSize FloatType                   = 4
primSize SignedLongLongType          = 8
primSize UnsignedLongLongType        = 8
primSize SignedLongLongFixedTType    = 8
primSize UnsignedLongLongFixedTType  = 8
primSize SignedLongLongFixedType     = 8
primSize UnsignedLongLongFixedType   = 8
primSize DoubleType                  = 8



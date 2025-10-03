-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

module Language.SO.Literal where

import Data.Omg.Literal
import Text.PrettyPrint
import Numeric(showInt, showHex, showOct)
import Prelude hiding ( (<>) )
fromLiteral :: Literal -> Doc
fromLiteral  Nil = empty
fromLiteral  (CharLiteral    ch)             = quotes $ char ch
fromLiteral  (WCharLiteral   ch)             = text "L" <> quotes (char ch)
fromLiteral  (StringLiteral  str)            = text $ show str
fromLiteral  (WStringLiteral str)            = text $ "L" ++ (show str)

fromLiteral  (IntLiteral Dec (UnsignedLongLongLiteral ii))  = showInt' ii "ull"
fromLiteral  (IntLiteral Dec (UnsignedLongLiteral ii))      = showInt' ii "u"
fromLiteral  (IntLiteral Dec (UnsignedShortLiteral ii))     = showInt' ii ""
fromLiteral  (IntLiteral Dec (SignedLongLongLiteral ii))    = showInt' ii "ll"
fromLiteral  (IntLiteral Dec (SignedLongLiteral ii))        = showInt' ii ""
fromLiteral  (IntLiteral Dec (SignedShortLiteral ii))       = showInt' ii ""
fromLiteral  (IntLiteral Dec (OctetLiteral ii))             = showInt' ii ""

fromLiteral  (IntLiteral Hex (UnsignedLongLongLiteral ii))  = showHex' ii ""
fromLiteral  (IntLiteral Hex (UnsignedLongLiteral ii))      = showHex' ii ""
fromLiteral  (IntLiteral Hex (UnsignedShortLiteral ii))     = showHex' ii ""
fromLiteral  (IntLiteral Hex (SignedLongLongLiteral ii))    = showHex' ii ""
fromLiteral  (IntLiteral Hex (SignedLongLiteral ii))        = showHex' ii ""
fromLiteral  (IntLiteral Hex (SignedShortLiteral ii))       = showHex' ii ""
fromLiteral  (IntLiteral Hex (OctetLiteral ii))             = showHex' ii ""

fromLiteral  (IntLiteral Oct (UnsignedLongLongLiteral ii))  = showOct' ii ""
fromLiteral  (IntLiteral Oct (UnsignedLongLiteral ii))      = showOct' ii ""
fromLiteral  (IntLiteral Oct (UnsignedShortLiteral ii))     = showOct' ii ""
fromLiteral  (IntLiteral Oct (SignedLongLongLiteral ii))    = showOct' ii ""
fromLiteral  (IntLiteral Oct (SignedLongLiteral ii))        = showOct' ii ""
fromLiteral  (IntLiteral Oct (SignedShortLiteral ii))       = showOct' ii ""
fromLiteral  (IntLiteral Oct (OctetLiteral ii))             = showOct' ii ""

fromLiteral  (RealLiteral   (FloatLit ff))   = float ff
fromLiteral  (RealLiteral   (DoubleLit ff))  = double ff
fromLiteral  (BoolLiteral    True)  = text "TRUE"
fromLiteral  (BoolLiteral    False) = text "FALSE"

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




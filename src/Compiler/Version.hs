-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Compiler.Version where

version :: String
version = "01.00.50." --format for version must be xx.xx.xx where x belongs to 0-9

removedot :: String -> String
removedot xs = [ x | x <- xs, not (x `elem` ".") ]

ver = removedot version
str = show $ (read ver :: Integer)

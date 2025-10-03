-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

module Language.Idl.ParserState where

import Text.ParserCombinators.Parsec.Pos(
     SourcePos
   )

type ParserState                     = [Comment]
type Comment                         = (SourcePos, Either MultiLine SingleLine)
type MultiLine                       = String
type SingleLine                      = String


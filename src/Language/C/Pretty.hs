-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

module Language.C.Pretty where

import Text.PrettyPrint(Doc, render, text, (<+>))
import Debug.Trace(trace)

data Lang = C89
          | CPlusPlus

data Cfg = Cfg { indentSize :: Int
               , lang :: Lang
               , maxStackAllocation::Int
               }
new :: Cfg
new = Cfg 0 C89 512

class Pretty a where
   pretty :: Cfg -> a -> Doc


dbg' :: Pretty a => String -> a -> a
dbg' name a = (render $ text name <+> pretty new a) `trace` a



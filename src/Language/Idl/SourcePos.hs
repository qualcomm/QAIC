-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Data and Typeable instances for Parsec's SourcePos
module Language.Idl.SourcePos where

import Text.ParserCombinators.Parsec.Pos(
     SourcePos
   , newPos
   , sourceName
   , sourceLine
   , sourceColumn
   )
import Data.Data(
     Data(..)
   , Constr
   , DataType
   , Fixity(Prefix)
   , mkConstr
   , mkDataType
   )
import Data.Typeable(
     Typeable(..)
   )


#if (__GLASGOW_HASKELL__ < 700)

deriving instance Typeable SourcePos

instance Data SourcePos where
   gfoldl k z sp                     = z newPos `k` sourceName sp `k` sourceLine sp `k` sourceColumn sp
   gunfold k z _                     = k (k (k (z newPos)))
   toConstr                          = const conSourcePos
   dataTypeOf                        = const tySourcePos

conSourcePos                        :: Constr
conSourcePos                         = mkConstr (tySourcePos) "SourcePos" [] Prefix

tySourcePos                         :: DataType
tySourcePos                          = mkDataType "Text.ParserCombinators.Parsec.Pos.SourcePos" [conSourcePos]

#endif

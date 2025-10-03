-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

-- | This module extends System.FilePath
module System.FilePathUtils(
     forceForwardSlashes
   , DirPath
   ) where

type DirPath                         = FilePath

-- | Force directories separators to be forward slashes
forceForwardSlashes                 :: FilePath -> FilePath
forceForwardSlashes                  = map toSlash
   where
      toSlash '\\'                   = '/'
      toSlash ch                     = ch


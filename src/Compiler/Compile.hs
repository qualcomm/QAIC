-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Compiler.Compile where

import Debug.Trace(trace)
import Compiler.Data(
     IdlBackend
   , FileMap
   , CompilerCfg(..)
   )
import Language.Idl.Data(
     Idl(Idl)
   )
import Language.Idl.Parser(
     parseIdl
   )
import Language.Idl.ToPalette(
     paletteFromIdl
   )
import Language.Idl.Comment(
     processComments
   )
import Language.Palette.Data(
     Palette(Palette)
   )
import System.FilePath(
     takeBaseName
   )
import Control.MonadUtils(
     concatMapM
   )
import Data.ListUtils(
     unionl
   )

import qualified Data.ByteString.Lazy.Char8 as BSC


-- | Compile a set of preprocessed IDL files, applying each given backend
compile                             :: CompilerCfg -> [(String, IdlBackend)] -> [(FilePath, String)] -> Either String FileMap
compile cfg _ xs
   | ccPreprocessOnly cfg            = Right (preprocessOnly cfg (map snd xs))
compile cfg bs xs                    = do
                                          idls <- mapM (frontend cfg) xs
                                          concatMapM (backend (batchIdl cfg idls)) bs

-- | A backend that always succeeds and only prints one file
singletonBackend                    :: String -> String -> Either String FileMap
singletonBackend outNm               = binarySingletonBackend outNm . BSC.pack

-- | A backend that always succeeds and only prints one binary file
binarySingletonBackend              :: String -> BSC.ByteString -> Either String FileMap
binarySingletonBackend outNm cnts    = Right [(outNm, cnts)]

-- | Run batch mode over the preprocessed files and return the result
preprocessOnly                      :: CompilerCfg -> [String] -> FileMap
preprocessOnly cfg                   = map mkFile . batchFiles cfg
   where
      mkFile (nm, cnts)              = (nm++".idl", BSC.pack cnts)

-- | Parse and typecheck an IDL string, return an IDL AST and its Palette
frontend                            :: CompilerCfg -> (FilePath, String) -> Either String ([FilePath], Idl, Palette)
frontend cfg (src, cnts)             = do
                                          idl <- parseIdl src cnts
                                          pal <- paletteFromIdl warnUndef keepFiles rootIface idl
                                          return (keepFiles, idl, (dbg " here4" pal))
   where
      dbg str val = trace ((show val) ++ str) val
      keepFiles                      = if ccKeepNested cfg then [] else [src]
      warnUndef                      = ccWarnUndef cfg
      rootIface                      = ccRootIface cfg

-- | Apply backend to the given set of Idls
backend                             :: [(String, ([FilePath], Idl, Palette))] -> (String, IdlBackend) -> Either String FileMap
backend idls (lang, f)               = concatMapM (idlToCode f lang) idls

-- | Apply backend to Idl
idlToCode                           :: IdlBackend -> String -> (String, ([FilePath], Idl, Palette)) -> Either String FileMap
idlToCode f lang (nm, (files, idl, pal))
                                     = f nm files commentFilteredIdl pal
   where
      commentFilteredIdl             = processComments files lang idl

-- | Apply "batch" mode to a set of files
batchFiles                          :: CompilerCfg -> [String] -> [(String, String)]
batchFiles                           = batch concat

-- | Apply "batch" mode to a set of IDL ASTs
batchIdl                            :: CompilerCfg -> [([FilePath], Idl, Palette)] -> [(String, ([FilePath], Idl, Palette))]
batchIdl                             = batch mergeDeclarations

-- | In each set of declarations, remove the first declaration if it exists in a set to the left
mergeDeclarations                   :: [([FilePath], Idl, Palette)] -> ([FilePath], Idl, Palette)
mergeDeclarations xs                 = (concatMap fst3 xs, idl xs, pal xs)
   where
      fst3 (x,_,_)                   = x
      idl                            = merge Idl     (\(_, Idl ds, _)     -> ds)
      pal                            = merge Palette (\(_, _, Palette ds) -> ds)
      merge put get                  = put . unionl . map get

-- | If in batch mode, bundle all the files into one target.  Otherwise, tag each file with its target name and source files.
batch                               :: ([a] -> a) -> CompilerCfg -> [a] -> [(String, a)]
batch merge cfg xs
   | isBatch                         = [(batchName, merge xs)]
   | otherwise                       = zip (map takeBaseName files) xs
   where
      isBatch                        = ccIsBatch cfg
      batchName                      = ccBatchName cfg
      files                          = ccFiles cfg


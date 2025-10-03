-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

module Compiler.Data where

import Language.Idl.Data(
     Idl
   , PreDoc
   , PostDoc
   )
import Language.Palette.Data(
     Palette
   )
import System.FilePathUtils(
     DirPath
   )

import qualified Data.ByteString.Lazy as BS

type IdlBackend                      = String -> [FilePath] -> (Idl,QidlComments) -> Palette -> Either String FileMap
type QidlComments                    = (PreDoc, PostDoc)
type FileMap                         = [(String, BS.ByteString)]

data Cfg                             = Cfg {
                                         cfgPreprocessOnly :: Bool
                                       , cfgWarnUndef      :: Bool
                                       , cfgKeepNested     :: Bool
                                       , cfgIsCif          :: Bool
                                       , cfgIsBatch        :: Bool
                                       , cfgIsHeader       :: Bool
                                       , cfgIsRemoting     :: Bool
                                       , cfgIsSlim         :: Bool
                                       , cfgIndent         :: Int
                                       , cfgRootIface      :: Maybe String
                                       , cfgBatchName      :: String
                                       , cfgOutDir         :: DirPath
                                       , cfgCpp            :: FilePath
                                       , cfgCppArgs        :: [String]
                                       , cfgLangs          :: [String]
                                       , cfgCifLangs       :: [String]
                                       , cfgIncludeDirs    :: [DirPath]
                                       , cfgDefines        :: [String]
                                       , cfgFiles          :: [FilePath]
                                       } deriving (Show, Eq)

data CompilerCfg                      = CompilerCfg {
                                          ccPreprocessOnly :: Bool
                                        , ccWarnUndef      :: Bool
                                        , ccKeepNested     :: Bool
                                        , ccIsBatch        :: Bool
                                        , ccBatchName      :: String
                                        , ccRootIface      :: Maybe String
                                        , ccFiles          :: [FilePath]
                                        } deriving (Show, Eq)

defaultConfig                       :: Cfg
defaultConfig                        = Cfg {
                                         cfgPreprocessOnly = False
                                       , cfgWarnUndef      = False
                                       , cfgKeepNested     = False
                                       , cfgIsCif          = False
                                       , cfgIsBatch        = False
                                       , cfgIsHeader       = True
                                       , cfgIsRemoting     = True
                                       , cfgIsSlim         = True
                                       , cfgIndent         = 3
                                       , cfgRootIface      = Nothing
                                       , cfgBatchName      = ""
                                       , cfgOutDir         = "."
                                       , cfgCpp            = ""
                                       , cfgCppArgs        = []
                                       , cfgLangs          = ["cxx"]
                                       , cfgCifLangs       = []
                                       , cfgIncludeDirs    = []
                                       , cfgDefines        = []
                                       , cfgFiles          = []
                                       }


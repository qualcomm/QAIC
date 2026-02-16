-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

import System.Environment(
     getArgs
   )
import System.FilePath(
     (</>)
   )
import System.FilePathUtils(
     DirPath
   )
import Language.Preprocessor.Cpp(
     preprocess
   )
import Language.SO.IdlBackend(
     soBackend
   )
import System.Process(
     readProcessWithExitCode
   )
import System.Exit(
     ExitCode(ExitSuccess)
   )
import Control.Monad(
     when
   , unless
   )
import Data.Maybe(
     fromMaybe
   )
import Data.List(
     intercalate
   )
import Compiler.CommandLineArgs(
     configure
   , fatalError
   )
import Compiler.Data(
     Cfg(..)
   , CompilerCfg(..)
   , IdlBackend
   )
import Compiler.Compile(
     compile
   )

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

main                                 :: IO ()
main                                  = do
                                           args <- getArgs
                                           cfg  <- configure args
                                           assertSupportedTargetLanguages (backendMap cfg) (cfgLangs cfg ++ cfgCifLangs cfg)

                                           run cfg

run                                  :: Cfg -> IO ()
run cfg                               = do
                                           preprocessed <- mapM (preprocessFile cpp cppArgs includeDirs defines) files
                                           outFiles     <- returnOrDie (compile cc (backends cfg) preprocessed)
                                           writeFiles (cfgOutDir cfg) outFiles
   where
      cpp                             = cfgCpp cfg
      cppArgs                         = cfgCppArgs cfg
      includeDirs                     = cfgIncludeDirs cfg
      defines                         = cfgDefines cfg
      files                           = cfgFiles cfg

      returnOrDie                     = either fatalError return
      cc                              = CompilerCfg{
                                           ccPreprocessOnly = cfgPreprocessOnly cfg
                                         , ccWarnUndef      = cfgWarnUndef cfg
                                         , ccKeepNested     = cfgKeepNested cfg
                                         , ccIsBatch        = cfgIsBatch cfg
                                         , ccBatchName      = cfgBatchName cfg
                                         , ccRootIface      = cfgRootIface cfg
                                         , ccFiles          = files
                                        }

preprocessFile                      :: FilePath -> [String] -> [DirPath] -> [String] -> FilePath -> IO (FilePath, String)
preprocessFile cppPath cppArgs includeDirs defines nm
   | null cppPath                    = do
                                          out <- preprocess includeDirs defines nm
                                          return (nm, out)
   | otherwise                       = do
                                          (errCd, out, err) <- readProcessWithExitCode cppPath args ""
                                          when (errCd /= ExitSuccess) (fatalError err)
                                          return (nm, out)
   where
      args                           = cppArgs ++ map ("-I"++) includeDirs ++ map ("-D"++) defines ++ [nm]


backends                            :: Cfg -> [(String, IdlBackend)]
backends cfg                         = map lookupBackend langs
   where
      lookupBackend lang             = (lang, gimme (lang `lookup` backendMap cfg))
      gimme                          = fromMaybe (error "internal error: unsupported output language")
      langs                          = cfgLangs cfg

backendMap                          :: Cfg -> [(String, IdlBackend)]
backendMap cfg                       = [ ("parse-only", semanticCheck)
                                       , ("dll",        soBackend isHeader isRemoting isSlim indent useStdTypes)
                                       ]
   where
      isSlim                         = cfgIsSlim cfg
      isHeader                       = cfgIsHeader cfg
      isRemoting                     = cfgIsRemoting cfg
      indent                         = cfgIndent cfg
      useStdTypes                    = cfgUseStandardTypes cfg

      semanticCheck                 :: IdlBackend
      semanticCheck _ _ _ _          = Right []


writeFiles                          :: DirPath -> [(FilePath, BS.ByteString)] -> IO ()
writeFiles dir                       = mapM_ write
   where
      write (nm, cnts)               = do let (hh:rest) = (BS.toChunks cnts)
                                          BSS.writeFile (dir</>nm) $! hh
                                          mapM_ (BSS.appendFile (dir</>nm)) rest

assertSupportedTargetLanguages      :: [(String, IdlBackend)] -> [String] -> IO ()
assertSupportedTargetLanguages bmp   = mapM_ (assertSupportedTargetLanguage (map fst bmp))

assertSupportedTargetLanguage       :: [String] -> String -> IO ()
assertSupportedTargetLanguage langs lang
                                     = unless (lang `elem` langs) (error msg)
   where
      msg                            = "Unsupported output language: "
                                    ++ lang
                                    ++ "\n  Supported languages: "
                                    ++ intercalate "," (filter (`notElem` unsupportedLangs) langs)
                                    ++ "\n"
      unsupportedLangs               = ["slim", "ast", "im"]


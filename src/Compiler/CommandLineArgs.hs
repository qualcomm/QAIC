-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Compiler.CommandLineArgs where

import System.IO(
     hPutStrLn
   , stderr
   )
import System.FilePathUtils(
     DirPath
   , forceForwardSlashes
   )
import System.Directory(
     doesDirectoryExist
   , doesFileExist
   )
import Language.Preprocessor.Cpp(
     readBinaryFile
   )
import System.Console.GetOpt(
     getOpt
   , ArgDescr(..)
   , OptDescr(..)
   , ArgOrder(..)
   , usageInfo
   )
import System.Exit(
     exitSuccess
   , exitFailure
   )
import Control.Monad(
     when
   , unless
   , filterM
   )
import Data.ListUtils(
     headDef
   , notNull
   )
import Compiler.Data(
     Cfg(..)
   )

import Compiler.Version(
      version
   ,  gitcommit
   ,  ghcVersion
   ,  cabalVersion
   ,  platform
   )

data Flag                            = Lang String
                                     | Include DirPath
                                     | Define String
                                     | OutDir DirPath
                                     | KeepNested
                                     | Batch String
                                     | HeaderOnly
                                     | RemotingOnly
                                     | Slim
                                     | GenerateCif
                                     | CifOnly
                                     | IdlFiles FilePath
                                     | Cpp FilePath
                                     | CppArg String
                                     | RootIface String
                                     | WarnUndefined
                                     | Indent String
                                     | PreprocessOnly
                                     | Version
                                     | GitCommitID
                                     | GhcVersion
                                     | CabalVersion
                                     | Platform
                                     | Help
                                     deriving (Show, Eq)


copyright                            :: String
copyright                             = "Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.\nSPDX-License-Identifier: BSD-3-Clause-Clear"

configure                            :: [String] -> IO Cfg
configure args                        = do
                                           (flgs, commandlineFiles) <- compilerOpts args

                                           when (Version `elem` flgs) $ do
                                              putStrLn ("Qaic's Another IDL Compiler: version " ++ version)
                                              putStrLn copyright
                                              exitSuccess

                                           when (GitCommitID `elem` flgs) $ do
                                              putStrLn ("Qaic's Another IDL Compiler: GitCommitID " ++ gitcommit)
                                              putStrLn copyright
                                              exitSuccess

					   when (GhcVersion `elem` flgs) $ do
                                              putStrLn ("Qaic's Another IDL Compiler: GhcVersion " ++ ghcVersion)
                                              putStrLn copyright
                                              exitSuccess

					   when (CabalVersion `elem` flgs) $ do
                                              putStrLn ("Qaic's Another IDL Compiler: CabalVersion " ++ cabalVersion)
                                              putStrLn copyright
                                              exitSuccess

					   when (Platform `elem` flgs) $ do
                                              putStrLn ("Qaic's Another IDL Compiler: Built on " ++ platform)
                                              putStrLn copyright
                                              exitSuccess

                                           when (Help `elem` flgs) $ do
                                              putStrLn (usageInfo header options)
                                              putStrLn copyright
                                              exitSuccess

                                           let mfs = map forceForwardSlashes

                                           let includeDirs  = mfs $ [dir  | Include dir <- flgs]
                                               outDirs      = mfs $ [dir  | OutDir  dir <- flgs]
                                               batchNames   = [nm   | Batch   nm  <- flgs]
                                               idlsPaths    = mfs $ [path | IdlFiles path <- flgs]
                                               cpps         = mfs $ [path | Cpp path <- flgs]
                                               rootIfaces   = [x    | RootIface x <- flgs]
                                               indents      = [read x | Indent x <- flgs]
                                               langsPre     = [lng  | Lang lng <- flgs]
                                               langsDef     = if null langsPre then ["dll"] else langsPre
                                               isCifOnly    = CifOnly `elem` flgs
                                               langs        = if isCifOnly then [] else langsDef
                                               isRemoteOnly = RemotingOnly `elem` flgs
                                               isSlim       = Slim `elem` flgs
                                               isHeaderOnly = HeaderOnly `elem` flgs || ("bim" `elem` langs && not isCifOnly && not isRemoteOnly)
                                               isCif        = GenerateCif `elem` flgs || isCifOnly

                                           assertDirectoryExists (headDef "." outDirs)
                                           includeDirs' <- filterM doesDirectoryExist includeDirs

                                           mapM_ assertFileExists idlsPaths
                                           idlsContents <- mapM readBinaryFile idlsPaths
                                           let files = mfs commandlineFiles ++ lines (concat idlsContents)
                                           mapM_ assertFileExists files

                                           when (null files) (fatalError "No input files given.")

                                           let isTooMany = 1 < length (filter id [isHeaderOnly, isRemoteOnly, isCifOnly])
                                           when isTooMany (fatalError "--header-only, --remoting-only, and --cif-only are mutually exclusive")

                                           when ("c++" `elem` langs && not isHeaderOnly) (fatalError "C++-only mapping does not support remoting; use with -ho (header only) or use -mcxx (C/C++ mapping) instead")

                                           return $ Cfg {
                                               cfgLangs          = langs
                                             , cfgFiles          = files
                                             , cfgIsCif          = isCif
                                             , cfgIncludeDirs    = includeDirs'
                                             , cfgDefines        = [sym  | Define  sym <- flgs]
                                             , cfgCifLangs       = if isCif     then langsDef else []
                                             , cfgRootIface      = Nothing
                                             , cfgWarnUndef      = WarnUndefined `elem` flgs
                                             , cfgIndent         = headDef 3 indents
                                             , cfgOutDir         = headDef "." outDirs
                                             , cfgBatchName      = headDef "" batchNames
                                             , cfgCpp            = headDef "" cpps
                                             , cfgCppArgs        = [x    | CppArg x <- flgs]
                                             , cfgKeepNested     = KeepNested `elem` flgs
                                             , cfgIsBatch        = notNull batchNames
                                             , cfgIsHeader       = isHeaderOnly || not isRemoteOnly
                                             , cfgIsRemoting     = isRemoteOnly || not isHeaderOnly
                                             , cfgIsSlim         = isSlim
                                             , cfgPreprocessOnly = PreprocessOnly `elem` flgs
                                           }

assertFileExists                    :: FilePath -> IO ()
assertFileExists path                = do
                                          exists <- doesFileExist path
                                          unless exists (error ("File does not exist: " ++ path))

assertDirectoryExists               :: DirPath -> IO ()
assertDirectoryExists dir            = do
                                          exists <- doesDirectoryExist dir
                                          unless exists (error ("Directory does not exist: " ++ dir))

compilerOpts                        :: [String] -> IO ([Flag], [String])
compilerOpts argv                    = case getOpt Permute options argv of
                                          (o,n,[]  ) -> return (o,n)
                                          (_,_,errs) -> usageError (concat errs)

usageError                          :: String -> IO a
usageError msg                       = do
                                          hPutStrLn stderr msg
                                          putStrLn ("\n" ++ usageInfo header options ++ copyright)
                                          exitFailure

fatalError                          :: String -> IO a
fatalError msg                       = do
                                          hPutStrLn stderr msg
                                          exitFailure

header                              :: String
header                               = "qaic - Qaic's Another Idl Compiler\n\nUsage: qaic -m LANG [OPTIONs] file1.idl file2.idl ... fileN.idl"

options                             :: [OptDescr Flag]
options                              = [ Option "o" ["output-path"]  (ReqArg OutDir "DIR")    "Set the output path for generated code."
                                       , Option "p" ["cpp"]          (ReqArg Cpp "PATH")      "Specifies the preprocessor to use."
                                       , Option "pa"["arg-cpp"]      (ReqArg CppArg "ARG")  $ "Specifies argument that is transparently\n"
                                                                                           ++ "passed on the preprocessor."
                                       , Option "i" ["indent"]       (ReqArg Indent "WIDTH") $"Specifies the indentation width to use in\n"
                                                                                           ++ "generated code."
                                       , Option "I" ["include-path"] (ReqArg Include "DIR")   "Additional path searched for IDL files."
                                       , Option "D" ["define"]       (ReqArg Define "SYMBOL") "Predefine macro for the preprocessor."
                                       , Option "ho"["header-only"]  (NoArg HeaderOnly)     $ "Generates only headers. No remoting stub or\n"
                                                                                           ++ "skeleton is generated."
                                       , Option "ro"["remoting-only"](NoArg RemotingOnly)   $ "Generates only remoting (stub/skel) code.  No\n"
                                                                                           ++ "header is generated."
                                       , Option "mdll" ["map-dll"]   (NoArg (Lang "dll"))     "Generate DLL mapping."
                                       , Option "m" []               (ReqArg Lang "LANG")     "Specifies the output language."
                                       , Option "s" ["parse-only"]   (NoArg (Lang "parse-only"))  $ "Parse the IDL and perform semantic checking,\n"
                                                                                           ++ "but do not generate any output."
                                       , Option ""  ["preprocess-only"] (NoArg PreprocessOnly) $ "Preprocess IDL files and output the\n"
                                                                                           ++ "preprocessor's output."
                                       , Option "Wu"["warn-undefined"] (NoArg WarnUndefined)$ "Issue warning for forward-declared interfaces\n"
                                                                                           ++ "that are never defined."
                                       , Option "r" ["recursive"]    (NoArg KeepNested)     $ "Generate bindings for declarations in included\n"
                                                                                           ++ "IDL files."
                                       , Option ""  ["idl-files"]    (ReqArg IdlFiles "PATH")$"Instead of command-line, use IDL files listed\n"
                                                                                           ++ "in this file."
                                       , Option "v" ["version"]      (NoArg Version)          "Prints the version of the compiler."
                                       , Option "Gc" ["gitcommit"]   (NoArg GitCommitID)      "Prints the GitCommitID of the compiler source code."
                                       , Option "ghc" ["ghcversion"]   (NoArg GhcVersion)      "Prints the GhcVersion used to build the compiler source code."
                                       , Option "cabal" ["cabalversion"]   (NoArg CabalVersion)      "Prints the CabalVersion used to build the compiler source code."
                                       , Option "ostype" ["platform"]   (NoArg Platform)      "Prints the platform used to build the compiler source code."
                                       , Option "h" ["help"]         (NoArg Help)             "Prints this message."
                                       ]




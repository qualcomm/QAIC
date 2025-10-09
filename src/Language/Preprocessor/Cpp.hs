-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

-- | This is a quick and dirty implementation of a subset of CPP, the C Preprocessor.
--   It only handles, #include, #ifndef, and #endif, and only respects single-line comments.
module Language.Preprocessor.Cpp(
     preprocess
   , readBinaryFile
   ) where

import System.IO(
     hPutStrLn
   , stderr
   )
import Text.ParserCombinators.Parsec.Prim(
     (<|>)
   , many
   , Parser
   , parse
   , try
   )
import Text.ParserCombinators.Parsec.Char(
     char
   , string
   , alphaNum
   , spaces
   , oneOf
   , noneOf
   , anyChar
   )
import Text.ParserCombinators.Parsec.Combinator(
     many1
   )
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.FilePath(
     takeDirectory
   )
import System.FilePathUtils(
     forceForwardSlashes
   )
import System.Directory(
     doesFileExist
   )
import System.Exit(
     exitFailure
   )
import Data.List(
     isPrefixOf
   , groupBy
   )
import Data.Function(
     on
   )
import Control.MonadUtils(
     butReturn
   , singletonM
   , mapAccumM
   )

-- |Run the preprocessor over the file, expanding all #include lines and removing
-- code guarded by #ifndef
preprocess                          :: [FilePath] -> [String] -> FilePath -> IO String
preprocess includeDirs defines path  = do
                                          contents <- readBinaryFile path'
                                          (xs', _) <- processConditionals includeDirs' path' defines (lines contents)
                                          return (unlines (dropUnneeded (startPragma : xs')))
   where
      startPragma                    = "#line 1 " ++ show path'
      path'                          = forceForwardSlashes path
      includeDirs'                   = map forceForwardSlashes includeDirs

-- |'expandIncludes' can cause several lines in a row to start with "#line".  A parser
-- only needs to be aware of the last one.  So group all lines by "#line" and return
-- just the last item in each group
dropUnneeded                        :: [String] -> [String]
dropUnneeded                         = map last . groupBy ((&&) `on` isPrefixOf "#line")

type Accumulator                     = (Defines, Conditionals, InComment)
type InComment                       = Bool

-- |When we encounter a #define we add the name it defines to this collection
type Defines                         = [String]

-- |When we encounter a #ifndef we add a boolean to this stack.  Its existence
-- in the stack lets us know that we are waiting for an #endif.  Its value lets us
-- know if all lines until the next #endif should be removed.
type Conditionals                    = [Bool]

-- |Process each line, passing along an accumulator of holding previous #defines
-- and current #ifndef state.
processConditionals                 :: [FilePath] -> FilePath -> [String] -> [String] -> IO ([String], [String])
processConditionals includeDirs path defines xs
                                     = do
                                          ((defines',_,_),xss) <- mapAccumM (processLine includeDirs path) (defines, [], False) (zip [2..] xs)
                                          return (concat xss, defines')

-- |Handle #ifndef, #endif, #define
processLine                         :: [FilePath] -> FilePath -> Accumulator -> (Int, String) -> IO (Accumulator, [String])
processLine _ _ (defs, conds, True) (_, line)
                                     = return ((defs, conds, isStillInComment line), [line])
processLine includeDirs path (defs, conds, False) (nextLineNumber, line)
                                     = -- check for #endif
                                       case matchEndif line of
                                          Just _ -> return ((defs, drop 1 conds, False), [])  -- \#endif, pop a condition
                                          _      ->                             -- Not an #endif, move on
                                             -- check for #ifndef
                                             case matchIfndef line of
                                                -- if #ifndef, add True to conditionals if the symbol is not in defs
                                                Just nm -> return ((defs, (nm `notElem` defs) : conds, False), [])
                                                _       ->
                                                   -- check for #ifdef
                                                   case matchIfdef line of
                                                      -- if #ifdef, add True to conditionals if the symbol is in defs
                                                      Just nm -> return ((defs, (nm `elem` defs) : conds, False), [])  -- \#ifdef, add name to conditionals
                                                      _       ->
                                                         -- if any conditional is False, remove the line
                                                         if any not conds
                                                            then return ((defs, conds, False), [])
                                                            else
                                                               -- check for #define
                                                               case matchDefine line of
                                                                  Just nm -> return ((nm:defs, conds, False), [])  -- \#define, add name to defs
                                                                  _       ->
                                                                     -- check for #include
                                                                     case matchInclude line of
                                                                          Just (nm, isAngle) -> do
                                                                             (xs, defs') <- expandInclude isAngle includeDirs path defs nextLineNumber nm
                                                                             return ((defs', conds, False), xs)
                                                                          _       -> return ((defs, conds, isStartComment line), [line]) -- Normal line, no active conditionals

isStartComment                      :: String -> InComment
isStartComment                       = simpleParse startComment

isStillInComment                    :: String -> InComment
isStillInComment                     = simpleParse endComment

simpleParse                         :: Parser a -> String -> a
simpleParse p line                   = either (error msg) id (parse p "pre" line)
   where
      msg                            = "internal error: preprocessor choked on: " ++ line

startComment                        :: Parser InComment
startComment                         = try (string "//") `butReturn` False  -- Ignore lines where multi-comment is within a single-line comment
                                   <|> (reserved "/*" >> endComment)
                                   <|> ((try stringLiteral <|> singletonM anyChar) >> startComment)
                                   <|> return False

endComment                          :: Parser InComment
endComment                           = (try endCommentOnly >> startComment)
                                   <|> many anyChar `butReturn` True

endCommentOnly                      :: Parser ()
endCommentOnly                       = reserved "*/"
                                   <|> (many1 (noneOf "*") >> endCommentOnly)
                                   <|> (char '*' >> endCommentOnly)

expandInclude                       :: Bool -> [FilePath] -> FilePath -> [String] -> Int -> String -> IO ([String], [String])
expandInclude isAngle includeDirs path defines nextLineNumber nm
                                     = do
                                          includedPath <- findIncludedFile (currDirs ++ includeDirs) nm
                                          (expanded, defines') <- expand includedPath
                                          return (expandedLineInstruction includedPath : expanded ++ [returnLineInstruction], defines')
   where
      currDirs                       = if isAngle then [] else [mkDir (takeDirectory path)]
      mkDir p                        = if null p then "." else p

      expandedLineInstruction p      = "#line 1 " ++ show p
      returnLineInstruction          = "#line " ++ show nextLineNumber ++ " " ++ show path

      expand                        :: FilePath -> IO ([String], [String])
      expand p                       = readBinaryFile p >>= processConditionals includeDirs p defines . lines

      findIncludedFile []     n      = hPutStrLn stderr (path ++ ":" ++ show (nextLineNumber - 1) ++ ": Included file could not be found: " ++ show n) >> exitFailure
      findIncludedFile (d:ds) n      = do
                                          exists <- doesFileExist (d++"/"++n)
                                          if exists then return (d++"/"++n) else findIncludedFile ds n


matchEndif                          :: String -> Maybe ()
matchEndif                           = fmap (const ()) . parseMaybe (reserved "#endif")

matchIfndef                         :: String -> Maybe String
matchIfndef                          = parseMaybe (reserved "#ifndef" >> identifier)

matchIfdef                          :: String -> Maybe String
matchIfdef                           = parseMaybe (reserved "#ifdef" >> identifier)

matchDefine                         :: String -> Maybe String
matchDefine                          = parseMaybe (reserved "#define" >> identifier)

matchInclude                        :: String -> Maybe (String, Bool)
matchInclude                         = parseMaybe (reserved "#include" >> (quotedInclude <|> angleInclude))

angleInclude                        :: Parser (String, Bool)
angleInclude                         = do
                                          _ <- char '<'
                                          path <- many (noneOf ['>'])
                                          _ <- char '>'
                                          return (path, True)

quotedInclude                       :: Parser (String, Bool)
quotedInclude                        = do
                                          _ <- char '"'
                                          path <- many (noneOf ['"'])
                                          _ <- char '"'
                                          return (path, False)

parseMaybe                          :: Parser a -> String -> Maybe a
parseMaybe p                         = either (const Nothing) Just . parse (spaces >> p) "pre"

reserved                            :: String -> Parser ()
reserved                             = Token.reserved tokenizer

identifier                          :: Parser String
identifier                           = Token.identifier tokenizer

stringLiteral                       :: Parser String
stringLiteral                        = Token.stringLiteral tokenizer

tokenizer                           :: Token.TokenParser a
tokenizer                            = Token.makeTokenParser Token.LanguageDef{
       Token.commentStart            = "/*"
     , Token.commentEnd              = "*/"
     , Token.commentLine             = "//"
     , Token.nestedComments          = False
     , Token.identStart              = alphaNum <|> char '_' <|> char '-'
     , Token.identLetter             = alphaNum <|> char '_' <|> char '-'
     , Token.opStart                 = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , Token.opLetter                = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , Token.reservedOpNames         = []
     , Token.reservedNames           = ["#define","#include","#ifndef","#ifdef","#endif"]
     , Token.caseSensitive           = False
     }

readBinaryFile                      :: FilePath -> IO String
readBinaryFile                       = readFile


-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

module Language.Idl.Comment(
     processComments
   , qcHeader
   , CommentConfig(..)
   , defaultCommentConfig
   ) where

import Language.Idl.Data
import Text.ParserCombinators.Parsec.Prim(
     (<|>)
   , many
   , try
   , Parser
   , parse
   )
import Text.ParserCombinators.Parsec.Char(
     char
   , string
   , anyChar
   , alphaNum
   )
import Text.ParserCombinators.Parsec.Combinator(
     option
   , sepBy1
   , many1
   , manyTill
   )
import Data.List(
     partition
   , mapAccumL
   , nubBy
   , groupBy
   )
import Control.Arrow(
     first
   )
import Control.MonadUtils(
     butReturn
   , followedBy
   , tupleM
   )
import System.FilePathUtils(
     forceForwardSlashes
   )

data QidlComment                     = QidlComment {
     qcSrc                          :: FilePath
   , qcConfig                       :: CommentConfig
   , qcComments                     :: [Comment]
   } deriving (Show, Eq)

data CommentConfig                   = CommentConfig {
     commentIsTop                   :: Bool
   , commentIsNested                :: Bool
   , commentLangs                   :: [String]
   , commentIsFold                  :: Bool
   } deriving (Show, Eq)

defaultCommentConfig                :: CommentConfig
defaultCommentConfig                 = CommentConfig{ commentIsTop    = False
                                                    , commentIsNested = False
                                                    , commentLangs    = ["c", "cxx", "c++"]
                                                    , commentIsFold   = False
                                                    }

copyrightCommentConfig              :: CommentConfig
copyrightCommentConfig               = CommentConfig{ commentIsTop    = True
                                                    , commentIsNested = False
                                                    , commentLangs    = [] -- empty == "all"
                                                    , commentIsFold   = True
                                                    }

-- | Apply "qidl comment" filters
processComments                     :: [FilePath] -> String -> Idl -> (Idl,(PreDoc,PostDoc))
processComments files lang           = compact . first process . extract
   where
      extract                       :: Idl -> ([QidlComment], [TopLevelDeclaration])
      extract (Idl ds)               = (concatMap mkQcs ds, concatMap strip ds)

      process                       :: [QidlComment] -> [QidlComment]
      process                        = foldComments . filterLang . concatMap (parseComment files')

      compact                       :: ([QidlComment], [TopLevelDeclaration]) -> (Idl, (PreDoc, PostDoc))
      compact (xs, ds)               = (Idl ds, (concatMap qcComments top, concatMap qcComments bottom))
         where
            (top,bottom)             = partition (commentIsTop . qcConfig) xs

      strip                         :: TopLevelDeclaration -> [TopLevelDeclaration]
      strip (TopLevelDeclaration src (Declaration pos pre post nm def))
                                     = [TopLevelDeclaration src (Declaration pos (deleteQcs pre) (deleteQcs post) nm def)]
      strip (TopLevelDeclaration src (CommentBlock xs))
            | null xs'               = []
            | otherwise              = [TopLevelDeclaration src (CommentBlock xs')]
         where
            xs'                      = deleteQcs xs

      deleteQcs                      = filter (not . isQc)
      isQc (Comment _ s)             = isRight (parse (qcLine defaultCommentConfig) "" s)
      isRight                        = either (const False) (const True)

      filterLang                     = filter (is . commentLangs . qcConfig)
         where
            is []                    = True
            is xs                    = lang `elem` xs

      foldComments                  :: [QidlComment] -> [QidlComment]
      foldComments                   = nubBy isQcEq

      isQcEq qc1 qc2                 = commentIsFold (qcConfig qc1) == True
                                    && commentIsFold (qcConfig qc2) == True
                                    && qcComments qc1               == qcComments qc2

      mkQcs                         :: TopLevelDeclaration -> [QidlComment]
      mkQcs (TopLevelDeclaration src (CommentBlock xs))
                                     = mkQc src xs
      mkQcs (TopLevelDeclaration src (Declaration _ pre post _ _))
                                     = mkQc src (pre ++ post)

      mkQc                          :: FilePath -> [Comment] -> [QidlComment]
      mkQc _   []                    = []
      mkQc src xs                    = map (QidlComment src defaultCommentConfig) (groupBy isBody xs)
         where
            isBody _ (Comment _ s)   = isRight (parse qcString "" s)  -- split by 'qidl' header

      files'                         = map forceForwardSlashes files

parseComment                        :: [FilePath] -> QidlComment -> [QidlComment]
parseComment files qc
      | null xs'                     = []
      | otherwise                    = [qc{qcConfig=cfg', qcComments=xs'}]
   where
      xs'                            = filter (not . empty) xs
      empty (Comment _ s)            = null s

      (cfg', xs)                     = mapAccumL parseQcHeader (qcConfig qc) (qcComments qc)

      parseQcHeader                 :: CommentConfig -> Comment -> (CommentConfig, Comment)
      parseQcHeader cfg (Comment b s)= either (const (cfg, Comment b "")) (filterNested b) (parse (qcLine cfg) "comment" s)

      filterNested                  :: Bool -> (CommentConfig, String) -> (CommentConfig, Comment)
      filterNested isMulti (cfg, s)
         | commentIsNested cfg || null files || qcSrc qc `elem` files
                                                     = (cfg, Comment isMulti s)
         | otherwise                                 = (cfg, Comment isMulti "")

qcLine                              :: CommentConfig -> Parser (CommentConfig, String)
qcLine cfg                           = qcHeader cfg <|> tupleM (return cfg) qcString

qcString                            :: Parser String
qcString                             = char '%' >> many anyChar

qcHeader                            :: CommentConfig -> Parser (CommentConfig, String)
qcHeader cfg                         = do
                                          _ <- reserved "qidl"
                                          tupleM (option cfg qcPreset >>= qcCommentConfig) qcTail
   where
      qcTail                         = qcString <|> nextLine <|> return ""
      nextLine                       = manyTill anyChar (char '\n') >> many anyChar

qcPreset                            :: Parser CommentConfig
qcPreset                             = reserved "default"         `butReturn` defaultCommentConfig
                                   <|> try (reserved "c-comment") `butReturn` defaultCommentConfig
                                   <|> reserved "copyright"       `butReturn` copyrightCommentConfig

qcCommentConfig                     :: CommentConfig -> Parser CommentConfig
qcCommentConfig cfg                  = option cfg (try (commentAttr cfg) >>= qcCommentConfig)  -- "many commentAttr" with an accumulating cfg

commentAttr                         :: CommentConfig -> Parser CommentConfig
commentAttr cfg                      = qcLocation
                                   <|> qcNested
                                   <|> qcTarget
                                   <|> qcFold
   where
      qcLocation                     = reserved "location" >> eq >> isTop >>= \b  -> return cfg{commentIsTop=b}
      qcNested                       = reserved "nested"   >> eq >> bool  >>= \b  -> return cfg{commentIsNested=b}
      qcTarget                       = reserved "target"   >> eq >> langs >>= \xs -> return cfg{commentLangs=filter (/="all") xs}
      qcFold                         = reserved "fold"     >> eq >> bool  >>= \b  -> return cfg{commentIsFold=b}

      isTop                          = reserved "top"    `butReturn` True
                                   <|> reserved "bottom" `butReturn` False

      langs                          = identifier `sepBy1` comma

      bool                           = reserved "true"   `butReturn` True
                                   <|> reserved "false"  `butReturn` False

      eq                             = lexeme (char '=')
      comma                          = lexeme (char ',')

reserved                            :: String -> Parser String
reserved                             = lexeme . try . string

identifier                          :: Parser String
identifier                           = lexeme (many1 alphaNum)

lexeme                              :: Parser a -> Parser a
lexeme p                             = p `followedBy` many (char ' ')


-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause

module Language.Idl.DoxygenComment where

import Language.Idl.Data(
     Comment(Comment)
   )
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
   , noneOf
   )
import Text.ParserCombinators.Parsec.Combinator(
     many1
   )
import Data.List(
     intercalate
   )


insertComment :: String -> Int -> [Comment] -> [Comment]
insertComment _ _ [] = []
insertComment s i xs = init xs ++ [injectComment s i (last xs)]

injectComment :: String -> Int -> Comment -> Comment
injectComment s i (Comment b old) = Comment b (either noMatch match (parse commentP "tmp" old))
   where
      noMatch _err = old ++ "\n" ++ s ++ "\n"
      match x = injectParsedComment s i x

-- Inject comment at parameter 'i'.  If 'i' is negative, prepend the comment.
injectParsedComment :: String -> Int -> (String, String, [String], String) -> String
injectParsedComment s i (gal, '*':pre, ps, post)
   | i < 0                                = "* " ++ s' ++ "\n" ++ gal ++ pre ++ concat ps ++ post
   where
      s' = intercalate ("\n" ++ gal) (lines s)
injectParsedComment s i (gal,pre,ps,post) = pre ++ concat beforeI ++ s' ++ concat afterI ++ post
   where
      s' = unlines (map (gal ++) (lines s))
      (beforeI, afterI) = splitAt i ps

commentP :: Parser (String, String, [String], String)
commentP = do
      start <- notDoxyLine
      gal   <- galaxy
      desc  <- many notDoxyLine
      ps    <- many param
      post  <- many anyChar
      let pre = concat ((start ++ gal) : desc)
      return (gal, pre, ps, post)

-- Stop parsing as soon as a line has a '@', and then back up to the start of the line
notDoxy :: Parser String
notDoxy = do
      xs <- many notDoxyLine
      return (concat xs)

notDoxyLine :: Parser String
notDoxyLine = try $ do
      ss  <- many (many1 (noneOf "@\n") <|> ignorableTag)
      end <- string "\n"
      return (concat ss ++ end)
   where
      ignorableTag = try (string "@par ") <|> try (string "@brief ")

param :: Parser String
param = try $ do
      pre  <- galaxy
      p    <- string "@param"
      s    <- many1 (noneOf "@\n")
      end  <- string "\n"
      subs <- many (subParam pre)
      return (pre ++ p ++ s ++ end ++ concat subs)

subParam :: String -> Parser String
subParam gal = try $ do
      pre <- string gal
      s   <- many1 (noneOf "@\n")
      end <- string "\n"
      return (pre ++ s ++ end)

-- star-studded space
galaxy :: Parser String
galaxy = many (char ' ' <|> char '*')


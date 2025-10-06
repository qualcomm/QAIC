-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.SO.IdlBackend(soBackend) where

import qualified Language.SO.ToStub    as ToStub
import qualified Language.SO.ToCStub   as ToCStub
import qualified Language.SO.ToCSkel   as ToCSkel
import qualified Language.SO.ToSkel    as ToSkel
import qualified Language.SO.ToHeader  as ToHeader
import qualified Language.SO.Cfg       as Cfg
import Language.Idl.Data( Idl , PreDoc , PostDoc)
import Language.Palette.Data( Palette)
import Data.List(nubBy)
import Data.Function(on)
import Text.PrettyPrint(
     render
   , Doc
   )

import qualified Data.ByteString.Lazy.Char8 as BSC

type IsHeader                      = Bool
type IsSlim                        = Bool
type IsRemoting                    = Bool

-- | Generate .c and .h files from IDL
soBackend :: IsHeader -> IsRemoting -> IsSlim -> Int -> String -> [FilePath] -> (Idl, (PreDoc,PostDoc)) -> Palette -> Either String [(String, BSC.ByteString)]
soBackend isHeader isRemoting isSlim idt moduleName moduleFiles (idl,_) _ =
   let
         cfg = (Cfg.new idt moduleName moduleFiles idl)
   in    Right $ nubBy ((==) `on` fst) $
                 (headers isHeader moduleFiles cfg idl)
             ++  (stubs isRemoting isSlim moduleFiles cfg idl)
             ++  (skels isRemoting isSlim moduleFiles cfg idl)

stubs :: Bool -> Bool -> [FilePath] -> Cfg.Cfg -> Idl -> [(String, BSC.ByteString)]
stubs False _    _     _   _   = []
stubs True True files cfg idl = map (renderFile . ToHeader.ifdef) (ToStub.toStub cfg files idl)
stubs _    _    files cfg idl = map (renderFile . ToHeader.ifdef) (ToCStub.toStub cfg files idl)

skels :: Bool -> Bool -> [FilePath] -> Cfg.Cfg -> Idl -> [(String, BSC.ByteString)]
skels False _    _     _   _   = []
skels True True files cfg idl = map (renderFile . ToHeader.ifdef) (ToSkel.toSkel cfg files idl)
skels _ _       files cfg idl = map (renderFile . ToHeader.ifdef) (ToCSkel.toSkel cfg files idl)


headers :: IsHeader -> [FilePath] -> Cfg.Cfg -> Idl -> [(String, BSC.ByteString)]
headers True  paths cfg idl = map (renderFile . ToHeader.ifdef) (ToHeader.toHeader cfg paths idl)
headers False _  _     _   = []

-- | Render a filename and its contents from a name, extension and Doc
renderFile :: (String, Doc) -> (String, BSC.ByteString)
renderFile (nm,doc) = (nm, BSC.pack (render doc ++ "\n"))



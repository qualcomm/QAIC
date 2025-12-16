-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries. 
-- SPDX-License-Identifier: BSD-3-Clause-Clear

-- | Parsec parser combinators for Qualcomm Interface Description Language, QIDL

{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Language.Idl.Parser where

import Language.Idl.Data
import Language.Palette.Data(
     Palette
   )
import Language.Idl.ToPalette(
     paletteFromIdl
   )
import Text.ParserCombinators.Parsec.Prim(
     GenParser
   , runParser
   , (<|>)
   , (<?>)
   , many
   , try
   , getPosition
   , getState
   , setState
   , unexpected
   )
import Text.ParserCombinators.Parsec.Char(
     char
   , string
   , hexDigit
   , octDigit
   , digit
   , letter
   , oneOf
   , noneOf
   , alphaNum
   )
import Text.ParserCombinators.Parsec.Combinator(
     option
   , optionMaybe
   , many1
   , sepBy
   , sepBy1
   , between
   , eof
   )
import Text.ParserCombinators.Parsec.Expr(
     buildExpressionParser
   , Assoc(..)
   , Operator(..)
   )
import qualified Language.Idl.Token as Token
import Text.ParserCombinators.Parsec.Pos(
     sourceName
   , SourceName
   , SourcePos
   )
import Text.ParserCombinators.Parsec.Error(
     ParseError
   )
import Control.Monad(
     liftM
   , liftM2
   )
import Control.MonadUtils(
     singletonM
   , consM
   , concatM
   , tupleM
   , appendM
   , butReturn
   , followedBy
   )
import Data.List(
     groupBy
   , partition
   , isPrefixOf
   )
import Data.ListUtils(
     headDef
   , notNull
   )
import Data.Char(
     chr
   )
import System.FilePathUtils(
     forceForwardSlashes
   )
import Language.Idl.ParserState(
     ParserState
   )
import Language.Idl.DoxygenComment(
     insertComment
   )
-- Control.Monad.Error is deprecated in GHC 9.10, Either String already has a Monad instance
import Control.Monad.Fail(MonadFail(..))

-- Define a MonadFail instance for Either String in GHC 9.10
instance MonadFail (Either String) where
  fail = Left

type NamedDef                        = (Identifier, Definition, ParamComment)
type ParamComment                    = Maybe (String, Int)
type IdlParser                       = FilePath -> String -> Either String Idl

-- We redefine 'Parser' here so that we can add state to the parser without disrupting any dependencies
type Parser a                        = GenParser Char ParserState a

-- | Parses an IDL file and transforms it into the Palette interface language
palette                             :: Bool -> [FilePath] -> String -> Either String Palette
palette ignoreUndef paths input      = parseIdl (headDef "tmp" paths) input >>= paletteFromIdl ignoreUndef paths Nothing

-- | Run the IDL parser and return a data structure 'Idl' or an error message
parseIdl                            :: FilePath -> String -> Either String Idl
parseIdl src                         = showIfError . parse idl src
   where
      showIfError                   :: Show a => Either a b -> Either String b
      showIfError                    = either (Left . show) Right

parse                               :: Parser a -> SourceName -> [Char] -> Either ParseError a
parse p                              = runParser p []

-- | Parses and IDL file and returns a data structure 'Idl'
idl                                 :: Parser Idl
idl                                  = do
                                          whiteSpace
                                          preBlk  <- toplevelCommentBlock
                                          defss   <- many topLevelDeclaration
                                          postBlk <- toplevelCommentBlock
                                          eof
                                          return (Idl (preBlk ++ concat defss ++ postBlk))

toplevelCommentBlock                :: Parser [TopLevelDeclaration]
toplevelCommentBlock                 = liftM (map mkTop) comments
   where
      mkTop (pos, cms)               = TopLevelDeclaration (forceForwardSlashes (sourceName pos)) (CommentBlock cms)

commentBlock                        :: Parser [Declaration]
commentBlock                         = liftM (map (CommentBlock . snd)) comments

-- | Parses a declaration and returns it, along with the name of the source
--   file it came from.
topLevelDeclaration                 :: Parser [TopLevelDeclaration]
topLevelDeclaration                  = do
                                          -- 'getPosition' is a lexeme instead of 'declaration' because
                                          -- chewing up the whitespace after the declaration can potentially
                                          -- encounter preprocessor instructions and change the file position.
                                          -- If lexeme is changed to chew whitespace before the parser instead
                                          -- of after, then this special handling can be removed.
                                          pre  <- predocs
                                          junk <- toplevelCommentBlock
                                          ds   <- declaration'
                                          pos  <- lexeme getPosition
                                          post <- postdocs pos
                                          let srcName = forceForwardSlashes (sourceName pos)
                                              decls   = [Declaration pos (addComment cm pre) post nm def | (nm, def, cm) <- ds]
                                          return (junk ++ map (TopLevelDeclaration srcName) decls)

comments                            :: Parser [(SourcePos, [Comment])]
comments                             = do
                                          xs <- getState
                                          setState []
                                          return (cluster (reverse xs))
   where
      cluster                        = filter (notNull . snd) . map merge . groupBy (\a b -> src a == src b)

      merge                         :: [(SourcePos, Either String String)] -> (SourcePos, [Comment])
      merge xs                       = (fst (head xs), concatMap mkComment xs)

      src                            = forceForwardSlashes . sourceName . fst

predocs                             :: Parser [Comment]
predocs                              = do
                                          pos <- getPosition
                                          xs  <- getState
                                          let (good, bad) = partition (doxy (srcName pos)) xs
                                          setState bad
                                          return (reverse (concatMap mkComment good))
   where
      srcName                        = forceForwardSlashes . sourceName
      doxy src (pos, Left (x:y:_))   = src == srcName pos && (((x == '*' || x == '!') && y /= '<') || groupDcl x y)
      doxy src (pos, Right (x:y:_))  = src == srcName pos && (((x == '/' || x == '!') && y /= '<') || groupDcl x y)
      doxy _   _                     = False

postdocs                            :: SourcePos -> Parser [Comment]
postdocs pos                         = do
                                          xs  <- getState
                                          let (good, bad) = partition inlineDoxy xs
                                          setState bad
                                          return (reverse (concatMap mkComment good))
   where
      inlineDoxy (p, Left (x:y:_))   = sourceName pos == sourceName p && (x == '*' || x == '!') && y == '<'
      inlineDoxy (p, Right (x:y:_))  = sourceName pos == sourceName p && (x == '/' || x == '!') && y == '<'
      inlineDoxy _                   = False

mkComment                           :: (SourcePos, Either String String) -> [Comment]
mkComment (_pos, str)
   | validateComment str             = [either (Comment True) (Comment False) str]
   | otherwise                       = []

validateComment                     :: Either String String -> Bool
validateComment (Left  ('*':_))      = True                     -- Doxygen Multi-line
validateComment (Left  ('!':_))      = True                     -- Doxygen
validateComment (Right ('/':_))      = True                     -- Doxygen Single-line
validateComment (Right ('!':_))      = True                     -- Doxygen
validateComment (Left  ('@':'{':_))  = True                     -- Doxygen Group Declaration Start
validateComment (Left  ('@':'}':_))  = True                     -- Doxygen Group Declaration End
validateComment (Right ('@':'{':_))  = True                     -- Doxygen Group Declaration Start
validateComment (Right ('@':'}':_))  = True                     -- Doxygen Group Declaration End
validateComment (Left  ('%':_))      = True                     -- qidl-comment multi-line
validateComment (Right ('%':_))      = True                     -- qidl-comment single-line
validateComment (Left str)           = "qidl" `isPrefixOf` str  -- qidl-comment multi-line header
validateComment (Right str)          = "qidl" `isPrefixOf` str  -- qidl-comment single-line header

groupDcl                            :: Char -> Char -> Bool
groupDcl '@' '{'                     = True
groupDcl '@' '}'                     = True
groupDcl _ _                         = False

-- | Parses a single declaration.  Returns a list of declarations, because a declaration with
--   multiple declarators is split up, returning one declaration for each declarator.
declaration                         :: Parser [Declaration]
declaration                          = mkDeclaration declaration'

declaration'                        :: Parser [NamedDef]
declaration'                         = typeDcl `followedBy` semi'
                                   <|> singletonM constant `followedBy` semi'
                                   <|> singletonM interface `followedBy` semi'
                                   <|> singletonM moduleDcl `followedBy` semi'
   where
      semi'                          = char ';'

-- | Makes a Declaration parser from a NamedDef parser.  The Declaration parser
--   adds source position, pre-documentation, and post-documentation
mkDeclaration                       :: Parser [NamedDef] -> Parser [Declaration]
mkDeclaration decl                   = do pre  <- predocs
                                          _jnk <- commentBlock
                                          ds   <- decl
                                          pos  <- lexeme getPosition
                                          post <- postdocs pos
                                          return [Declaration pos (addComment cm pre) post nm def | (nm, def, cm) <- ds]

-- | Adds a doxygen parameter comment if there are already doxygen comments
addComment                          :: ParamComment -> [Comment] -> [Comment]
addComment Nothing                   = id
addComment (Just (s, i))             = insertComment s i

-- | Parses a type declaration, such as a type alias, struct, union, or enum
typeDcl                             :: Parser [NamedDef]
typeDcl                              = typeAlias
                                   <|> singletonM (structType <|> unionType <|> enumType <|> nativeType)

-- | Parses a type alias, aka typedef
typeAlias                           :: Parser [NamedDef]
typeAlias                            = reserved "typedef" >> typeDeclarator

nativeType                          :: Parser NamedDef
nativeType                           = do
                                          nm <- reserved "native" >> identifier
                                          return (nm, TypeDcl (Native nm), Nothing)

-- | Consumes the array sizes into a type
mkType                 :: Type -> [ConstExpr] -> Type
mkType                  = foldr Array


-- | Parses a type spec followed by a list of declarators
typeDeclarator                      :: Parser [NamedDef]
typeDeclarator                       = do
                                          (nm, t) <- typeSpec
                                          ds <- declarators nm
                                          return (map (mkTypeDecl t) ds)
   where

       mkTypeDecl               :: Type -> (Identifier, [ConstExpr]) -> NamedDef
       mkTypeDecl ty (nm, sz)    = (nm, TypeDcl (mkType ty sz), Nothing)

-- | Parses a list of declarators
declarators                         :: Maybe Identifier -> Parser [(Identifier, [ConstExpr])]
declarators tyNm                     = declarator tyNm `sepBy1` comma

-- | Parses a declarator
declarator                          :: Maybe Identifier -> Parser (Identifier, [ConstExpr])
declarator tyNm                      = do
                                          i <- identifier
                                          validate tyNm i
                                          manySz <- many fixedArraySize
                                          return (i, manySz)
   where
      validate (Just nm) i
          | nm == i                  = unexpected ("name clash with earlier type definition '" ++ unwrapId nm ++ "'")
      validate _ _                   = return ()

-- | Parses an array size enclosed in angle brackets
fixedArraySize                      :: Parser ConstExpr
fixedArraySize                       = brackets positiveIntConst

-- | Parses a type spec
typeSpec                            :: Parser (Maybe Identifier, Type)
typeSpec                             = liftM constr constrTypeSpec
                                   <|> liftM simple simpleTypeSpec
   where
      constr (nm, ty)                = (Just nm, ty)
      simple ty                      = (Nothing, ty)

-- | Parses a simple type spec or a name that references a type spec
simpleTypeSpec                      :: Parser Type
simpleTypeSpec                       = liftM PrimType baseTypeSpec
                                   <|> interfaceType
                                   <|> templateTypeSpec
                                   <|> scopedNameType

-- | Parses a struct, union, or enum type declaration
constrTypeSpec                      :: Parser (Identifier, Type)
constrTypeSpec                       = do
                                          (nm, TypeDcl ty, _) <- (structType <|> unionType <|> enumType)
                                          return (nm, ty)

-- | Parses a struct type declaration
structType                          :: Parser NamedDef
structType                           = do
                                          reserved "struct"
                                          nm <- identifier
                                          ms <- braces (concatM (many1 member))
                                          return (nm, TypeDcl (Struct (unwrapId nm) ms), Nothing)

-- | Parses a struct member.  Because IDL allows multiple names to be associated
--   with each type declaration, one 'Member' is returned for each declarator
member                              :: Parser [Member]
member                               = do
                                          (nm, t) <- typeSpec
                                          pos     <- getPosition
                                          ds      <- declarators nm
                                          _       <- semi
                                          post    <- postdocs pos
                                          return (map (mkMember t pos post) ds)

-- | Makes a 'Member' from a tye and a declarator
mkMember                            :: Type -> SourcePos -> [Comment] -> (Identifier, [ConstExpr]) -> Member
mkMember ty pos post (nm, sz)       = Member pos post nm (mkType ty sz)


unwrapId                            :: Identifier -> String
unwrapId (Identifier nm)             = nm

-- | Parses a union type declaration
unionType                           :: Parser NamedDef
unionType                            = do
                                          reserved "union"
                                          nm <- identifier
                                          reserved "switch"
                                          t   <- parens switchTypeSpec
                                          pos <- getPosition
                                          (cs,def) <- braces unionCases
                                          return (nm, TypeDcl (Union pos (unwrapId nm) t cs def), Nothing)

-- | Parses the switch of a union declaration
switchTypeSpec                      :: Parser Type
switchTypeSpec                       = liftM PrimType (
                                              integerType
                                          <|> charType
                                          <|> booleanType
                                       )
                                   <|> do { (_, TypeDcl ty, _) <- enumType
                                          ; return ty
                                          }
                                   <|> scopedNameType


-- | Parses the body of a union declaration
unionCases                          :: Parser ([UnionCase], Maybe DefaultCase)
unionCases                           = do
                                          cs  <- many unionCase
                                          def <- optionMaybe defaultCase
                                          return (cs,def)

-- | Parses a union case
unionCase                           :: Parser UnionCase
unionCase                            = liftM2 UnionCase (many1 caseLabel) elementSpec `followedBy` semi

-- | Parses the default case for a union declaration
defaultCase                         :: Parser DefaultCase
defaultCase                          = reserved "default" >> colon >> (elementSpec `followedBy` semi)

-- | Parses a union declaration's case label
caseLabel                           :: Parser ConstExpr
caseLabel                            = reserved "case" >> (constExpr `followedBy` colon)

-- | Parses a union case's type and declarator
elementSpec                         :: Parser Member
elementSpec                          = do
                                          (nm, ty) <- typeSpec
                                          pos      <- getPosition
                                          decl     <- declarator nm
                                          return (mkMember ty pos [] decl)

-- | Parses an enum declaration
enumType                            :: Parser NamedDef
enumType                             = do
                                          reserved "enum"
                                          i  <- identifier
                                          es <-  braces enumMembers
                                          return (i, TypeDcl (Enum (unwrapId i) es), Nothing)

enumMembers                         :: Parser [(PostDoc, String)]
enumMembers                          = do
                                          pos      <- getPosition
                                          i        <- identifier
                                          p1       <- postdocs pos
                                          (p2, es) <- option ([], []) nextEnumMember
                                          return ((p1++p2, unwrapId i) : es)

nextEnumMember                      :: Parser (PostDoc, [(PostDoc, String)])
nextEnumMember                       = do
                                          pos  <- getPosition
                                          _    <- comma
                                          prev <- postdocs pos
                                          es   <- enumMembers
                                          return (prev, es)

-- | Parses a reference to some previously defined type
scopedNameType                      :: Parser Type
scopedNameType                       = do
                                          notNil <- option False (reserved "notnil" `butReturn` True)
                                          nm     <- scopedName
                                          pos    <- getPosition
                                          return (TypeRef pos notNil nm)
                                   <?> "scoped name"

-- | Parses a list of identifiers with \"::\" between each
scopedName                          :: Parser ScopedName
scopedName                           = do
                                          gbl <- option [] (try (string "::") `butReturn` [""])
                                          nms <- liftM unwrapId identifier `sepBy` try (string "::")
                                          return (gbl ++ nms)
                                   <?> "scoped name"

-- | Parses the name of a primitive type
baseTypeSpec                        :: Parser Prim
baseTypeSpec                         = floatingPtType
                                   <|> integerType
                                   <|> charType
                                   <|> wideCharType
                                   <|> booleanType
                                   <|> octetType
                                   -- <|> anyType
                                   -- <|> objectType
                                   -- <|> valueBaseType

-- | Parses a templated type spec
templateTypeSpec                    :: Parser Type
templateTypeSpec                     = sequenceType
                                   <|> stringType
                                   <|> wideStringType
                                   <|> dmahandleType
                                   {- <|> fixedPtType -}

-- | Parses a sequence type
sequenceType                        :: Parser Type
sequenceType                         = do
                                          _ <- reserved "sequence" >> lAngle
                                          ty  <- simpleTypeSpec
                                          pos <- getPosition
                                          sz  <- sizeOrNot pos
                                          return (Sequence sz ty)
   where
      sizeOrNot pos                  = size <|> rAngle `butReturn` zero pos
      size                           = comma >> (positiveIntConst `followedBy` rAngle)
      zero pos                       = ConstExpr pos (LiteralExpr (IntLiteral Dec 0))

-- | Parses an expression that reduces to a positive integer
positiveIntConst                    :: Parser ConstExpr
positiveIntConst                     = constExpr

-- | Parses the 'Interface' type, a QC extension to IDL
interfaceType                       :: Parser Type
interfaceType                        = reserved "interface" >> return Interface

-- | Parses a string type
stringType                          :: Parser Type
stringType                           = reserved "string" >> liftM StringType sizeTemplate

-- | Parses a wide string type
wideStringType                      :: Parser Type
wideStringType                       = reserved "wstring" >> liftM WideStringType sizeTemplate

-- | Parses a dmahandle type
dmahandleType                       :: Parser Type
dmahandleType                       = reserved "dmahandle" >> liftM DmahandleType sizeTemplate

-- | Parses a positive integer enclosed in angle brackets.  Return the integer if it exists, else zero.
sizeTemplate                        :: Parser ConstExpr
sizeTemplate                         = do
                                          pos <- getPosition
                                          option (zero pos) (angles positiveIntConst)
   where
      zero p                         = ConstExpr p (LiteralExpr (IntLiteral Dec 0))

-- | Parses a float type
floatingPtType                      :: Parser Prim
floatingPtType                       = reserved "float"  `butReturn` FloatType
                                   <|> reserved "double" `butReturn` DoubleType
                                   -- <|> reservedList ["long", "double"] `butReturn` LongDoubleType
                                   <?> "floating point type"

-- | Parses an integer type
integerType                         :: Parser Prim
integerType                          = reserved "short"                          `butReturn` SignedShortType
                                   <|> reservedList ["long", "long"]             `butReturn` SignedLongLongType
                                   <|> reserved "long"                           `butReturn` SignedLongType
                                   <|> reservedList ["unsigned", "short"]        `butReturn` UnsignedShortType
                                   <|> reservedList ["unsigned", "long", "long"] `butReturn` UnsignedLongLongType
                                   <|> reservedList ["unsigned", "long"]         `butReturn` UnsignedLongType
                                   <|> reserved "uint8_t"                        `butReturn` UnsignedCharFixedTType
                                   <|> reserved "uint16_t"                       `butReturn` UnsignedShortFixedTType
                                   <|> reserved "uint32_t"                       `butReturn` UnsignedLongFixedTType
                                   <|> reserved "uint64_t"                       `butReturn` UnsignedLongLongFixedTType
                                   <|> reserved "int8_t"                         `butReturn` SignedCharFixedTType
                                   <|> reserved "int16_t"                        `butReturn` SignedShortFixedTType
                                   <|> reserved "int32_t"                        `butReturn` SignedLongFixedTType
                                   <|> reserved "int64_t"                        `butReturn` SignedLongLongFixedTType
                                   <|> reserved "uint8"                        `butReturn` UnsignedCharFixedType
                                   <|> reserved "uint16"                       `butReturn` UnsignedShortFixedType
                                   <|> reserved "uint32"                       `butReturn` UnsignedLongFixedType
                                   <|> reserved "uint64"                       `butReturn` UnsignedLongLongFixedType
                                   <|> reserved "int8"                         `butReturn` SignedCharFixedType
                                   <|> reserved "int16"                        `butReturn` SignedShortFixedType
                                   <|> reserved "int32"                        `butReturn` SignedLongFixedType
                                   <|> reserved "int64"                        `butReturn` SignedLongLongFixedType
                                   <?> "integer type"

-- | Parses a character type
charType                            :: Parser Prim
charType                             = reserved "char" `butReturn` CharType

-- | Parses a wide character type
wideCharType                        :: Parser Prim
wideCharType                         = reserved "wchar" `butReturn` WideCharType

-- | Parses a Boolean type
booleanType                         :: Parser Prim
booleanType                          = reserved "boolean" `butReturn` BooleanType

-- | Parses an octet type
octetType                           :: Parser Prim
octetType                            = reserved "octet"    `butReturn` OctetType
                                   <?> "octet type"

-- | Parses a constant declaration
constant                            :: Parser NamedDef
constant                             = do
                                          reserved "const"
                                          t  <- constType
                                          nm <- identifier
                                          _ <- equal
                                          val <- constExpr
                                          return (nm, ConstDcl t val, Nothing)

-- | Parses any of the types which can be assigned a constant
constType                           :: Parser Type
constType                            = liftM PrimType (
                                               integerType
                                           <|> charType
                                           <|> wideCharType
                                           <|> booleanType
                                           <|> floatingPtType
                                           <|> octetType
                                        )
                                    <|> stringType
                                    <|> wideStringType
                                    <|> scopedNameType

constExpr                           :: Parser ConstExpr
constExpr                            = buildExpressionParser operators constTerm
   where
      operators                      = [ [prefix "-" Negate, prefix "~" Complement]
                                       , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft, binary "%" Modulo AssocLeft]
                                       , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
                                       , [binary "<<" LeftShift AssocLeft, binary ">>" RightShift AssocLeft]
                                       , [binary "&" BitwiseAnd AssocLeft]
                                       , [binary "^" BitwiseXor AssocLeft]
                                       , [binary "|" BitwiseOr AssocLeft]
                                       ]
      prefix s f                     = Prefix (do{ lexeme (reservedOp s)
                                                 ; pos <- getPosition
                                                 ; return (ConstExpr pos . f)
                                                 })
      binary s f                     = Infix  (do{lexeme (reservedOp s)
                                                 ; pos <- getPosition
                                                 ; return (\a b -> ConstExpr pos (f a b))
                                                 })

-- | Parses an expression that reduces to one of the 'constType' types
constTerm                           :: Parser ConstExpr
constTerm                            = do
                                          pos <- getPosition
                                          (parens constExpr
                                            <|> liftM (ConstExpr pos . LiteralExpr) literal
                                            <|> liftM (ConstExpr pos . ConstExprRef) scopedName
                                            )

-- | Parses a module declaration
moduleDcl                           :: Parser NamedDef
moduleDcl                            = do
                                         reserved "module"
                                         nm     <- identifier
                                         declss <- braces (many1 declaration)
                                         cms    <- commentBlock
                                         let decls = concat declss ++ cms
                                         return (nm, ModuleDcl decls, Nothing)

-- | Parses an interface declaration or a forward interface declaration
interface                           :: Parser NamedDef
interface                            = do
                                          isLocal <- option False (reserved "local" `butReturn` True)
                                          base <- option False (reserved "ext_params" `butReturn` True)
                                          reserved "interface"
                                          nm   <- identifier
                                          pos  <- getPosition
                                          (isForward, bs, ds)  <- option (True,(pos,Nothing),[]) (interfaceDcl (unwrapId nm))
                                          let iid = ConstExpr pos (ConstExprRef ["AEEIID_"++ unwrapId nm])
                                          return (nm, InterfaceDcl base isLocal isForward (Just iid) bs ds, Nothing)

-- | Parses an interface declaration
interfaceDcl                        :: String -> Parser (Bool, (SourcePos, Maybe ScopedName), [Declaration])
interfaceDcl nm                      = do
                                          pos  <- getPosition
                                          base <- optionMaybe baseInterface
                                          exs  <- braces (concatM (many export))
                                          cms  <- commentBlock
                                          return (False, (pos, base), addQueryInterface pos nm (exs++cms))
   where
      addQueryInterface pos "IQI" []= [qi pos]
      addQueryInterface _ _     xs   = xs
      qi pos                         = Declaration pos [qiComment] [] (Identifier "QueryInterface") (OperationDcl False Nothing Nothing (PrimType SignedLongType) [qiParam])
      qiComment                      = Comment True "*\n * Detect if an object implements the requested interface.\n *\n * @param iqi Requested interface.\n "
      qiParam                        = Parameter (Identifier "iqi") ParameterROut Interface

-- | Parses a colon followed by a reference to a base interface
baseInterface                       :: Parser ScopedName
baseInterface                        = colon >> scopedName
                                   <?> "parent interface name"

-- | Parses a single declaration.  Returns a list of declarations, because a declaration with
--   multiple declarators is split up, returning one declaration for each declarator.
export                              :: Parser [Declaration]
export                               = mkDeclaration export'

-- | Parses an interface export declaration
export'                             :: Parser [NamedDef]
export'                              = typeDcl `followedBy` semi'
                                   <|> singletonM constant `followedBy` semi'
                                   <|> attrDcl `followedBy` semi'
                                   <|> singletonM operation `followedBy` semi'
   where
      semi'                          = char ';'

-- | Parses a readonly or read/write interface attribute
attrDcl                             :: Parser [NamedDef]
attrDcl                              = readonlyAttrSpec
                                   <|> attrSpec

-- | Parses a readonly interface attribute declaration.
readonlyAttrSpec                    :: Parser [NamedDef]
readonlyAttrSpec                     = do
                                          pos <- getPosition
                                          let returnType = TypeRef pos False ["AEEResult"]
                                          reserved "readonly"
                                          reserved "attribute"
                                          t <- paramTypeSpec
                                          Identifier nm <- attrDeclarator  -- TODO: support multiple declarators
                                          return [(Identifier ("Get"++nm), OperationDcl False Nothing Nothing returnType [Parameter (Identifier "value") ParameterROut t], cm)]
   where
      cm                             = Just ("@param value Attribute value", 0)

-- | Parses an interface attribute declaration.  Returns \'Get\' and/or \'Set'\ method declarations
attrSpec                            :: Parser [NamedDef]
attrSpec                             = do
                                         pos <- getPosition
                                         let returnType = TypeRef pos False ["AEEResult"]
                                         reserved "attribute"
                                         t <- paramTypeSpec
                                         Identifier nm     <- attrDeclarator  -- TODO: support multiple declarators
                                         return [ (Identifier ("Get"++nm), OperationDcl False Nothing Nothing returnType [Parameter (Identifier "value") ParameterROut t], cm)
                                                , (Identifier ("Set"++nm), OperationDcl False Nothing Nothing returnType [Parameter (Identifier "value") ParameterIn t], cm)
                                                ]
   where
      cm                             = Just ("@param value Attribute value", 0)

-- | Parses an interface attribute declarator
attrDeclarator                      :: Parser Identifier
attrDeclarator                       = identifier -- TODO: many1 simpleDeclarator

-- | Parses an interface method
operation                           :: Parser NamedDef
operation                            = do
                                          base <- option False (reserved "async" `butReturn` True)
                                          opAttr   <- optionMaybe opAttribute
                                          ret      <- opTypeSpec
                                          funcName <- identifier
                                          pos  <- getPosition
                                          let iid = ConstExpr pos (ConstExprRef ["AEEIID_"++ unwrapId funcName])
                                          params   <- parens (paramDcl `sepBy` comma)
                                          return (funcName, OperationDcl base opAttr (Just iid)  ret params, Nothing)

-- | Parses a method's attribute, such as 'oneway' or 'signal'.
opAttribute                         :: Parser (SourcePos, Identifier)
opAttribute                          = tupleM getPosition (brackets identifier)
                                   <?> "method attribute"

-- | Parses the return type of an operation
opTypeSpec                          :: Parser Type
opTypeSpec                           = -- We don't use the return type, but still want to validate it
                                       reserved "void" `butReturn` PrimType SignedLongType
                                   <|> liftM snd typeSpec


-- | Parses an interface method's parameter
paramDcl                            :: Parser Parameter
paramDcl                             = do
                                          dir   <- paramAttribute
                                          t     <- paramTypeSpec
                                          i     <- identifier
                                          return (Parameter i dir t)

-- | Parses a method parameter's direction
paramAttribute                      :: Parser ParameterMode
paramAttribute                       = reserved "in"     `butReturn` ParameterIn
                                   <|> reserved "rout"   `butReturn` ParameterROut
                                   <|> reserved "inrout" `butReturn` ParameterInROut

-- | Parses a method parameter's type
paramTypeSpec                       :: Parser Type
paramTypeSpec                        = simpleTypeSpec

-- | Single-quoted character lexeme
charLiteral                         :: Parser Literal
charLiteral                          = lexeme (liftM CharLiteral (between (char '\'') (char '\'') character)) <?> "character literal"

-- | Single-quoted wide character lexeme
wcharLiteral                        :: Parser Literal
wcharLiteral                         = lexeme (liftM WCharLiteral (between (string "L'") (char '\'') wideCharacter)) <?> "wide character literal"

-- | Double-quoted string lexeme
stringLiteral                       :: Parser Literal
stringLiteral                        = lexeme (liftM StringLiteral (doubleQuoted (many character))) <?> "string literal"

-- | Double-quoted wide string lexeme
wstringLiteral                      :: Parser Literal
wstringLiteral                       = lexeme (liftM WStringLiteral (wideDoubleQuoted (many wideCharacter))) <?> "wide string literal"
   where
      wideDoubleQuoted               = between (string "L\"") (char '"')

-- | Oct literal lexeme
octLiteral                          :: Parser Literal
octLiteral                           = do
                                          s <- lexeme octString
                                          return (IntLiteral Oct (read ("0o" ++ s)))

-- | String of octal characters, starting with zero
octString                           :: Parser String
octString                            = char '0' >> many1 octDigit

-- | Hexadecimal literal lexeme
hexLiteral                          :: Parser Literal
hexLiteral                           = do
                                          s <- lexeme hexString
                                          return (IntLiteral Hex (read ("0x"++s)))

hexString                           :: Parser String
hexString                            = (try (char '0' >> oneOf "xX") >> many1 hexDigit)
                                   <?> "Hexadecimal number"

-- | Boolean lexeme
boolLiteral                         :: Parser Literal
boolLiteral                          = reserved "TRUE" `butReturn` BoolLiteral True
                                   <|> reserved "FALSE" `butReturn` BoolLiteral False
                                   <?> "Boolean literal"

-- | Integer lexeme
intLiteral                          :: Parser Literal
intLiteral                           = lexeme integer >>= return . IntLiteral Dec . read
                                   <?> "integer literal"

integer                             :: Parser String
integer                              = option "" (string "-") `appendM` nonnegativeInteger

-- | Float lexeme
floatLiteral                        :: Parser Literal
floatLiteral                         = do
                                          (pre, frac, expon) <- lexeme float
                                          return (FloatLiteral pre frac expon)
                                          <?> "float literal"

-- | Parse a float in the form 1.234e0.  Return the integer part, the fractional and the exponent
--   As defined by OMG 3.2.5.3, Floating-point Literals
float                               :: Parser (String, String, String)
float                                = try intAndExp
                                   <|> maybeIntAndFracAndMaybeExp
    where
       intAndExp                     = do
                                          i <- integer

                                          e <- expo
                                          return (i, "0", e)


       maybeIntAndFracAndMaybeExp    = do
                                          i <- option "0" integer
                                          f <- frac
                                          e <- option "0" expo
                                          return (i,f,e)

       frac                          = char '.' >> digits
       expo                          = oneOf "eE" >> integer
-- short   :: Parser String
-- short      =   manyN 5 digit
-- | Parses a literal
literal                             :: Parser Literal
literal                              = try octLiteral
                                   <|> hexLiteral
                                   <|> try floatLiteral
                                   <|> intLiteral
                                   <|> boolLiteral
                                   <|> charLiteral
                                   <|> try wcharLiteral
                                   <|> stringLiteral
                                   <|> try wstringLiteral
                                   <?> "literal"
-- | Parses any character that is not a backslash or double-quote
notBackslashOrQuote                 :: Parser Char
notBackslashOrQuote                  = noneOf "\"\\" <?> "non-backslash character"

-- | Parses any notBackslashOrQuote character or an escape sequence
character                           :: Parser Char
character                            = notBackslashOrQuote
                                   <|> charEscapeSequence
                                   <?> "character"

-- | Parses any notBackslashOrQuote character or a wide-char escape sequence
wideCharacter                       :: Parser Char
wideCharacter                        = notBackslashOrQuote
                                   <|> wideCharEscapeSequence
                                   <?> "wide character"

-- | Parses a wide char escape sequence
wideCharEscapeSequence              :: Parser Char
wideCharEscapeSequence               = char '\\' >> wideCharEscapableSequence
                                   <?> "wide character escape sequence"

-- | Parses a wide char escape sequence after the escape character
wideCharEscapableSequence           :: Parser Char
wideCharEscapableSequence            = charEscapableSequence
                                   <|> unicodeCharEncoding
                                   <?> "wide character escapable sequence"

-- | Parses a numeric character encoding.  i.e. u0F82
unicodeCharEncoding                 :: Parser Char
unicodeCharEncoding                  = liftM hexToChar (char 'u' >> many1N 4 hexDigit)
                                   <?> "numeric character encoding"

-- | Parses an 'x' followed by a sequence of one, or two hexadecimal digits, encodes the result in a char
hexCharEncoding                     :: Parser Char
hexCharEncoding                      = liftM hexToChar (char 'x' >> many1N 2 hexDigit)
                                   <?> "hexadecimal character encoding"

-- | Parses a sequence of one, two, or three octal digits, encodes the result in a char
octalCharEncoding                   :: Parser Char
octalCharEncoding                    = liftM octalToChar (many1N 3 octDigit)
                                   <?> "octal character encoding"

-- | Encode a string of hex characters to one 16-bit char
hexToChar                           :: String -> Char
hexToChar                            = chr . read . ("0x"++)

-- | Encode a string of oct characters to one 16-bit char
octalToChar                         :: String -> Char
octalToChar                          = chr . read . ("0o"++)

-- | Same as 'many1', but no more than N
many1N                              :: Int -> Parser a -> Parser [a]
many1N n p                           = p `consM` manyN (n - 1) p

-- | Same as 'many', but no more than N
manyN                               :: Int -> Parser a -> Parser [a]
manyN 0 _                            = return []
manyN n p                            = option [] (p `consM` manyN (n - 1) p)

-- | Parses a character escape sequence
charEscapeSequence                  :: Parser Char
charEscapeSequence                   = char '\\' >> charEscapableSequence <?> "character escape sequence"

-- | Parses a character escape sequence after the escape character
charEscapableSequence               :: Parser Char
charEscapableSequence                = escChar '\\' '\\'
                                   <|> escChar '\'' '\''
                                   <|> escChar '"'  '\"'
                                   <|> escChar 'r'  '\r'
                                   <|> escChar 'n'  '\n'
                                   <|> escChar 'a'  '\a'
                                   <|> escChar 'b'  '\b'
                                   <|> escChar 'f'  '\f'
                                   <|> escChar 't'  '\t'
                                   <|> escChar 'v'  '\v'
                                   <|> escChar '?'  '?'
                                   <|> octalCharEncoding
                                   <|> hexCharEncoding
                                   <?> "character escapable sequence"
   where
      escChar ch esc                 = char ch >> return esc

-- | Parses a positive integer or zero
nonnegativeInteger                  :: Parser String
nonnegativeInteger                   = digits
                                     <?> "nonnegativeInteger"
-- | Parses one or more digits
digits                               :: Parser String
digits                                = many1 digit
                                    <?> "digits"

-- | Parses an '=' character and any whitespace after it
equal                               :: Parser String
equal                                = lexeme (string "=")

-- | Create a parser that parses an opening double-quote followed by the input
--   parser followed by an closing double-quote.  Return the result from the
--   input parser.
doubleQuoted                        :: Parser a -> Parser a
doubleQuoted                         = between (char '"') (char '"')

-- | Left angle bracket lexeme
lAngle                              :: Parser Char
lAngle                               = lexeme (char '<')

-- | Right angle bracket lexeme
rAngle                              :: Parser Char
rAngle                               = lexeme (char '>')

-- | Create a parser that first parses the input parser and then drops any
--   trailing whitespace
lexeme                              :: Parser a -> Parser a
lexeme                               = Token.lexeme tokenizer

-- | Create a parser that parses a list of reserved words
reservedList                        :: [String] -> Parser ()
reservedList                         = try . mapM_ reserved

-- | Create a parser that parses a reserved word
reserved                            :: String -> Parser ()
reserved                             = Token.reserved tokenizer

-- | Create a parser that parses a reserved operation
reservedOp                          :: String -> Parser ()
reservedOp                           = Token.reservedOp tokenizer

-- | Create a parser that parses an opening angle bracket followed by the input
--   parser followed by an closing angle bracket.  Return the result from the
--   input parser.
angles                              :: Parser a -> Parser a
angles                               = Token.angles tokenizer

-- | Create a parser that parses an opening brace followed by the input parser
--   followed by an closing brace.  Return the result from the input parser.
braces                              :: Parser a -> Parser a
braces                               = Token.braces tokenizer

-- | Create a parser that parses an opening parenthesis followed by the input
--   parser followed by an closing parenthesis.  Return the result from the
--   input parser.
parens                              :: Parser a -> Parser a
parens                               = Token.parens tokenizer

-- | Create a parser that parses an opening square bracket followed by the input
--   parser followed by an closing square bracket.  Return the result from the
--   input parser.
brackets                            :: Parser a -> Parser a
brackets                             = Token.brackets tokenizer

-- | Comma lexeme
comma                               :: Parser String
comma                                = Token.comma tokenizer

-- | Colon lexeme
colon                               :: Parser String
colon                                = Token.colon tokenizer

-- | Semicolon lexeme
semi                                :: Parser String
semi                                 = Token.semi tokenizer

-- | Parses an identifier
identifier                          :: Parser Identifier
identifier                           = liftM Identifier (Token.identifier tokenizer)

-- | WhiteSpace parser
whiteSpace                          :: Parser ()
whiteSpace                           = Token.whiteSpace tokenizer

-- | A collection of lexemes that ignore whitespace and comments
tokenizer                           :: Token.TokenParser ParserState
tokenizer                            = Token.makeTokenParser Token.LanguageDef{
       Token.commentStart            = "/*"
     , Token.commentEnd              = "*/"
     , Token.commentLine             = "//"
     , Token.nestedComments          = False
     , Token.identStart              = letter <|> char '_'
     , Token.identLetter             = alphaNum <|> oneOf "_'"
     , Token.opStart                 = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , Token.opLetter                = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , Token.reservedOpNames         = []
     , Token.reservedNames           = reservedNames
     , Token.caseSensitive           = True
     }
   where
      reservedNames                  = [ "typedef"
                                       , "struct"
                                       , "union"
                                       , "switch"
                                       , "default"
                                       , "case"
                                       , "enum"
                                       , "notnil"
                                       , "sequence"
                                       , "interface"
                                       , "string"
                                       , "wstring"
                                       , "float"
                                       , "double"
                                       , "short"
                                       , "long"
                                       , "uint8_t"
                                       , "uint16_t"
                                       , "uint32_t"
                                       , "uint64_t"
                                       , "int8_t"
                                       , "int16_t"
                                       , "int32_t"
                                       , "int64_t"
                                       , "uint8"
                                       , "uint16"
                                       , "uint32"
                                       , "uint64"
                                       , "int8"
                                       , "int16"
                                       , "int32"
                                       , "int64"
                                       , "unsigned"
                                       , "char"
                                       , "wchar"
                                       , "boolean"
                                       , "octet"
                                       , "const"
                                       , "module"
                                       , "readonly"
                                       , "attribute"
                                       , "void"
                                       , "in"
                                       , "rout"
                                       , "inrout"
                                       , "out"
                                       , "inout"
                                       , "TRUE"
                                       , "FALSE"
                                       , "local"
                                       , "native"
                                       , "async"
                                       ]


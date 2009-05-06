{-# LANGUAGE NoMonomorphismRestriction #-}

module Atomo.Parser where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive

import Control.Monad
import Control.Monad.Error
import Data.List (intercalate, nub, sort)
import Data.Char (isAlpha, toLower, toUpper, isSpace, digitToInt)
import Debug.Trace
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

dump s x = trace (s ++ ": " ++ show x) (return ())
debug x = trace (show x) x

-- Custom makeTokenParser with tweaked whiteSpace rules
makeTokenParser languageDef
    = P.TokenParser{ P.identifier = identifier
                   , P.reserved = reserved
                   , P.operator = operator
                   , P.reservedOp = reservedOp

                   , P.charLiteral = charLiteral
                   , P.stringLiteral = stringLiteral
                   , P.natural = natural
                   , P.integer = integer
                   , P.float = float
                   , P.naturalOrFloat = naturalOrFloat
                   , P.decimal = decimal
                   , P.hexadecimal = hexadecimal
                   , P.octal = octal

                   , P.symbol = symbol
                   , P.lexeme = lexeme
                   , P.whiteSpace = whiteSpace

                   , P.parens = parens
                   , P.braces = braces
                   , P.angles = angles
                   , P.brackets = brackets
                   , P.squares = brackets
                   , P.semi = semi
                   , P.comma = comma
                   , P.colon = colon
                   , P.dot = dot
                   , P.semiSep = semiSep
                   , P.semiSep1 = semiSep1
                   , P.commaSep = commaSep
                   , P.commaSep1 = commaSep1
                   }
    where

    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens p        = between (symbol "(") (symbol ")") p
    braces p        = between (symbol "{") (symbol "}") p
    angles p        = between (symbol "<") (symbol ">") p
    brackets p      = between (symbol "[") (symbol "]") p

    semi            = symbol ";"
    comma           = symbol ","
    dot             = symbol "."
    colon           = symbol ":"

    commaSep p      = sepBy p comma
    semiSep p       = sepBy p semi

    commaSep1 p     = sepBy1 p comma
    semiSep1 p      = sepBy1 p semi


    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    charLiteral     = lexeme (between (char '\'')
                                      (char '\'' <?> "end of character")
                                      characterChar )
                    <?> "character"

    characterChar   = charLetter <|> charEscape
                    <?> "literal character"

    charEscape      = do{ char '\\'; escapeCode }
    charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))



    stringLiteral   = lexeme (
                      do{ str <- between (char '"')
                                         (char '"' <?> "end of string")
                                         (many stringChar)
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "literal string")

    stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = do{ char '\\'
                        ;     do{ escapeGap  ; return Nothing }
                          <|> do{ escapeEmpty; return Nothing }
                          <|> do{ esc <- escapeCode; return (Just esc) }
                        }

    escapeEmpty     = char '&'
    escapeGap       = do{ many1 space
                        ; char '\\' <?> "end of string gap"
                        }



    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do{ char '^'
                        ; code <- upper
                        ; return (toEnum (fromEnum code - fromEnum 'A'))
                        }

    charNum         = do{ code <- decimal
                                  <|> do{ char 'o'; number 8 octDigit }
                                  <|> do{ char 'x'; number 16 hexDigit }
                        ; return (toEnum (fromInteger code))
                        }

    charEsc         = choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ char c; return code }

    charAscii       = choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = try (do{ string asc; return code })


    -- escape code tables
    escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    naturalOrFloat  = lexeme (natFloat) <?> "number"

    float           = lexeme floating   <?> "float"
    integer         = lexeme int        <?> "integer"
    natural         = lexeme nat        <?> "natural"


    -- floats
    floating        = do{ n <- decimal
                        ; fractExponent n
                        }


    natFloat        = do{ char '0'
                        ; zeroNumFloat
                        }
                      <|> decimalFloat

    zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                         ; return (Left n)
                         }
                    <|> decimalFloat
                    <|> fractFloat 0
                    <|> return (Left 0)

    decimalFloat    = do{ n <- decimal
                        ; option (Left n)
                                 (fractFloat n)
                        }

    fractFloat n    = do{ f <- fractExponent n
                        ; return (Right f)
                        }

    fractExponent n = do{ fract <- fraction
                        ; expo  <- option 1.0 exponent'
                        ; return ((fromInteger n + fract)*expo)
                        }
                    <|>
                      do{ expo <- exponent'
                        ; return ((fromInteger n)*expo)
                        }

    fraction        = do{ char '.'
                        ; digits <- many1 digit <?> "fraction"
                        ; return (foldr op 0.0 digits)
                        }
                      <?> "fraction"
                    where
                      op d f    = (f + fromIntegral (digitToInt d))/10.0

    exponent'       = do{ oneOf "eE"
                        ; f <- sign
                        ; e <- decimal <?> "exponent"
                        ; return (power (f e))
                        }
                      <?> "exponent"
                    where
                       power e  | e < 0      = 1.0/power(-e)
                                | otherwise  = fromInteger (10^e)


    -- integers and naturals
    int             = do{ f <- sign
                        ; n <- nat
                        ; return (f n)
                        }

    sign            =   (char '-' >> return negate)
                    <|> (char '+' >> return id)
                    <|> return id

    nat             = zeroNumber <|> decimal

    zeroNumber      = do{ char '0'
                        ; hexadecimal <|> octal <|> decimal <|> return 0
                        }
                      <?> ""

    decimal         = number 10 digit
    hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
    octal           = do{ oneOf "oO"; number 8 octDigit  }

    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------
    reservedOp name =
        lexeme $ try $
        do{ string name
          ; notFollowedBy (P.opLetter languageDef) <?> ("end of " ++ show name)
          }

    operator =
        lexeme $ try $
        do{ name <- oper
          ; if (isReservedOp name)
             then unexpected ("reserved operator " ++ show name)
             else return name
          }

    oper =
        do{ c <- (P.opStart languageDef)
          ; cs <- many (P.opLetter languageDef)
          ; return (c:cs)
          }
        <?> "operator"

    isReservedOp name =
        isReserved (sort (P.reservedOpNames languageDef)) name


    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------
    reserved name =
        lexeme $ try $
        do{ caseString name
          ; notFollowedBy (P.identLetter languageDef) <?> ("end of " ++ show name)
          }

    caseString name
        | P.caseSensitive languageDef  = string name
        | otherwise               = do{ walk name; return name }
        where
          walk []     = return ()
          walk (c:cs) = do{ caseChar c <?> msg; walk cs }

          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                      | otherwise  = char c

          msg         = show name


    identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName name)
             then unexpected ("reserved word " ++ show name)
             else return name
          }


    ident
        = do{ c <- P.identStart languageDef
            ; cs <- many (P.identLetter languageDef)
            ; return (c:cs)
            }
        <?> "identifier"

    isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | P.caseSensitive languageDef  = name
                        | otherwise               = map toLower name


    isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

    theReservedNames
        | P.caseSensitive languageDef  = sortedNames
        | otherwise               = map (map toLower) sortedNames
        where
          sortedNames   = sort (P.reservedNames languageDef)



    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    symbol name
        = lexeme (string name)

    lexeme p
        = do{ x <- p; spacing; return x  }


    --whiteSpace
    whiteSpace = do spacing
                    skipMany (try $ spacing >> newline)
                    spacing

-- Atomo parser
atomo :: P.TokenParser st
atomo = makeTokenParser atomoDef

-- Atomo language definition
atomoDef :: P.LanguageDef st
atomoDef = P.LanguageDef { P.commentStart    = "{-"
                         , P.commentEnd      = "-}"
                         , P.commentLine     = "--"
                         , P.nestedComments  = True
                         , P.identStart      = letter
                         , P.identLetter     = alphaNum <|> satisfy ((> 0x80) . fromEnum)
                         , P.opStart         = letter <|> P.opLetter atomoDef
                         , P.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , P.reservedOpNames = ["=", "=>", "->", "::"]
                         , P.reservedNames   = ["if", "else", "elseif", "while",
                                                "do", "class", "data", "type",
                                                "where", "module", "infix",
                                                "infixl", "infixr", "import",
                                                "return"]
                         , P.caseSensitive   = True
                         }

whiteSpace    = P.whiteSpace atomo
simpleSpace   = skipMany1 $ satisfy (`elem` " \t\f\v\xa0")
spacing = skipMany spacing1
spacing1 | noLine && noMulti  = simpleSpace <?> ""
         | noLine             = simpleSpace <|> multiLineComment <?> ""
         | noMulti            = simpleSpace <|> oneLineComment <?> ""
         | otherwise          = simpleSpace <|> oneLineComment <|> multiLineComment <?> ""
         where
             noLine  = null (P.commentLine atomoDef)
             noMulti = null (P.commentStart atomoDef)
oneLineComment = try (string (P.commentLine atomoDef)) >> skipMany (satisfy (/= '\n'))
multiLineComment = try (string (P.commentStart atomoDef)) >> inComment
inComment | P.nestedComments atomoDef  = inCommentMulti
          | otherwise                  = inCommentSingle

inCommentMulti = (try (string (P.commentEnd atomoDef)) >> return ())
             <|> (multiLineComment >> inCommentMulti)
             <|> (skipMany1 (noneOf startEnd) >> inCommentMulti)
             <|> (oneOf startEnd >> inCommentMulti)
                 <?> "end of comment"
               where
                   startEnd = nub (P.commentEnd atomoDef ++ P.commentStart atomoDef)

inCommentSingle = (try (string (P.commentEnd atomoDef)) >> return ())
              <|> (skipMany1 (noneOf startEnd) >> inCommentSingle)
              <|> (oneOf startEnd >> inCommentSingle)
                  <?> "end of comment"
                where
                    startEnd   = nub (P.commentEnd atomoDef ++ P.commentStart atomoDef)

parens        = P.parens atomo
brackets      = P.brackets atomo
braces        = P.braces atomo
comma         = P.comma atomo
commaSep      = P.commaSep atomo
commaSep1     = P.commaSep1 atomo
colon         = char ':'
eol           = newline >> return ()
dot           = P.dot atomo
identifier    = P.identifier atomo
ident         = do c <- P.identStart atomoDef
                   cs <- many (P.identLetter atomoDef)
                   return (c:cs)
operator      = P.operator atomo
reserved      = P.reserved atomo
reservedOp    = P.reservedOp atomo
integer       = P.integer atomo
float         = P.float atomo
charLit       = P.charLiteral atomo
natural       = P.natural atomo
symbol        = P.symbol atomo
stringLiteral = P.stringLiteral atomo
charLiteral   = P.charLiteral atomo


-- Returns the current column
getIndent :: Parser Int
getIndent = do pos <- getPosition
               return $ sourceColumn pos


-- All possible expressions in the Atomo language.
-- Expressions that begin with a reserved word (e.g.)
-- `data' or `type' or `return') go first.
aExpr :: Parser AtomoVal
aExpr = aLambda
    <|> aData
    <|> aNewType
    <|> aReturn
    <|> aIf
    <|> aClass
    <|> aMutate
    <|> try aDefine
    <|> try aAnnot
    <|> try aInfix
    <|> try aCall
    <|> try aAttribute
    <|> try aDouble
    <|> aList
    <|> aHash
    <|> aTuple
    <|> aNumber
    <|> aString
    <|> aChar
    <|> aReference

aMainExpr :: Parser AtomoVal
aMainExpr = try aData
        <|> try aNewType
        <|> try aClass
        <|> try aDefine
        <|> try aAnnot

aScriptExpr :: Parser (SourcePos, AtomoVal)
aScriptExpr = do pos <- getPosition
                 expr <- aExpr
                 return (pos, expr)

-- Reference (variable lookup)
aReference :: Parser AtomoVal
aReference = do name <- identifier <|> try (parens operator)
                return $ AVariable name
             <?> "variable reference"

-- Class
aClass :: Parser AtomoVal
aClass = do reserved "class"
            code <- aBlock
            return code

-- Object attribute
aAttribute :: Parser AtomoVal
aAttribute = do target <- aReference
                dot
                attribute <- identifier
                return $ AAttribute (target, attribute)

-- Type, excluding functions
aSimpleType :: Parser Type
aSimpleType = try (do con <- identifier
                      args <- aType `sepBy1` spacing1
                      return $ Type (Name con) args)
          <|> try (do theTypes <- parens (commaSep aType)
                      return $ Type (Name "()") theTypes)
          <|> try (do theType <- brackets aType
                      return $ Type (Name "[]") [theType])
          <|> (identifier >>= return . Name)

-- Type
aType :: Parser Type
aType = do types <- (aSimpleType <|> parens aType) `sepBy1` (symbol "->")
           return $ toFunc types
        <?> "type declaration"

-- Type header
aAnnot = do name <- identifier <|> operator
            symbol "::"
            types <- aType
            return $ AAnnot name types

-- Type composition
aNewType :: Parser AtomoVal
aNewType = do reserved "type"
              name <- identifier
              colon
              whiteSpace
              theType <- aType
              return $ AType name theType

-- Lambda
aLambda :: Parser AtomoVal
aLambda = do reserved "do"
             params <- many identifier
             code <- aBlock
             return $ lambdify params code

-- Pattern matching
aPattern :: Parser String -- TODO: Finish this
aPattern = parens (identifier <|> string "_")

-- Variable declaration
aDecl :: Parser (Type, String)
aDecl = do theType <- aType
           name <- identifier <|> aPattern
           return (theType, name)
        <?> "variable declaration"

-- Function arguments
aArgs :: Parser [(Type, String)]
aArgs = commaSep aDecl
        <?> "function arguments"

-- Return statement
aReturn :: Parser AtomoVal
aReturn = do reserved "return"
             expr <- aExpr
             return $ AReturn expr
-- Block
aBlock :: Parser AtomoVal
aBlock = do colon
            exprs <- (do newline
                         whiteSpace
                         i <- getIndent
                         aBlock' i i [])
                 <|> (spacing >> aExpr >>= return . (: []))
            return $ ABlock exprs
         <?> "block"
         where
             aBlock' :: Int -> Int -> [AtomoVal] -> Parser [AtomoVal]
             aBlock' p i es | i /= p = return es
                 {- pies!-} | otherwise = try (do x <- aExpr
                                                  whiteSpace
                                                  new <- getIndent
                                                  next <- aBlock' new i es
                                                  return $ x : next) <|> return es


-- Data constructor
aConstructor :: Parser (AtomoVal -> AtomoVal)
aConstructor = do name <- ident
                  params <- option [] $ parens (commaSep aType)
                  return $ AConstruct name params

-- New data declaration
aData :: Parser AtomoVal
aData = do reserved "data"
           name <- identifier
           params <- option [] $ parens (commaSep aType)
           colon
           whiteSpace
           constructors <- aConstructor `sepBy` (symbol "|")
           let d = AData name params (map ($ d) constructors)
           return d

-- If/If-Else
aIf :: Parser AtomoVal
aIf = do reserved "if"
         cond <- aExpr
         code <- aBlock
         other <- try (reserved "else" >> aBlock) <|> (return $ ABlock [])
         return $ AIf cond code other
         
-- Variable assignment
aDefine :: Parser AtomoVal
aDefine = do name <- identifier <|> parens operator
             args <- many identifier
             code <- aBlock
             return $ ADefine name $ lambdify args code
          <?> "definition"

-- Variable reassignment
aMutate :: Parser AtomoVal
aMutate = do reserved "mutate"
             name <- identifier
             val <- aBlock
             return $ AMutate name val
          <?> "variable reassignment"

-- Parse a list (mutable list of values of one type)
aList :: Parser AtomoVal
aList = do contents <- brackets $ commaSep aExpr
           return $ AList contents

-- Parse a tuple (immutable list of values of any type)
aTuple :: Parser AtomoVal
aTuple = do contents <- parens $ commaSep aExpr
            return $ ATuple contents

-- Parse a hash (mutable, named contents of any type)
aHash :: Parser AtomoVal
aHash = do contents <- braces $ commaSep (do theType <- aType
                                             name <- identifier
                                             colon
                                             whiteSpace
                                             expr <- aExpr
                                             return (name, (theType, expr)))
           return $ AHash contents

-- Parse a number
aNumber :: Parser AtomoVal
aNumber = integer >>= return . intToPrim
          <?> "integer"

-- Parse a floating-point number
aDouble :: Parser AtomoVal
aDouble = float >>= return . doubleToPrim
          <?> "double"

-- Parse a string
aString :: Parser AtomoVal
aString = stringLiteral >>= return . toAString
          <?> "string"

-- Parse a single character
aChar :: Parser AtomoVal
aChar = charLiteral >>= return . charToPrim
        <?> "character"

-- Function call (prefix)
aCall :: Parser AtomoVal
aCall = do name <- aReference <|> try (parens aExpr)
           pos <- getPosition
           args <- many $ try arg
           return $ callify args name
        <?> "function call"
        where
            arg = try aAttribute
              <|> try aDouble
              <|> try (parens aIf)
              <|> try (parens aInfix)
              <|> try (parens aCall)
              <|> aList
              <|> aHash
              <|> aTuple
              <|> aReference
              <|> aString
              <|> aChar
              <|> aNumber

-- Call to predefined primitive function
aInfix :: Parser AtomoVal
aInfix = do val <- buildExpressionParser table targets
            return val
         <?> "infix call"
         where
             table = [ []
                     , []
                     , [ op "*" AssocLeft
                       , op "/" AssocLeft
                       ]
                     , [ op "+" AssocLeft
                       , op "-" AssocLeft
                       ]
                     , [ op "++" AssocRight ]
                     , [ op "==" AssocNone
                       , op "/=" AssocNone
                       , op "<" AssocNone
                       , op ">" AssocNone
                       ]
                     ]
                     where
                         op s a = Infix ((reservedOp s >> return (call s)) <?> "operator") a
                         call op a b = callify [a, b] (AVariable op)

             targets = try aCall
                   <|> try aIf
                   <|> try aDouble
                   <|> aList
                   <|> aTuple
                   <|> aHash
                   <|> aNumber
                   <|> aString
                   <|> aChar
                   <|> aReference


-- Parse a string or throw any errors
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow p s = case parse (do whiteSpace
                                 x <- p
                                 eof
                                 return x) "" s of
                      Left err -> throwError $ Parser err
                      Right val -> return val

-- Read a single expression
readExpr :: String -> ThrowsError AtomoVal
readExpr = readOrThrow aExpr

-- Read all expressions in a string
readExprs :: String -> ThrowsError [AtomoVal]
readExprs es = readOrThrow (many $ do x <- aExpr
                                      optional eol <|> eof
                                      return x) es

readScript :: String -> ThrowsError [(SourcePos, AtomoVal)]
readScript es = readOrThrow (many $ do x <- aScriptExpr
                                       optional eol <|> eof
                                       return x) es

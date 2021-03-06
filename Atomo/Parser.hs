{-# LANGUAGE NoMonomorphismRestriction, RelaxedPolyRec #-}

module Atomo.Parser where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Data.List (intercalate, nub, sort, sortBy, groupBy)
import Data.Char (isAlpha, toLower, toUpper, isUpper, isLower, digitToInt)
import Debug.Trace
import Text.Parsec hiding (sepBy, sepBy1)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P

type Parser = Parsec String [[(String, Assoc)]]

sepBy p sep  = sepBy1 p sep <|> return []
sepBy1 p sep = do x <- p
                  xs <- many (try (whiteSpace >> sep >> whiteSpace >> p))
                  return (x:xs)

instance Show Assoc where
    show AssocNone = "AssocNone"
    show AssocLeft = "AssocLeft"
    show AssocRight = "AssocRight"

instance Show (Operator s u m a) where
    show (Infix _ a) = "Infix (...) " ++ show a
    show (Prefix _) = "Prefix (...)"
    show (Postfix _) = "Postfix (...)"

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
                         , P.identLetter     = alphaNum <|> satisfy ((> 0x80) . fromEnum) <|> oneOf "'?"
                         , P.opStart         = letter <|> P.opLetter atomoDef
                         , P.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , P.reservedOpNames = ["=", "=>", "->", "::", ":=", ":"]
                         , P.reservedNames   = ["if", "else", "elseif", "while",
                                                "do", "class", "data", "type",
                                                "where", "module", "infix",
                                                "infixl", "infixr", "import",
                                                "return", "receive", "spawn"]
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

lexeme = P.lexeme atomo
capIdent      = do c <- satisfy isUpper
                   cs <- many (P.identLetter atomoDef)
                   return (c:cs)
lowIdent      = do c <- satisfy isLower
                   cs <- many (P.identLetter atomoDef)
                   return (c:cs)
capIdentifier = lexeme capIdent
lowIdentifier = lexeme lowIdent
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
operator      = P.operator atomo <|> between (char '`') (char '`') identifier
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
    <|> aImport
    <|> aAtom
    <|> aReceive
    <|> aFixity
    <|> try aBind
    <|> try aAnnot
    <|> try aSpawn
    <|> try aDefine
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
    <|> aVariable

aSimpleExpr :: Parser AtomoVal
aSimpleExpr = aAtom
          <|> try aBind
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
          <|> aVariable

aMainExpr :: Parser AtomoVal
aMainExpr = aImport
        <|> aData
        <|> aNewType
        <|> aClass
        <|> aTypeclass
        <|> aInstance
        <|> aFixity
        <|> try aDefine
        <|> try aBind
        <|> try aAnnot

aScriptExpr :: Parser (SourcePos, AtomoVal)
aScriptExpr = do pos <- getPosition
                 expr <- aMainExpr
                 return (pos, expr)

-- Reference (variable lookup)
aReference :: Parser String
aReference = identifier <|> try (parens operator)

-- Variable reference
aVariable :: Parser AtomoVal
aVariable = aReference >>= return . AVariable
            <?> "variable reference"

aModule :: Parser String
aModule = do path <- capIdentifier `sepBy1` char '.'
             return (intercalate "." path)

-- Import
aImport :: Parser AtomoVal
aImport = do reserved "import"
             from <- option "" (symbol "from" >> aModule)
             colon
             whiteSpace
             targets <- case from of
                             "" -> commaSep1 aModule
                             _ -> commaSep1 (aReference <|> symbol "*")
             return $ AImport from targets
          <?> "import"

-- Atom (@foo)
aAtom :: Parser AtomoVal
aAtom = do char '@'
           name <- lowIdentifier
           return $ AAtom name
        <?> "atom"

-- Receive
aReceive :: Parser AtomoVal
aReceive = do reserved "receive"
              matches <- aBlockOf (do atom <- aPattern
                                      code <- aBlock
                                      return $ APattern atom code)
              return $ AReceive matches
           <?> "receive"

-- Fixity declaration
aFixity :: Parser AtomoVal
aFixity = do decl <- try (symbol "infixl") <|> try (symbol "infixr") <|> symbol "infix"
             level <- natural
             colon
             spacing
             ops <- commaSep1 operator
             modifyState (insLevel level (map (\o -> (o, assoc decl)) ops))
             return ANone
          where
              assoc "infix" = AssocNone
              assoc "infixl" = AssocLeft
              assoc "infixr" = AssocRight

              insLevel n os t = insLevel' n os t

              insLevel' 9 os (t:ts) = (os ++ t) : ts
              insLevel' n os (t:ts) = t : insLevel' (n + 1) os ts

              call op a b = callify [a, b] (AVariable op)
            
-- Class
aClass :: Parser AtomoVal
aClass = do reserved "class"
            name <- aSimpleType
            code <- aBlockOf (try aStaticAnnot <|> try aAnnot <|> try aStatic <|> try aMethod)
            return $ ADefine (Class name) $ AClass name (static code) (public code)
         <?> "class"

-- Static definition
aStatic :: Parser AtomoVal
aStatic = do string "self"
             dot
             name <- lowIdentifier <|> parens operator
             args <- many aPattern
             code <- aBlock
             return $ AStatic name $ lambdify args code
          <?> "static definition"

-- Method definition
aMethod :: Parser AtomoVal
aMethod = do col <- getIndent
             name <- lowIdentifier <|> parens operator
             args <- many aPattern
             code <- aBlockOf (try aDefAttr <|> aExpr)
             others <- many (try (do newline
                                     replicateM (col - 1) anyToken
                                     symbol name
                                     args <- many aPattern
                                     code <- aBlockOf (try aDefAttr <|> aExpr)
                                     return $ lambdify args code))
             return $ ADefine (Define name) (AFunction $ (lambdify args code : others))
          <?> "definition"

-- Data field definition
aDefAttr :: Parser AtomoVal
aDefAttr = do object <- aVariable
              dot
              name <- lowIdentifier <|> parens operator
              reservedOp ":="
              val <- aExpr
              return $ ADefAttr object name $ val
           <?> "data definition"

-- Attribute
aAttribute :: Parser AtomoVal
aAttribute = do attr <- aAttribute'
                return $ ACall attr ANone
             where
                 aAttribute' = do target <- target
                                  dot
                                  attr <- try aAttribute' <|> aVariable
                                  return $ attribute target attr

                 target = try (parens aExpr)
                      <|> try aVariable
                      <|> aNumber
                      <|> aChar
                      <|> aString
                      <|> aList
                      <|> aHash
                      <|> aTuple

-- Typeclass
aTypeclass :: Parser AtomoVal
aTypeclass = do reserved "typeclass"
                name <- capIdentifier
                var <- lowIdentifier
                code <- aBlockOf (aFixity <|> try aAnnot <|> try aDefine)
                return $ ATypeclass name (Poly var) code

-- Typeclass instance
aInstance :: Parser AtomoVal
aInstance = do reserved "instance"
               name <- capIdentifier
               inst <- capIdentifier <|> (brackets spacing >> return "[]")
               code <- aBlockOf (try aAnnot <|> try aDefine)
               return $ AInstance name inst code

-- Type, excluding functions
aSimpleType :: Parser Type
aSimpleType = try (do con <- (capIdentifier >>= return . Name)
                         <|> (lowIdentifier >>= return . Poly)
                      args <- aSubType `sepBy1` spacing1
                      return $ Type con args)
          <|> aSubType
          <?> "type"

aSubType = try (do con <- (capIdentifier >>= return . Name)
                   args <- aSubType `sepBy1` spacing1
                   return $ Type con args)
       <|> try (do theType <- brackets aType
                   return $ Type (Name "[]") [theType])
       <|> try (parens aSimpleType)
       <|> try (parens aType)
       <|> try (do theTypes <- parens (commaSep aType)
                   return $ Type (Name "()") theTypes)
       <|> (capIdentifier >>= return . Name)
       <|> (lowIdentifier >>= return . Poly)
       <?> "type"

-- Type
aType :: Parser Type
aType = do types <- (aSimpleType <|> parens aType) `sepBy1` (symbol "->")
           return $ toFunc types
        <?> "type declaration"

-- Static type header (self.foo)
aStaticAnnot :: Parser AtomoVal
aStaticAnnot = do string "self"
                  dot
                  aAnnot

-- Type header
aAnnot :: Parser AtomoVal
aAnnot = do name <- identifier <|> parens operator
            symbol "::"
            types <- aType
            return $ AAnnot name types
         <?> "type annotation"

-- Type composition
aNewType :: Parser AtomoVal
aNewType = do reserved "type"
              name <- identifier
              colon
              whiteSpace
              theType <- aType
              return $ AType name theType
           <?> "type"

-- Lambda
aLambda :: Parser AtomoVal
aLambda = do reserved "do"
             params <- many aPattern
             code <- aBlock
             return $ lambdify params code
          <?> "lambda"

-- Pattern matching
aPattern :: Parser PatternMatch
aPattern = (symbol "_" >> return PAny)
       <|> try (do name <- lowIdentifier
                   char '@'
                   pattern <- aPattern
                   return $ PNamed name pattern)
       <|> (lowIdentifier >>= return . PName)
       <|> (do c <- capIdentifier
               return $ PCons c [])
       <|> try (parens (do c <- capIdentifier
                           as <- many aPattern
                           return $ PCons c as))
       <|> try (do tup <- parens (commaSep aPattern)
                   return $ PTuple tup)
       <|> try (do list <- brackets (commaSep aPattern)
                   return $ PList list)
       <|> try (parens (do h <- aPattern
                           symbol "|"
                           t <- aPattern
                           return $ PHeadTail h t))
       <|> (do val <- aChar
                  <|> aString
                  <|> aNumber
                  <|> aDouble
                  <|> aAtom
               return $ PMatch val)
       <?> "pattern match"

-- Return statement
aReturn :: Parser AtomoVal
aReturn = do reserved "return"
             expr <- option ANone (aExpr <|> parens aExpr)
             return $ AReturn expr
          <?> "return"
-- Block
aBlock :: Parser AtomoVal
aBlock = aBlockOf aExpr

aBlockOf :: Parser AtomoVal -> Parser AtomoVal
aBlockOf p = do colon
                exprs <- (do newline
                             whiteSpace
                             i <- getIndent
                             aBlock' i i [])
                     <|> (spacing >> aExpr >>= return . (: []))
                return $ ABlock exprs
             <?> "block"
             where
                 aBlock' :: Int -> Int -> [AtomoVal] -> Parser [AtomoVal]
                 aBlock' o i es = try (do x <- p
                                          new <- lookAhead (whiteSpace >> getIndent)
                                          if new == o
                                             then do whiteSpace
                                                     next <- aBlock' o new es
                                                     return $ x : next
                                             else return $ x : es) <|> return es


-- Data constructor
aConstructor :: Parser (AtomoVal -> AtomoVal)
aConstructor = do name <- capIdentifier
                  params <- many aSubType
                  return $ AConstruct name params

-- New data declaration
aData :: Parser AtomoVal
aData = do reserved "data"
           name <- capIdentifier
           params <- many aSimpleType
           colon
           whiteSpace
           constructors <- aConstructor `sepBy` (symbol "|")
           let d = AData name params
           return $ ABlock (map (\c -> ADefine (Define $ fromAConstruct c) (cons c)) (map ($ d) constructors))
        <?> "data"
        where
            cons c@(AConstruct n ts d) = lambdify (map PName as) (AValue n (map AVariable as) c)
                                         where as = map (\c -> [c]) $ take (length ts) ['a'..]

-- If/If-Else
aIf :: Parser AtomoVal
aIf = do reserved "if"
         cond <- aSimpleExpr
         code <- aBlock
         other <- try (whiteSpace >> reserved "else" >> aBlock) <|> (return $ ABlock [])
         return $ AIf cond code other
      <?> "if statement"
         
-- Variable assignment
aDefine :: Parser AtomoVal
aDefine = do (name, args) <- try (do n <- lowIdentifier <|> parens operator
                                     a <- many aPattern
                                     lookAhead colon
                                     return (n, a))
                         <|> (do a <- aPattern
                                 n <- operator
                                 as <- many aPattern
                                 return (n, a:as))
             code <- aBlock
             others <- many (try (do whiteSpace
                                     reserved name <|> parens (reserved name)
                                     args <- many1 aPattern
                                     code <- aBlock
                                     return $ lambdify args code)
                         <|> try (do whiteSpace
                                     a <- aPattern
                                     reserved name
                                     as <- many1 aPattern
                                     code <- aBlock
                                     return $ lambdify (a:as) code))
             return $ ADefine (Define name) (AFunction $ (lambdify args code : others))
          <?> "function definition"

-- Variable definition
aBind :: Parser AtomoVal
aBind = do name <- lowIdentifier <|> parens operator
           reservedOp ":="
           val <- aExpr
           return $ ADefine (Define name) val
        <?> "variable definition"

-- Parse a list (mutable list of values of one type)
aList :: Parser AtomoVal
aList = do contents <- brackets $ commaSep aExpr
           return $ AList contents

-- Parse a tuple (immutable list of values of any type)
-- A tuple must contain 2 or more values.
aTuple :: Parser AtomoVal
aTuple = do contents <- parens $ (do a <- aExpr
                                     symbol ","
                                     bs <- commaSep1 aExpr
                                     return (a:bs))
            return $ ATuple contents

-- Parse a hash (mutable, named contents of any type)
aHash :: Parser AtomoVal
aHash = do contents <- braces $ commaSep (do theType <- aType
                                             name <- lowIdentifier
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

-- Thread spawning
aSpawn :: Parser AtomoVal
aSpawn = do reserved "spawn"
            call <- aCall
            return $ ASpawn call
         <?> "spawn"

-- Function call (prefix)
aCall :: Parser AtomoVal
aCall = do name <- try aAttribute <|> aVariable <|> try (parens aExpr)
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
              <|> try (parens aExpr)
              <|> try aVariable
              <|> aLambda
              <|> aAtom
              <|> aList
              <|> aHash
              <|> aTuple
              <|> aString
              <|> aChar
              <|> aNumber

-- Call to predefined primitive function
aInfix :: Parser AtomoVal
aInfix = do st <- getState
            val <- buildExpressionParser (table st) targets
            return val
         <?> "infix call"
         where
             table st = (any st : head table') : tail table'
                        where
                            table' = map (map (\(o, a) -> Infix (reservedOp o >> return (call o)) a)) st

             any st = Infix (try (do op <- operator
                                     if op `elem` map fst (concat st)
                                        then fail "Reserved operator"
                                        else return (call op) <?> "any operator")) AssocLeft

             call op a b = callify [a, b] (AVariable op)

             targets :: Parser AtomoVal
             targets = try aCall
                   <|> try (parens aExpr)
                   <|> try aDouble
                   <|> aAtom
                   <|> aList
                   <|> aTuple
                   <|> aHash
                   <|> aNumber
                   <|> aString
                   <|> aChar
                   <|> try aAttribute
                   <|> aVariable


-- Parse a string or throw any errors
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow p s = case runP (do whiteSpace
                                x <- p
                                eof
                                return x) (replicate 10 []) "" s of
                      Left err -> throwError $ Parser err
                      Right val -> return val

-- Read a single expression
readExpr :: String -> ThrowsError AtomoVal
readExpr = readOrThrow aExpr

-- Read all expressions in a string
readExprs :: String -> ThrowsError [AtomoVal]
readExprs es = readOrThrow (many $ do x <- aExpr
                                      spacing
                                      eol <|> eof
                                      whiteSpace
                                      return x) es

readScript :: String -> ThrowsError [(SourcePos, AtomoVal)]
readScript es = readOrThrow (many $ do x <- aScriptExpr
                                       spacing
                                       eol <|> eof
                                       whiteSpace
                                       return x) es

{-# LANGUAGE NoMonomorphismRestriction #-}

module Atomo.Parser where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.List (intercalate, nub)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

-- Atomo parser
atomo :: P.TokenParser st
atomo = P.makeTokenParser atomoDef

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
                         , P.reservedOpNames = ["=", "=>", "->", "+", "-", "*", "/", "++"]
                         , P.reservedNames   = ["if", "else", "elseif", "while",
                                                "for", "class", "data", "type",
                                                "where", "module", "infix",
                                                "infixl", "infixr", "import",
                                                "return"]
                         , P.caseSensitive   = True
                         }

whiteSpace = do whiteSpace'
                many (whiteSpace' >> newline)
                return ()
         <|> whiteSpace'

simpleSpace = skipMany1 $ satisfy (`elem` " \t\f\v\xa0")

whiteSpace' | noLine && noMulti  = skipMany (simpleSpace <?> "")
            | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
            | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
            | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
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


parens     = P.parens atomo
brackets   = P.brackets atomo
braces     = P.braces atomo
comma      = P.comma atomo
commaSep   = P.commaSep atomo
commaSep1  = P.commaSep1 atomo
colon      = char ':'
eol        = try (string "\r\n") <|> try (string "\n\r") <|> try (string "\n") <|> try (string "\r") <?> "end of line"
dot        = P.dot atomo
identifier = P.identifier atomo
operator   = P.operator atomo
reserved   = P.reserved atomo
reservedOp = P.reservedOp atomo
integer    = P.integer atomo
float      = P.float atomo
charLit    = P.charLiteral atomo
natural    = P.natural atomo
symbol     = P.symbol atomo
stringLiteral = P.stringLiteral atomo
charLiteral = P.charLiteral atomo


-- String to an AList of AChars
toAString :: String -> AtomoVal
toAString s = AString $ AList (map AChar s)

-- Returns the current column
getIndent :: Parser Int
getIndent = do pos <- getPosition
               return $ sourceColumn pos


aExpr :: Parser AtomoVal
aExpr = try aVar
    <|> try aFunc
    <|> try aAttribute
    <|> try aPrimInfix -- Has to be before aCall/aPrimCall
    <|> try aPrimCall
    <|> try aCall
    <|> try aData
    <|> try aClass
    <|> try aIf
    <|> try aAssign
    <|> aList
    <|> aHash
    <|> try aTuple
    <|> try aDouble
    <|> aNumber
    <|> aString
    <|> aChar
    <|> aReference


-- Reference (variable lookup)
aReference :: Parser AtomoVal
aReference = do name <- identifier
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
-- Type
aType :: Parser Type
aType = try (do ret <- (identifier >>= return . Name) <|> parens aType
                anyChar
                args <- parens (commaSep aType)
                return $ Type (ret, args))
    <|> try (do con <- identifier
                args <- parens (commaSep aType)
                return $ Type (Name con, args))
    <|> try (do theType <- brackets aType
                return $ Type (Name "[]", [theType]))
    <|> (identifier >>= return . Name)

        <?> "type declaration"

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

-- Function header
aFuncHeader :: Parser (AtomoVal -> AtomoVal)
aFuncHeader = do theType <- aType
                 funcName <- identifier <|> operator
                 args <- parens (aArgs)
                 return $ AFunc theType funcName args
              <?> "function header"

-- Block
aBlock :: Parser AtomoVal
aBlock = do colon
            exprs <- (do newline
                         whiteSpace
                         i <- getIndent
                         aBlock' i i [])
                 <|> (whiteSpace >> aExpr >>= return . (: []))
            return $ ABlock exprs
         <?> "block"
         where
             aBlock' :: Int -> Int -> [AtomoVal] -> Parser [AtomoVal]
             aBlock' p i es | i /= p = return es
                 {- pies!-} | otherwise = try (do x <- aExpr
                                                  optional newline
                                                  whiteSpace
                                                  new <- getIndent
                                                  next <- aBlock' new i es
                                                  return $ x : next) <|> return es

-- Function
aFunc :: Parser AtomoVal
aFunc = do func <- aFuncHeader
           code <- aBlock
           return $ func code
        <?> "function"

-- Data constructor
aConstructor :: Parser (String, [Type])
aConstructor = do name <- identifier
                  params <- parens (commaSep aType) <|> return []
                  return $ (name, params)

-- New data declaration
aData :: Parser AtomoVal
aData = do reserved "data"
           name <- identifier
           params <- parens (commaSep aType) <|> return []
           colon
           optional newline
           whiteSpace
           constructors <- aConstructor `sepBy` (symbol "|")
           return $ AData name params constructors

-- If/If-Else
aIf :: Parser AtomoVal
aIf = do reserved "if"
         cond <- aExpr
         code <- aBlock
         other <- try (reserved "else" >> aBlock) <|> return ANone
         return $ AIf cond code other
         

-- Variable assignment
aVar :: Parser AtomoVal
aVar = do theType <- aType
          names <- commaSep1 identifier
          reservedOp "="
          val <- aExpr
          return $ ADefine theType (head names) val -- TODO: Multiple assignment
       <?> "variable"

-- Variable reassignment
aAssign :: Parser AtomoVal
aAssign = do names <- commaSep1 identifier
             reservedOp "="
             val <- aExpr
             return $ AAssign (head names) val -- TODO: Multiple assignment
          <?> "variable reassignment"

-- Parse a list (mutable list of values of one type)
aList :: Parser AtomoVal
aList = do contents <- brackets $ commaSep aExpr
           return $ AList contents

-- Parse a tuple (immutable list of values of any type)
aTuple :: Parser AtomoVal
aTuple = do contents <- parens $ commaSep (do theType <- aType
                                              expr <- aExpr
                                              return (theType, expr))
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

-- Parameters to a function call
aParams :: Parser [AtomoVal]
aParams = (parens $ commaSep aExpr) <?> "function arguments"

-- Function call (prefix)
aCall :: Parser AtomoVal
aCall = do name <- aReference <|> aAttribute
           params <- aParams
           return $ ACall name params
        <?> "function call"

-- Call to predefined primitive function
aPrimCall :: Parser AtomoVal
aPrimCall = do name <- choice (map (string . fst) primFuncs) <|> choice (map (string . fst) ioPrims)
               params <- aParams
               case lookup name primFuncs of
                  Just _ -> return $ ACall (APrimFunc name) params
                  Nothing -> return $ ACall (AIOFunc name) params
            <?> "primitive call"


aPrimInfix :: Parser AtomoVal
aPrimInfix = do val <- buildExpressionParser table targets
                return val
             <?> "primitive infix call"
             where
                 table = [ []
                         , []
                         , [ op "*" mulFunc AssocLeft
                           , op "/" divFunc AssocLeft
                           ]
                         , [ op "+" addFunc AssocLeft
                           , op "-" subFunc AssocLeft
                           ]
                         , [ op "++" concatFunc AssocRight ]
                         , [ op "==" equalityFunc AssocNone
                           , op "/=" inequalityFunc AssocNone
                           , op "<" lessFunc AssocNone
                           ]
                         ]
                         where
                             op s f assoc = Infix ((reservedOp s >> return f) <?> "operator") assoc
                             mulFunc a b = ACall (APrimFunc "*") [a, b]
                             divFunc a b = ACall (APrimFunc "/") [a, b]
                             addFunc a b = ACall (APrimFunc "+") [a, b]
                             subFunc a b = ACall (APrimFunc "-") [a, b]
                             concatFunc a b = ACall (APrimFunc "++") [a, b]
                             equalityFunc a b = ACall (APrimFunc "==") [a, b]
                             inequalityFunc a b = ACall (APrimFunc "/=") [a, b]
                             lessFunc a b = ACall (APrimFunc "<") [a, b]

                 targets = do val <- parens aExpr
                                 <|> try aVar
                                 <|> try aPrimCall
                                 <|> try aCall
                                 <|> try aIf
                                 <|> try aAssign
                                 <|> aList
                                 <|> aTuple
                                 <|> aHash
                                 <|> try aDouble
                                 <|> aNumber
                                 <|> aString
                                 <|> aChar
                                 <|> aReference
                              whiteSpace
                              return val

-- Primitive functions
primFuncs :: [(String, [AtomoVal] -> ThrowsError AtomoVal)]
primFuncs = [ ("++", concatFunc)
            , ("==", equalityFunc)
            , ("/=", inequalityFunc)
            , ("+", addFunc)
            , ("-", subFunc)
            , ("*", mulFunc)
            , ("/", divFunc)
            , ("<", lessFunc) -- WE NEED MORE FUNK
            , ("show", showFunc)
            , ("type", typeFunc)
            ]
            where
                addFunc [a, b] = return $ primAdd a b
                subFunc [a, b] = return $ primSub a b
                mulFunc [a, b] = return $ primMul a b
                divFunc [a, b] = return $ primDiv a b
                
                showFunc [a] = return $ toAString $ pretty a

                concatFunc [a, b] = return $ AList ((fromAList a) ++ (fromAList b))

                equalityFunc [(AInt a), (AInt b)] = return $ boolToPrim (a == b)
                equalityFunc [(AChar a), (AChar b)] = return $ boolToPrim (a == b)
                equalityFunc [(ADouble a), (ADouble b)] = return $ boolToPrim (a == b)
                equalityFunc [(AList a), (AList b)] = return $ boolToPrim (a == b)
                equalityFunc [(AString a), (AString b)] = return $ boolToPrim (a == b)
                equalityFunc [(AVariable a), (AVariable b)] = return $ boolToPrim (a == b)
                equalityFunc [(ADefine _ _ a), (ADefine _ _ b)] = return $ boolToPrim (a == b)
                equalityFunc [(AAssign _ a), (AAssign _ b)] = return $ boolToPrim (a == b)
                equalityFunc [a, b] = return $ boolToPrim (a == b)

                inequalityFunc [a, b] = equalityFunc [a, b] >>= return . primNot

                lessFunc [a, b] = return $ boolToPrim $ (<) (fromAInt a) (fromAInt b)

                typeFunc [a] = return . toAString . prettyType $ getType a

-- Primitive I/O functions
ioPrims :: [(String, [AtomoVal] -> IOThrowsError AtomoVal)]
ioPrims = [ ("print", printFunc)
          , ("dump", dumpFunc)
          ]
          where
              printFunc xs = liftIO $ mapM_ (putStrLn . fromAString) xs >> return ANone
              dumpFunc xs = liftIO $ mapM_ (putStrLn . pretty) xs >> return ANone

getIOPrim :: String -> ([AtomoVal] -> IOThrowsError AtomoVal)
getIOPrim = fromJust . (flip lookup ioPrims)

getPrim :: String -> ([AtomoVal] -> ThrowsError AtomoVal)
getPrim = fromJust . (flip lookup primFuncs)

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
readExprs = readOrThrow (many $ do whiteSpace
                                   x <- aExpr
                                   optional newline <|> eof
                                   return x)

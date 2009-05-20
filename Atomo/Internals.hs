module Atomo.Internals where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Error
import Data.List (intercalate)
import Debug.Trace
import Text.Parsec (ParseError)
import Text.Parsec.Pos (newPos, sourceName, sourceLine, sourceColumn, SourcePos)

dump s x = trace (s ++ ": " ++ show x) (return ())
debug x = trace (show x) x

type ThrowsError = Either AtomoError
type IOThrowsError = ErrorT AtomoError IO

data Index = Define String | Class Type | Process ThreadId
             deriving (Eq, Show)

data Type = Name String | Type Type [Type] | Func Type Type | None | Poly Char
            deriving (Show)

instance Eq Type where
    Func None a == b           = a == b
    a           == Func None b = a == b
    Name a      == Name b      = a == b
    Type a as   == Type b bs   = a == b && as == bs
    Func a a'   == Func b b'   = a == b && a' == b'
    Poly a      == Poly b      = a == b
    None        == None        = True
    Name a      == Poly b      = True
    Poly a      == Name b      = True
    _           == _           = False

instance Eq (Chan a) where
    _ == _ = False
instance Show (Chan a) where
    show _ = "<Channel>"

data AtomoVal = AInt Integer
              | AChar Char
              | ADouble Double
              | AList [AtomoVal]
              | ATuple [AtomoVal]
              | AHash [(String, (Type, AtomoVal))]
              | AString AtomoVal -- AString == AList of AChars
              | AVariable String
              | AClass [AtomoVal] [AtomoVal]
              | AObject [AtomoVal]
              | AAttribute AtomoVal String
              | AAccessor AtomoVal String
              | ADefine Index AtomoVal
              | ADefAttr AtomoVal String AtomoVal
              | AStatic String AtomoVal
              | APrimCall String [AtomoVal]
              | ALambda PatternMatch AtomoVal [(PatternMatch, AtomoVal)]
              | ACall AtomoVal AtomoVal
              | ABlock [AtomoVal]
              | AData String [Type]
              | AConstruct String [Type] AtomoVal
              | AValue String [AtomoVal] AtomoVal
              | AIf AtomoVal AtomoVal AtomoVal
              | AReturn AtomoVal
              | AType String Type
              | AAnnot String Type
              | AImport String [String]
              | AModule [AtomoVal]
              | AError String
              | AAtom String
              | ASpawn AtomoVal
              | AReceive AtomoVal
              | AProcess ThreadId (Chan AtomoVal)
              | APattern PatternMatch AtomoVal
              | AFunction [AtomoVal] -- List of ALambdas to try different patterns
              | ANone
              deriving (Eq, Show)


data AtomoError = NumArgs Int Int SourcePos
                | ImmutableVar String SourcePos
                | TypeMismatch Type Type SourcePos
                | NotFunction String SourcePos
                | UnboundVar String SourcePos
                | Parser ParseError
                | Default String SourcePos
                | Unknown String
                deriving (Show)

instance Error AtomoError where
    noMsg = Default "An error has occurred" (newPos "unknown" 0 0)
    strMsg = flip Default (newPos "unknown" 0 0)


data PatternMatch = PAny
                  | PMatch AtomoVal
                  | PName String
                  | PNamed String PatternMatch
                  | PCons String [PatternMatch]
                  | PHeadTail PatternMatch PatternMatch
                  | PList [PatternMatch]
                  | PTuple [PatternMatch]
                  deriving (Eq, Show)

fromAInt (AInt i) = i
fromAInt (AValue "Int" [AInt i] _) = i
fromAChar (AChar c) = c
fromAChar (AValue "Char" [AChar c] _) = c
fromADouble (ADouble d) = d
fromADouble (AValue "Double" [ADouble d] _) = d
fromAVariable (AVariable n) = n
fromAList (AList l) = l
fromAList (AString l) = fromAList l
fromAString (AString s) = map fromAChar (fromAList s)
fromAString (AList l) = map fromAChar (fromAList (AList l))
fromAConstruct (AConstruct s _ _) = s
fromAAtom (AAtom n) = n

-- String to an AList of AChars
toAString :: String -> AtomoVal
toAString s = AString $ AList (map AChar s)

list :: Type
list = Type (Name "[]") [Name "a"]

listOf :: Type -> Type
listOf a = Type (Name "[]") [a]

result :: Type -> Type
result (Func _ t) = t

arg :: Type -> Type
arg (Func t _) = t

toFunc :: [Type] -> Type
toFunc [] = None
toFunc [t] = Func None t
toFunc (t:ts) = Func t (toFunc ts)

pretty :: AtomoVal -> String
pretty (AInt int)       = show int
pretty (AChar char)     = show char
pretty (ADouble double) = show double
pretty (AValue "Int" [AInt i] _) = show i
pretty (AValue "Iouble" [ADouble d] _) = show d
pretty (AValue "Char" [AChar c] _) = show c
pretty (AList str@(AChar _:_)) = show $ AString $ AList str
pretty (AList list)     = "[" ++ (intercalate ", " (map pretty list)) ++ "]"
pretty (AHash es)       = "{ " ++ (intercalate ", " (map prettyVal es)) ++ " }"
                          where prettyVal (n, (t, v)) = (prettyType t) ++ " " ++ n ++ ": " ++ pretty v
pretty (ATuple vs)      = "(" ++ (intercalate ", " (map pretty vs)) ++ ")"
pretty (AVariable n)    = "<Variable (" ++ n ++ ")>"
pretty (ADefine _ v)    = pretty v
pretty (ADefAttr _ _ v) = pretty v
pretty (AStatic _ v)    = pretty v
pretty (AObject vs)     = "Object:\n" ++ (unlines $ map (" - " ++) $ map pretty vs)
pretty (ACall f a)      = "<Call (`" ++ pretty f ++ "') (`" ++ pretty a ++ "')>"
pretty s@(AString _)    = show $ fromAString s
pretty (ABlock es)      = intercalate "\n" $ map pretty es
pretty (AData s as)     = prettyType $ Type (Name s) as
pretty (AConstruct s [] _) = s
pretty (AConstruct s ts _) = s ++ " " ++ intercalate " " (map prettyType ts)
pretty (AValue v [] _)  = v
pretty (AValue v as _)  = v ++ " " ++ intercalate " " (map pretty as)
pretty (AAnnot n t)     = n ++ " :: " ++ prettyType t
pretty v@(ALambda _ _ _) = "\x03BB " ++ intercalate " " (map show $ reverse $ lambdas v []) ++ "."
                           where
                               lambdas (ALambda p v _) acc = lambdas v (p : acc)
                               lambdas _ acc = acc
pretty (AClass ss ms) = "<Class (`" ++ statics ++ "') (`" ++ methods ++ "')>"
                        where
                            statics = intercalate "' `" (map (\(AStatic n _) -> n) ss)
                            methods = intercalate "' `" (map (\(ADefine (Define n) _) -> n) ms)
pretty (AError m)       = m
pretty (AAtom n)        = "@" ++ n
pretty (AReceive v)     = "<Receive>"
pretty (APattern k v)   = "<Pattern (" ++ show k ++ ") (" ++ show v ++ ")>"
pretty ANone            = "None"
pretty a                = "TODO -- " ++ show a

prettyError (NumArgs e f p)      = prettyPos p ++ "Expected " ++ show e ++ " args; found " ++ show f
prettyError (ImmutableVar n p)   = prettyPos p ++ "Cannot reassign immutable reference `" ++ n ++ "`"
prettyError (TypeMismatch e f p) = prettyPos p ++ "Invalid type; expected `" ++ prettyType e ++ "', found `" ++ prettyType f ++ "'"
prettyError (NotFunction n p)    = prettyPos p ++ "Variable is not a function: " ++ n
prettyError (UnboundVar n p)     = prettyPos p ++ "Reference to unknown variable: " ++ n
prettyError (Parser e)           = "Parse error at " ++ show e
prettyError (Default m p)        = prettyPos p ++ m
prettyError (Unknown m)          = m

prettyPos :: SourcePos -> String
prettyPos p | null $ sourceName p = "Line " ++ show (sourceLine p) ++ " Col " ++ show (sourceColumn p) ++ ":\n    " 
            | otherwise = "`" ++ sourceName p ++ "', line " ++ show (sourceLine p) ++ " Col " ++ show (sourceColumn p) ++ ":\n    " 

prettyType :: Type -> String
prettyType (Name a) = a
prettyType (Type (Name "[]") []) = "[a]"
prettyType (Type (Name "[]") [t]) = "[" ++ prettyType t ++ "]"
prettyType (Type (Name "()") ts) = "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (Type a ts) = prettyType a ++ " " ++ intercalate " " (map prettyType ts)
prettyType (Func None b) = prettyType b
prettyType (Func a b) = "(" ++ prettyType a ++ " -> " ++ prettyType b ++ ")"
prettyType (None) = "None"

lambdify :: [PatternMatch] -> AtomoVal -> AtomoVal
lambdify [] b = b
lambdify (s:ss) b = ALambda s (lambdify ss b) []

callify :: [AtomoVal] -> AtomoVal -> AtomoVal
callify as t = callify' (reverse as) t
               where
                   callify' [] t = ACall t ANone
                   callify' [a] t = ACall t a
                   callify' (a:as) t = ACall (callify' as t) a

static :: AtomoVal -> [AtomoVal]
static (ABlock xs) = static' xs
                     where
                         static' [] = []
                         static' (v@(AStatic _ _):xs) = v : static' xs
                         static' (_:xs) = static' xs

public :: AtomoVal -> [AtomoVal]
public (ABlock xs) = public' xs
                     where
                         public' [] = []
                         public' (v@(ADefine _ _):xs) = v : public' xs
                         public' (_:xs) = public' xs

getType :: AtomoVal -> Type
getType (AChar _) = Name "char"
getType (ADouble _) = Name "double"
getType (AList []) = Type (Name "[]") [Poly 'a']
getType (AList as) = listOf $ getType (head as)
getType (ATuple as) = Type (Name "()") $ map getType as
getType (AHash _) = Name "hash"
getType (AString _) = listOf (Name "char")
getType (AType n v) = v
getType (AConstruct _ [] d@(AData n ps)) = getType d
getType (AConstruct _ ts d@(AData n ps)) = foldr Func (getType d) ts
getType (AData n []) = Name n
getType (AData n as) = Type (Name n) as
getType (ALambda _ b _) = undefined -- TODO
getType (AReturn r) = getType r
getType (ADefine _ _) = undefined
getType (AValue _ _ d@(AData n [])) = getType d
getType (AValue c as (AData n ps)) = Type (Name n) ps -- TODO
getType a = error ("Cannot get type of `" ++ pretty a ++ "'")

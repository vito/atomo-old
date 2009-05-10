module Atomo.Internals where

import Control.Monad.Error
import Data.List (intercalate)
import Debug.Trace
import Text.Parsec (ParseError)
import Text.Parsec.Pos (newPos, sourceName, sourceLine, sourceColumn, SourcePos)

dump s x = trace (s ++ ": " ++ show x) (return ())
debug x = trace (show x) x

type ThrowsError = Either AtomoError
type IOThrowsError = ErrorT AtomoError IO

data Type = Name String | Type Type [Type] | Func Type Type | None | Poly Char
            deriving (Show, Eq)

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
              | ADefine String AtomoVal
              | ADefAttr AtomoVal String AtomoVal
              | AStatic String AtomoVal
              | AMutate String AtomoVal
              | APrimCall String [AtomoVal]
              | ALambda String AtomoVal [(String, AtomoVal)]
              | ACall AtomoVal AtomoVal
              | ABlock [AtomoVal]
              | AData String [Type] [AtomoVal]
              | AConstruct String [Type] AtomoVal
              | AValue String [AtomoVal] AtomoVal
              | AIf AtomoVal AtomoVal AtomoVal
              | AReturn AtomoVal
              | AType String Type
              | AAnnot String Type
              | AImport String [String]
              | AModule [AtomoVal]
              | ANone
              deriving (Eq)

instance Show AtomoVal where
    show (AInt v) = "AInt " ++ show v
    show (AChar v) = "AChar " ++ show v
    show (ADouble v) = "ADouble " ++ show v
    show (AList v) = "AList " ++ show v
    show (ATuple v) = "ATuple " ++ show v
    show (AHash v) = "AHash " ++ show v
    show (AString v) = "AString (" ++ show v ++ ")"
    show (AVariable v) = "AVariable " ++ show v
    show (AClass s i) = "AClass " ++ show s ++ " " ++ show i
    show (AAttribute v a) = "AAttribute (" ++ show v ++ ") " ++ show a
    show (AObject v) = "AObject " ++ show v
    show (ADefine n v) = "ADefine " ++ show n ++ " (" ++ show v ++ ")"
    show (ADefAttr o n v) = "ADefAttr (" ++ show o ++ ") " ++ show n ++ " (" ++ show v ++ ")"
    show (AStatic n v) = "AStatic " ++ show n ++ " (" ++ show v ++ ")"
    show (AMutate n v) = "AMutate " ++ show n ++ " " ++ show v
    show (APrimCall n ps) = "APrimCall " ++ show n ++ " " ++ show ps
    show (ALambda p c bs) = "ALambda " ++ show p ++ " (" ++ show c ++ ") " ++ show bs
    show (ACall t a) = "ACall (" ++ show t ++ ") (" ++ show a ++ ")"
    show (ABlock vs) = "ABlock " ++ show vs
    show (AData n ps cs) = "AData " ++ show n ++ " " ++ show ps ++ " " ++ show cs
    show (AConstruct n ps d) = "AConstruct " ++ show n ++ " " ++ show ps ++ " (AData ...)"
    show (AValue n as d) = "AValue " ++ show n ++ " " ++ show as ++ " (AData ...)"
    show (AIf c t f) = "AIf (" ++ show c ++ ") (" ++ show t ++ ") (" ++ show f ++ ")"
    show (AReturn v) = "AReturn (" ++ show v ++ ")"
    show (AType n t) = "AType " ++ show n ++ " (" ++ show t ++ ")"
    show (AAnnot n t) = "AAnnot " ++ show n ++ " (" ++ show t ++ ")"
    show (AImport f vs) = "AImport " ++ show f ++ " " ++ show vs
    show (AModule as) = "AModule " ++ show as
    show (ANone) = "ANone"


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

-- String to an AList of AChars
toAString :: String -> AtomoVal
toAString s = AString $ AList (map AChar s)

list :: Type
list = Type (Name "[]") [Name "a"]

listOf :: Type -> Type
listOf a = Type (Name "[]") [a]


data AtomoError = NumArgs Int Int SourcePos
                | ImmutableVar String SourcePos
                | TypeMismatch Type Type SourcePos
                | NotFunction String SourcePos
                | UnboundVar String SourcePos
                | Parser ParseError
                | Default String SourcePos
                | Unknown String


instance Show AtomoError where
    show (NumArgs e f p)      = prettyPos p ++ "Expected " ++ show e ++ " args; found " ++ show f
    show (ImmutableVar n p)   = prettyPos p ++ "Cannot reassign immutable reference `" ++ n ++ "`"
    show (TypeMismatch e f p) = prettyPos p ++ "Invalid type; expected `" ++ prettyType e ++ "', found `" ++ prettyType f ++ "'"
    show (NotFunction n p)    = prettyPos p ++ "Variable is not a function: " ++ n
    show (UnboundVar n p)     = prettyPos p ++ "Reference to unknown variable: " ++ n
    show (Parser e)           = "Parse error at " ++ show e
    show (Default m p)        = prettyPos p ++ m
    show (Unknown m)          = m

instance Error AtomoError where
    noMsg = Default "An error has occurred" (newPos "unknown" 0 0)
    strMsg = flip Default (newPos "unknown" 0 0)

prettyPos :: SourcePos -> String
prettyPos p | null $ sourceName p = "Line " ++ show (sourceLine p) ++ " Col " ++ show (sourceColumn p) ++ ":\n    " 
            | otherwise = "`" ++ sourceName p ++ "', line " ++ show (sourceLine p) ++ " Col " ++ show (sourceColumn p) ++ ":\n    " 

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
pretty (AMutate _ v)    = pretty v
pretty (AObject vs)     = "Object:\n" ++ (unlines $ map (" - " ++) $ map pretty vs)
pretty (ACall f a)      = "<Call (`" ++ pretty f ++ "') (`" ++ pretty a ++ "')>"
pretty s@(AString _)    = show $ fromAString s
pretty (ABlock es)      = intercalate "\n" $ map pretty es
pretty (AData s as _)   = prettyType $ Type (Name s) as
pretty (AConstruct s [] _) = s
pretty (AConstruct s ts _) = s ++ " " ++ intercalate " " (map prettyType ts)
pretty (AValue v [] _)  = v
pretty (AValue v as _)  = v ++ " " ++ intercalate " " (map pretty as)
pretty (AAnnot n t)     = n ++ " :: " ++ prettyType t
pretty v@(ALambda _ _ _) = "\\ " ++ intercalate " " (reverse $ lambdas v []) ++ "."
                           where
                               lambdas (ALambda n v _) acc = lambdas v (n : acc)
                               lambdas _ acc = acc
pretty (AClass ss ms) = "<Class (`" ++ statics ++ "') (`" ++ methods ++ "')>"
                        where
                            statics = intercalate "' `" (map (\(AStatic n _) -> n) ss)
                            methods = intercalate "' `" (map (\(ADefine n _) -> n) ms)
pretty ANone            = "()"
pretty a                = "TODO -- " ++ show a

prettyType :: Type -> String
prettyType (Name a) = a
prettyType (Type (Name "[]") [t]) = "[" ++ prettyType t ++ "]"
prettyType (Type (Name "()") ts) = "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (Type a ts) = prettyType a ++ " " ++ intercalate " " (map prettyType ts)
prettyType (Func None b) = prettyType b
prettyType (Func a b) = "(" ++ prettyType a ++ " -> " ++ prettyType b ++ ")"
prettyType (None) = "None"

lambdify :: [String] -> AtomoVal -> AtomoVal
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

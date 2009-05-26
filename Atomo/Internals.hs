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

data Index = Define String | Class Type | Process ThreadId | Typeclass String | Instance String String
             deriving (Eq, Show)

data Type = Name String | Type Type [Type] | Func Type Type | None | Poly String
            deriving (Eq, Show)

match (Func None a) b             = match a b
match a             (Func None b) = match a b
match (Name a)      (Name b)      = a == b
match (Type a as)   (Type b bs)   = match a b && (and $ zipWith match as bs)
match (Func a a')   (Func b b')   = match a b && match a' b'
match (Poly a)      (Poly b)      = a == b
match (None)        (None)        = True
match (Name a)      (Poly b)      = True
match (Poly a)      (Name b)      = False
match _             _             = False

instance Ord Type where
    compare (Poly _) (Name _) = LT
    compare (Name _) (Poly _) = GT
    compare (Name _) (Name _) = EQ
    compare (Poly _) (Poly _) = EQ
    compare (Type a as) (Type b bs) | compare a b == GT = GT
                                    | compare a b == EQ = compare (zipWith compare as bs) (zipWith compare bs as)
                                    | otherwise = LT
    compare (Func af at) (Func bf bt) | compare af bf == GT = GT
                                      | compare af bf == EQ = compare at bt
                                      | otherwise = LT
    compare _ (Poly _) = GT
    compare (Poly _) _ = LT
    compare _ _ = EQ


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
              | ATypeclass String Type AtomoVal
              | ATypeFunc String String
              | AInstance String String AtomoVal
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
fromAInt (AValue "Integer" [AInt i] _) = i
fromAChar (AChar c) = c
fromAChar (AValue "Char" [AChar c] _) = c
fromADouble (ADouble d) = d
fromADouble (AValue "Double" [ADouble d] _) = d
fromAVariable (AVariable n) = n
fromAList (AList l) = l
fromAList (AString l) = fromAList l
fromAString (AString s) = map fromAChar (fromAList s)
fromAString (AList l) = map fromAChar (fromAList (AList l))
fromAString a = error ("Not a string: `" ++ pretty a ++ "'")
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
pretty (AValue "Integer" [AInt i] _) = show i
pretty (AValue "Double" [ADouble d] _) = show d
pretty (AValue "Char" [AChar c] _) = show c
pretty (AList str@(AChar _:_)) = pretty $ AString $ AList str
pretty (AList list)     = "[" ++ (intercalate ", " (map pretty list)) ++ "]"
pretty (AHash es)       = "{ " ++ (intercalate ", " (map prettyVal es)) ++ " }"
                          where prettyVal (n, (t, v)) = (prettyType t) ++ " " ++ n ++ ": " ++ pretty v
pretty (ATuple vs)      = "(" ++ (intercalate ", " (map pretty vs)) ++ ")"
pretty (AVariable n)    = "<Variable (" ++ n ++ ")>"
pretty (ADefine n v)    = "<`" ++ show n ++ "': " ++ pretty v ++ ">"
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
pretty v@(ALambda _ _ _) = "\x03BB " ++ intercalate " " (map prettyPattern $ reverse $ lambdas v []) ++ "."
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
pretty (AFunction ls)   = "<Function (" ++ intercalate ") (" (map pretty ls) ++ ")>"
pretty ANone            = "~"
pretty a                = "TODO -- " ++ show a

prettyError (NumArgs e f p)      = prettyPos p ++ "Expected " ++ show e ++ " args; found " ++ show f
prettyError (ImmutableVar n p)   = prettyPos p ++ "Cannot reassign immutable reference `" ++ n ++ "`"
prettyError (TypeMismatch e f p) = prettyPos p ++ "Invalid type; expected `" ++ prettyType e ++ "', found `" ++ prettyType f ++ "'"
prettyError (NotFunction n p)    = prettyPos p ++ "Variable is not a function: " ++ n
prettyError (UnboundVar n p)     = prettyPos p ++ "Reference to unknown variable: " ++ n
prettyError (Parser e)           = "Parse error at " ++ show e
prettyError (Default m p)        = prettyPos p ++ m
prettyError (Unknown m)          = m

prettyPattern PAny = "_"
prettyPattern (PMatch v) = pretty v
prettyPattern (PName a) = a
prettyPattern (PNamed n p) = n ++ "@" ++ prettyPattern p
prettyPattern (PCons n ps) = "(" ++ n ++ " " ++ intercalate " " (map prettyPattern ps) ++ ")"
prettyPattern (PHeadTail h t) = "(" ++ prettyPattern h ++ ":" ++ prettyPattern t ++ ")"
prettyPattern (PList ps) = "[" ++ intercalate ", " (map prettyPattern ps) ++ "]"
prettyPattern (PTuple ps) = "(" ++ intercalate ", " (map prettyPattern ps) ++ ")"

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
prettyType (Poly a) = a
prettyType (None) = "None"

lambdify :: [PatternMatch] -> AtomoVal -> AtomoVal
lambdify [] b = b
lambdify (s:ss) b = ALambda s (lambdify ss b) []

callify :: [AtomoVal] -> AtomoVal -> AtomoVal
callify as t = callify' (reverse as) t
               where
                   callify' [] t = ACall t ANone
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
getType (AList []) = Type (Name "[]") [Poly "a"]
getType (AList as) = listOf $ getType (head as)
getType (ATuple as) = Type (Name "()") $ map getType as
getType (AHash _) = Name "hash"
getType (AString _) = listOf (Name "char")
getType (AType n v) = v
getType (AConstruct _ [] d@(AData n ps)) = getType d
getType (AConstruct _ ts d@(AData n ps)) = foldr Func (getType d) ts
getType (AData n []) = Name n
getType (AData n as) = Type (Name n) as
getType (ALambda _ b _) = error "Cannot get type of a lambda." -- TODO
getType (AReturn r) = getType r
getType (ADefine _ v) = getType v
getType (AValue _ _ (AConstruct _ _ d@(AData n []))) = getType d
getType (AValue c as (AConstruct _ cs (AData n ps))) = Type (Name n) (args cs as ps)
                                                       where
                                                           args [] [] ps = ps
                                                           args (c:cs) (a:as) ps = args cs as (swapType ps c (getType a))
getType a = error ("Cannot get type of `" ++ pretty a ++ "'")

getData :: AtomoVal -> String
getData (AValue _ _ c) = getData c
getData (AConstruct _ _ (AData n _)) = n
getData (AData n _) = n
getData (AList _) = "[]"
getData v = error ("Cannot get data name for `" ++ pretty v ++ "'")

-- Deep-replace a type with another type (used for replacing polymorphic types)
swapType :: [Type] -> Type -> Type -> [Type]
swapType ts t n = swapType' ts [] t n
                  where
                      swapType' [] acc _ _ = acc
                      swapType' (t@(Type a ts'):ts) acc f r = swapType' ts (acc ++ [Type (repl a f r) (swapType ts' f r)]) f r
                      swapType' (t@(Func a b):ts) acc f r = swapType' ts (acc ++ [Func (repl a f r) (repl b f r)]) f r
                      swapType' (t:ts) acc f r = swapType' ts (acc ++ [repl t f r]) f r
                      repl a f r | a == f = r
                                 | otherwise = a

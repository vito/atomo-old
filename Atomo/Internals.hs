module Atomo.Internals where

import Control.Monad.Error
import Data.List (intercalate)
import Text.Parsec (ParseError)

type Type = String
type ThrowsError = Either AtomoError
type IOThrowsError = ErrorT AtomoError IO

data AtomoVal = AInt Integer
              | AChar Char
              | ADouble Double
              | AList [AtomoVal]
              | ATuple [(Type, AtomoVal)]
              | AHash [(String, (Type, AtomoVal))]
              | AVariable String
              | AClass String [(String, AtomoVal)] -- Name [(Attribute/Function Name, Value)]
              | AAttribute (AtomoVal, String)
              | ADefine Type String AtomoVal
              | AAssign String AtomoVal
              | AObject String [AtomoVal]
              | APrimFunc String
              | AIOFunc String
              | AFunc Type String [(Type, String)] AtomoVal
              | ACall AtomoVal [AtomoVal]
              | AString AtomoVal -- AString == AList of AChars
              | ABlock [AtomoVal]
              | AData String [AtomoVal]
              | AConstruct String AtomoVal
              | AIf AtomoVal AtomoVal AtomoVal
              | ANone
              deriving (Show)

instance Eq AtomoVal where
    (AInt a) == (AInt b) = a == b
    (AChar a) == (AChar b) = a == b
    (ADouble a) == (ADouble b) = a == b
    (AList a) == (AList b) = a == b
    (AString a) == (AString b) = a == b
    (AVariable a) == (AVariable b) = a == b
    (ADefine _ _ a) == (ADefine _ _ b) = a == b
    (AAssign _ a) == (AAssign _ b) = a == b

fromAInt (AInt i) = i
fromAChar (AChar c) = c
fromADouble (ADouble d) = d
fromAVariable (AVariable n) = n
fromAList (AList l) = l
fromAList (AString l) = fromAList l
fromAString (AString s) = map fromAChar (fromAList s)
fromAString (AList l) = map fromAChar (fromAList (AList l))
fromAConstruct (AConstruct s _) = s
fromAData (AData s _) = s

data AtomoError = NumArgs Int Int
                | ImmutableVar String
                | TypeMismatch Type Type
                | NotFunction String String
                | UnboundVar String String
                | Parser ParseError
                | Default String


instance Show AtomoError where
    show (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found " ++ show found
    show (ImmutableVar var)            = "Cannot reassign immutable reference `" ++ var ++ "`"
    show (TypeMismatch expected found) = "Invalid type; expected " ++ expected ++ ", found " ++ found
    show (NotFunction message func)    = message ++ ": " ++ func
    show (UnboundVar message var)      = message ++ ": " ++ var
    show (Parser err)                  = "Parse error at " ++ show err
    show (Default message)             = message

instance Error AtomoError where
    noMsg = Default "An error has occurred"
    strMsg = Default

verifyList :: [AtomoVal] -> Maybe (Type, Type)
verifyList [] = Nothing
verifyList (x:xs) = verifyList' (getType x) xs
                    where verifyList' t [] = Nothing
                          verifyList' t (x:xs) | getType x == t = verifyList' t xs
                                               | otherwise = Just (t, getType x)

verifyTuple :: [(Type, AtomoVal)] -> Maybe (Type, Type)
verifyTuple [] = Nothing
verifyTuple ((t, v):vs) | t == getType v = verifyTuple vs
                        | otherwise = Just (t, getType v)

verifyHash :: [(String, (Type, AtomoVal))] -> Maybe (Type, Type)
verifyHash [] = Nothing
verifyHash ((_, (t, v)):vs) | t == getType v = verifyHash vs
                            | otherwise = Just (t, getType v)

getType :: AtomoVal -> Type
getType (AInt _) = "int"
getType (AChar _) = "char"
getType (ADouble _) = "double"
getType (ATuple _) = "tuple"
getType (AHash _) = "hash"
getType (AString _) = "string" -- todo: make type aliases work
getType (AConstruct _ t) = getType t
getType (AData n _) = n
getType (AFunc t _ as _) = t ++ " f(" ++ intercalate ", " (map fst as) ++ ")"
getType (AList []) = "[]"
getType (AList as) = "[" ++ getType (head as) ++ "]"
getType _ = "unknown"

getReturnType (AFunc t _ _ _) = t
getReturnType a = getType a

pretty :: AtomoVal -> String
pretty (AInt int)       = show int
pretty (AChar char)     = show char
pretty (ADouble double) = show double
pretty (AList str@(AChar _:_)) = show $ AString $ AList str
pretty (AList list)     = "[" ++ (intercalate ", " (map pretty list)) ++ "]"
pretty (AHash es)       = "{ " ++ (intercalate ", " (map prettyVal es)) ++ " }"
                          where prettyVal (n, (t, v)) = t ++ " " ++ n ++ ": " ++ pretty v
pretty (ATuple vs)      = "(" ++ (intercalate ", " (map prettyVal vs)) ++ ")"
                          where prettyVal (t, v) = t ++ " " ++ pretty v
pretty (AVariable n)    = "Variable: " ++ n
pretty (ADefine _ _ v)  = pretty v
pretty (AAssign _ v)    = pretty v
pretty (AObject n vs)   = n ++ " (Object):\n" ++ (unlines $ map (" - " ++) $ map pretty vs)
pretty (APrimFunc n)    = "<Function Primitive (" ++ n ++ ")>"
pretty (AIOFunc n)      = "<IO Primitive (" ++ n ++ ")>"
pretty (AFunc t n _ _)  = n ++ " (Function)"
pretty (ACall f as)     = pretty f ++ ": " ++ (intercalate ", " $ map pretty as)
pretty s@(AString _)    = show $ fromAString s
pretty (ABlock es)      = intercalate "\n" $ map pretty es
pretty (AData s cs)     = s ++ " (Data): " ++ (intercalate " | " $ map fromAConstruct cs)
pretty (AConstruct s d) = s
pretty ANone            = "None"

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
              | ATuple [AtomoVal]
              | AHash [(String, AtomoVal)]
              | AVariable String
              | ADefine Type String AtomoVal
              | AAssign String AtomoVal
              | AObject String [AtomoVal]
              | APrimFunc ([AtomoVal] -> ThrowsError AtomoVal)
              | AIOFunc ([AtomoVal] -> IOThrowsError AtomoVal)
              | AFunc Type String [(Type, String)] AtomoVal
              | ACall AtomoVal [AtomoVal]
              | AString AtomoVal -- AString == AList of AChars
              | ABlock [AtomoVal]
              | AData String [AtomoVal]
              | AConstruct String AtomoVal
              | AIf AtomoVal AtomoVal AtomoVal
              | ANone

instance Eq AtomoVal where
    (AInt a) == (AInt b) = a == b
    (AChar a) == (AChar b) = a == b
    (ADouble a) == (ADouble b) = a == b
    (AList a) == (AList b) = a == b
    (AString a) == (AString b) = a == b
    (AVariable a) == (AVariable b) = a == b
    (ADefine _ _ a) == (ADefine _ _ b) = a == b
    (AAssign _ a) == (AAssign _ b) = a == b

instance Show AtomoVal where
    show (AInt int)       = show int
    show (AChar char)     = show char
    show (ADouble double) = show double
    show (AList str@(AChar _:_)) = show $ AString $ AList str
    show (AList list)     = show list
    show (AHash es)       = "{ " ++ (intercalate ", " (map (\(n, v) -> n ++ ": " ++ show v) es)) ++ " }"
    show (ATuple vs)      = "(" ++ (intercalate ", " (map show vs)) ++ ")"
    show (AVariable n)    = "Variable: " ++ n
    show (ADefine _ _ v)  = show v
    show (AAssign _ v)    = show v
    show (AObject n vs)   = n ++ " (Object):\n" ++ (unlines $ map (" - " ++) $ map show vs)
    show (APrimFunc _)    = "<Function Primitive>"
    show (AIOFunc _)      = "<IO Primitive>"
    show (AFunc t n _ _)  = n ++ " (Function)"
    show (ACall f as)     = show f ++ ": " ++ (intercalate ", " $ map show as)
    show s@(AString _)    = show $ fromAString s
    show (ABlock es)      = intercalate "\n" $ map show es
    show (AData s cs)     = s ++ " (Data): " ++ (intercalate " | " $ map fromAConstruct cs)
    show (AConstruct s d) = s
    show ANone            = "None"

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
getType v = show v

getReturnType (AFunc t _ _ _) = t
getReturnType a = getType a

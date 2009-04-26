module Atomo.Internals where

import Control.Monad.Error
import Data.List (intercalate)
import Text.Parsec (ParseError)

type ThrowsError = Either AtomoError
type IOThrowsError = ErrorT AtomoError IO

data Type = Name String | Type (Type, [Type])
            deriving (Show, Eq)

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
              | AData String [Type] [AtomoVal]
              | AConstruct String [Type] AtomoVal
              | AValue String [AtomoVal] AtomoVal
              | AIf AtomoVal AtomoVal AtomoVal
              | AReturn AtomoVal
              | ANone
              deriving (Show, Eq)

fromAInt (AInt i) = i
fromAInt (AValue "int" [AInt i] _) = i
fromAChar (AChar c) = c
fromADouble (ADouble d) = d
fromAVariable (AVariable n) = n
fromAList (AList l) = l
fromAList (AString l) = fromAList l
fromAString (AString s) = map fromAChar (fromAList s)
fromAString (AList l) = map fromAChar (fromAList (AList l))
fromAConstruct (AConstruct s _ _) = s

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
    show (TypeMismatch expected found) = "Invalid type; expected `" ++ prettyType expected ++ "', found `" ++ prettyType found ++ "'"
    show (NotFunction message func)    = message ++ ": " ++ func
    show (UnboundVar message var)      = message ++ ": " ++ var
    show (Parser err)                  = "Parse error at " ++ show err
    show (Default message)             = message

instance Error AtomoError where
    noMsg = Default "An error has occurred"
    strMsg = Default

pretty :: AtomoVal -> String
pretty (AInt int)       = show int
pretty (AChar char)     = show char
pretty (ADouble double) = show double
pretty (AValue "int" [AInt i] _) = show i
pretty (AValue "double" [ADouble d] _) = show d
pretty (AValue "char" [AChar c] _) = show c
pretty (AList str@(AChar _:_)) = show $ AString $ AList str
pretty (AList list)     = "[" ++ (intercalate ", " (map pretty list)) ++ "]"
pretty (AHash es)       = "{ " ++ (intercalate ", " (map prettyVal es)) ++ " }"
                          where prettyVal (n, (t, v)) = (prettyType t) ++ " " ++ n ++ ": " ++ pretty v
pretty (ATuple vs)      = "(" ++ (intercalate ", " (map prettyVal vs)) ++ ")"
                          where prettyVal (t, v) = (prettyType t) ++ " " ++ pretty v
pretty (AVariable n)    = "<Variable (" ++ n ++ ")>"
pretty (ADefine _ _ v)  = pretty v
pretty (AAssign _ v)    = pretty v
pretty (AObject n vs)   = n ++ " (Object):\n" ++ (unlines $ map (" - " ++) $ map pretty vs)
pretty (APrimFunc n)    = "<Function Primitive (" ++ n ++ ")>"
pretty (AIOFunc n)      = "<IO Primitive (" ++ n ++ ")>"
pretty (AFunc _ n _ _)  = "<Function (" ++ n ++ ")>"
pretty (ACall f as)     = "<Call (" ++ pretty f ++ ") (" ++ intercalate ", " (map pretty as) ++ ")>"
pretty s@(AString _)    = show $ fromAString s
pretty (ABlock es)      = intercalate "\n" $ map pretty es
pretty (AData s as _)   = prettyType $ Type (Name s, as)
pretty (AConstruct s [] _) = s
pretty (AConstruct s ts _) = s ++ "(" ++ intercalate ", " (map prettyType ts) ++ ")"
pretty (AValue v as _)  = v ++ "(" ++ intercalate ", " (map pretty as) ++ ")"
pretty ANone            = "None"
pretty a                = "TODO"

prettyType :: Type -> String
prettyType (Type (Name "[]", [t])) = "[" ++ prettyType t ++ "]"
prettyType (Type (a, ts)) = prettyType a ++ "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (Name a) = a

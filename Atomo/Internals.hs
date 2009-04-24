module Atomo.Internals where

import Control.Monad.Error
import Data.List (intercalate)
import Text.Parsec (ParseError)

data Type = Name String | Type (Type, [Type])
            deriving (Show, Eq)

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
              | AData String [Type] [(String, [Type])]
              | AConstruct String [AtomoVal] AtomoVal
              | AIf AtomoVal AtomoVal AtomoVal
              | AReturn AtomoVal
              | ANone
              deriving (Show, Eq)

fromAInt (AInt i) = i
fromAInt (AConstruct "int" [AInt i] _) = i
fromAChar (AChar c) = c
fromADouble (ADouble d) = d
fromAVariable (AVariable n) = n
fromAList (AList l) = l
fromAList (AString l) = fromAList l
fromAString (AString s) = map fromAChar (fromAList s)
fromAString (AList l) = map fromAChar (fromAList (AList l))
fromAConstruct (AConstruct s _ _) = s
fromAData (AData s _ _) = s

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
    show (TypeMismatch expected found) = "Invalid type; expected " ++ prettyType expected ++ ", found " ++ prettyType found
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
                          verifyList' t (x:xs) | checkType x t = verifyList' t xs
                                               | otherwise = Just (t, getType x)

verifyTuple :: [(Type, AtomoVal)] -> Maybe (Type, Type)
verifyTuple [] = Nothing
verifyTuple ((t, v):vs) | checkType v t = verifyTuple vs
                        | otherwise = Just (t, getType v)

verifyHash :: [(String, (Type, AtomoVal))] -> Maybe (Type, Type)
verifyHash [] = Nothing
verifyHash ((_, (t, v)):vs) | checkType v t = verifyHash vs
                            | otherwise = Just (t, getType v)

getType :: AtomoVal -> Type
getType (ADouble _) = Name "double"
getType (ATuple _) = Name "tuple"
getType (AHash _) = Name "hash"
getType (AString _) = Name "string" -- todo: make type aliases work
getType (AConstruct _ _ (AData n [] _)) = Name n
getType (AConstruct c as (AData n ps cs)) = Type (Name n, args)
                                            where
                                                args = map (\ a -> case lookup a values of
                                                                        Just v -> getType v
                                                                        Nothing -> a) ps
                                                values = zip (argNames cs) as
                                                argNames [] = []
                                                argNames ((n,v):ps) | n == c = v
                                                                    | otherwise = argNames ps
getType (AData n [] _) = Name n
getType (AData n as _) = Type (Name n, as)
getType (AFunc t _ as _) = Type (t, map fst as)
getType (AList []) = Name "[]"
getType (AList as) = Type (Name "[]", [getType (head as)])
getType (AReturn r) = getType r
getType a = Name "unknown"

getReturnType (AFunc t _ _ _) = t
getReturnType a = getType a

-- todo: type aliases, and alias string to [char] in the prelude
--       so we don't have to do this silliness
checkType :: AtomoVal -> Type -> Bool
 -- `[char] foo = "hi"`
checkType (AString _) (Type (Name "[]", [Name "char"]))
          = True
-- `string foo = ['h', 'i']1
checkType (AList (AConstruct _ _ (AData "char" _ _):_)) (Name "string")
          = True
-- Everything matches to "a".."z", at least initially.
checkType _ (Name [_]) = True
-- Constructors that take no argument should always match against their constructor
checkType (AConstruct _ [] (AData n _ _)) (Type (Name n', _)) | n == n' = True
                                                              | otherwise = False
-- Everything else? Match the types.
checkType a b = matchTypes (getType a) b

matchTypes :: Type -> Type -> Bool
matchTypes (Name a) (Name b) = a == b || length a == 1 || length b == 1
matchTypes (Type (a, as)) (Type (b, bs)) = matchTypes a b && (length as) == (length bs) && and (zipWith (matchTypes) as bs)
matchTypes a b = False

-- Deep-replace a type with another type (used for replacing polymorphic types)
swapType :: [Type] -> Type -> Type-> [Type]
swapType ts t n = swapType' ts [] t n
                  where
                      swapType' [] acc _ _ = acc
                      swapType' (t@(Type (a, ts')):ts) acc f r = swapType' ts (acc ++ [Type (a, swapType ts' f r)]) f r
                      swapType' (t@(Name _):ts) acc f r | t == f = swapType' ts (acc ++ [r]) f r
                                                            | otherwise = swapType' ts (acc ++ [t]) f r

pretty :: AtomoVal -> String
pretty (AInt int)       = show int
pretty (AChar char)     = show char
pretty (ADouble double) = show double
pretty (AList str@(AChar _:_)) = show $ AString $ AList str
pretty (AList list)     = "[" ++ (intercalate ", " (map pretty list)) ++ "]"
pretty (AHash es)       = "{ " ++ (intercalate ", " (map prettyVal es)) ++ " }"
                          where prettyVal (n, (t, v)) = (prettyType t) ++ " " ++ n ++ ": " ++ pretty v
pretty (ATuple vs)      = "(" ++ (intercalate ", " (map prettyVal vs)) ++ ")"
                          where prettyVal (t, v) = (prettyType t) ++ " " ++ pretty v
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
pretty (AData s _ cs)   = s ++ " (Data): " ++ (intercalate " | " $ map show cs)
pretty (AConstruct "int" [AInt i] _) = show i
pretty (AConstruct "double" [ADouble d] _) = show d
pretty (AConstruct "char" [AChar c] _) = show c
pretty (AConstruct s [] _) = s
pretty (AConstruct s as _) = s ++ "(" ++ (intercalate ", " (map pretty as)) ++ ")"
pretty ANone            = "None"
pretty a                = show a

prettyType :: Type -> String
prettyType (Type (Name "[]", [t])) = "[" ++ prettyType t ++ "]"
prettyType (Type (a, ts)) = prettyType a ++ "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (Name a) = a

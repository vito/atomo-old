module Atomo.Primitive where

import Atomo.Internals

-- Boolean
primBool :: AtomoVal
primBool = AData "bool" [] [("true", []), ("false", [])]

primTrue :: AtomoVal
primTrue = AConstruct "true" [] primBool

primFalse :: AtomoVal
primFalse = AConstruct "false" [] primBool

primNot :: AtomoVal -> AtomoVal
primNot (AConstruct "true"  _ _) = primFalse
primNot (AConstruct "false" _ _) = primTrue

boolToPrim :: Bool -> AtomoVal
boolToPrim True  = primTrue
boolToPrim False = primFalse

-- Integers
primInt :: AtomoVal
primInt = AData "int" [] [("int", [Name "a"])]

intToPrim :: Integer -> AtomoVal
intToPrim i = AConstruct "int" [AInt i] primInt

primToInt :: AtomoVal -> Integer
primToInt (AInt i) = i
primToInt (AConstruct "int" [AInt i] _) = i

-- Doubles
primDouble :: AtomoVal
primDouble = AData "double" [] [("double", [Name "a"])]

doubleToPrim :: Double -> AtomoVal
doubleToPrim d = AConstruct "double" [ADouble d] primDouble

primToDouble :: AtomoVal -> Double
primToDouble (ADouble d) = d
primToDouble (AConstruct "double" [ADouble d] _) = d

-- Characters
primChar :: AtomoVal
primChar = AData "char" [] []

charToPrim :: Char -> AtomoVal
charToPrim c = AConstruct (show c) [] primChar

primToChar :: AtomoVal -> Char
primToChar (AChar c) = c
primToChar (AConstruct c _ _) = read c

-- Primitive functions
primSub, primAdd, primMul, primDiv :: AtomoVal -> AtomoVal -> AtomoVal
primSub (AConstruct "int" [AInt a] _) (AConstruct "int" [AInt b] _)
        = intToPrim $ a - b
primSub (AConstruct "double" [ADouble a] _) (AConstruct "double" [ADouble b] _)
        = doubleToPrim $ a - b
primAdd (AConstruct "int" [AInt a] _) (AConstruct "int" [AInt b] _)
        = intToPrim $ a + b
primAdd (AConstruct "double" [ADouble a] _) (AConstruct "double" [ADouble b] _)
        = doubleToPrim $ a + b
primMul (AConstruct "int" [AInt a] _) (AConstruct "int" [AInt b] _)
        = intToPrim $ a * b
primMul (AConstruct "double" [ADouble a] _) (AConstruct "double" [ADouble b] _)
        = doubleToPrim $ a * b
primDiv (AConstruct "int" [AInt a] _) (AConstruct "int" [AInt b] _)
        = intToPrim $ a `div` b
primDiv (AConstruct "double" [ADouble a] _) (AConstruct "double" [ADouble b] _)
        = doubleToPrim $ a / b

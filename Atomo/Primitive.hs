module Atomo.Primitive where

import Atomo.Internals

-- Boolean
primBool :: AtomoVal
primBool = AData "bool" [] [primTrueC, primFalseC]

primTrueC :: AtomoVal
primTrueC = AConstruct "true" [] primBool

primFalseC :: AtomoVal
primFalseC = AConstruct "false" [] primBool

primTrue :: AtomoVal
primTrue = AValue "true" [] primBool

primFalse :: AtomoVal
primFalse = AValue "false" [] primBool

primNot :: AtomoVal -> AtomoVal
primNot (AConstruct "true"  _ _) = primFalse
primNot (AConstruct "false" _ _) = primTrue

boolToPrim :: Bool -> AtomoVal
boolToPrim True  = primTrue
boolToPrim False = primFalse

-- Integers
isAInt :: AtomoVal -> Bool
isAInt (AValue "int" _ _) = True
isAInt _ = False

primInt :: AtomoVal
primInt = AData "int" [] [primIntC]

primIntC :: AtomoVal
primIntC = AConstruct "int" [Name "a"] primInt

intToPrim :: Integer -> AtomoVal
intToPrim i = AValue "int" [AInt i] primInt

{- primToInt :: AtomoVal -> Integer -}
{- primToInt (AInt i) = i -}
{- primToInt (AConstruct "int" [AInt i] _) = i -}

-- Doubles
isADouble :: AtomoVal -> Bool
isADouble (AValue "double" _ _) = True
isADouble _ = False

primDouble :: AtomoVal
primDouble = AData "double" [] [primDoubleC]

primDoubleC :: AtomoVal
primDoubleC = AConstruct "double" [Name "a"] primDouble

doubleToPrim :: Double -> AtomoVal
doubleToPrim d = AValue "double" [ADouble d] primDouble

{- primToDouble :: AtomoVal -> Double -}
{- primToDouble (ADouble d) = d -}
{- primToDouble (AConstruct "double" [ADouble d] _) = d -}

-- Characters
isAChar :: AtomoVal -> Bool
isAChar (AValue "char" _ _) = True
isAChar _ = False

primChar :: AtomoVal
primChar = AData "char" [] [primCharC]

primCharC :: AtomoVal
primCharC = AConstruct "char" [Name "a"] primChar

charToPrim :: Char -> AtomoVal
charToPrim c = AValue "char" [AChar c] primChar

{- primToChar :: AtomoVal -> Char -}
{- primToChar (AChar c) = c -}
{- primToChar (AConstruct "char" [AChar c] _) = c -}

-- Primitive functions
primSub, primAdd, primMul, primDiv :: AtomoVal -> AtomoVal -> AtomoVal
primSub a b | isAInt a && isAInt b = intToPrim $ (fromAInt a) - (fromAInt b)
            | isADouble a && isADouble b = doubleToPrim $ (fromADouble a) - (fromADouble b)
primAdd a b | isAInt a && isAInt b = intToPrim $ fromAInt a + fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ (fromADouble a) + (fromADouble b)
primMul a b | isAInt a && isAInt b = intToPrim $ fromAInt a * fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ (fromADouble a) * (fromADouble b)
primDiv a b | isAInt a && isAInt b = intToPrim $ fromAInt a `div` fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ (fromADouble a) / (fromADouble b)

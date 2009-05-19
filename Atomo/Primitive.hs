module Atomo.Primitive where

import Atomo.Internals

import Control.Monad.Trans
import Data.Maybe (fromJust)

-- Boolean
primBool :: AtomoVal
primBool = AData "Bool" []

primTrueC :: AtomoVal
primTrueC = AConstruct "True" [] primBool

primFalseC :: AtomoVal
primFalseC = AConstruct "False" [] primBool

primTrue :: AtomoVal
primTrue = AValue "True" [] primBool

primFalse :: AtomoVal
primFalse = AValue "False" [] primBool

primNot :: AtomoVal -> AtomoVal
primNot (AValue "True"  _ _) = primFalse
primNot (AValue "False" _ _) = primTrue

boolToPrim :: Bool -> AtomoVal
boolToPrim True  = primTrue
boolToPrim False = primFalse

-- Integers
isAInt :: AtomoVal -> Bool
isAInt (AValue "Int" _ _) = True
isAInt _ = False

primInt :: AtomoVal
primInt = AData "Int" []

primIntC :: AtomoVal
primIntC = AConstruct "Int" [Name "a"] primInt

intToPrim :: Integer -> AtomoVal
intToPrim i = AValue "Int" [AInt i] primInt

-- Doubles
isADouble :: AtomoVal -> Bool
isADouble (AValue "Double" _ _) = True
isADouble _ = False

primDouble :: AtomoVal
primDouble = AData "Double" []

primDoubleC :: AtomoVal
primDoubleC = AConstruct "Double" [Name "a"] primDouble

doubleToPrim :: Double -> AtomoVal
doubleToPrim d = AValue "Double" [ADouble d] primDouble

-- Characters
isAChar :: AtomoVal -> Bool
isAChar (AValue "Char" _ _) = True
isAChar _ = False

primChar :: AtomoVal
primChar = AData "Char" []

primCharC :: AtomoVal
primCharC = AConstruct "Char" [Name "a"] primChar

charToPrim :: Char -> AtomoVal
charToPrim c = AValue "Char" [AChar c] primChar

-- Primitive functions
getPrim :: String -> ([AtomoVal] -> IOThrowsError AtomoVal)
getPrim = snd . fromJust . (flip lookup primFuncs)

primSub, primAdd, primMul, primDiv :: AtomoVal -> AtomoVal -> AtomoVal
primSub a b | isAInt a && isAInt b = intToPrim $ fromAInt a - fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ fromADouble a - fromADouble b
primAdd a b | isAInt a && isAInt b = intToPrim $ fromAInt a + fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ fromADouble a + fromADouble b
primMul a b | isAInt a && isAInt b = intToPrim $ fromAInt a * fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ fromADouble a * fromADouble b
primDiv a b | isAInt a && isAInt b = intToPrim $ fromAInt a `div` fromAInt b
            | isADouble a && isADouble b = doubleToPrim $ fromADouble a / fromADouble b

primFuncs :: [(String, ([String], [AtomoVal] -> IOThrowsError AtomoVal))]
primFuncs = [ ("++", (["x", "b"], concatFunc))
            , ("==", (["x", "b"], equalityFunc))
            , ("/=", (["a", "b"], inequalityFunc))
            , ("+", (["x", "b"], addFunc)) -- Where "a" is `int` or `double`.
            , ("-", (["x", "b"], subFunc)) -- However, this needs to be expanded
            , ("*", (["x", "b"], mulFunc)) -- to typeclasses eventually. (TODO)
            , ("/", (["x", "b"], divFunc))
            , ("<", (["x", "b"], lessFunc))
            , ("show", (["x"], showFunc))

            -- IO Primitives
            , ("print", (["x"], printFunc))
            , ("dump", (["a"], dumpFunc))
            ]
            where
                addFunc [a, b] = return $ primAdd a b
                subFunc [a, b] = return $ primSub a b
                mulFunc [a, b] = return $ primMul a b
                divFunc [a, b] = return $ primDiv a b
                
                showFunc [a] = return $ toAString $ pretty a

                concatFunc [a, b] = return $ AList ((fromAList a) ++ (fromAList b))

                equalityFunc [ (AValue a as (AData ad _))
                             , (AValue b bs (AData bd _))
                             ] = return $ boolToPrim (a == a && as == bs && ad == bd)
                equalityFunc [a, b] = return $ boolToPrim (a == b)

                inequalityFunc [a, b] = equalityFunc [a, b] >>= return . primNot

                lessFunc [a, b] = return $ boolToPrim $ (<) (fromAInt a) (fromAInt b)

                printFunc [x] = liftIO $ (putStrLn . fromAString) x >> return ANone
                dumpFunc [x] = liftIO $ (putStrLn . pretty) x >> return ANone


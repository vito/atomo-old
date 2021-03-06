module Atomo.Primitive where

import Atomo.Internals

import Control.Concurrent.Chan
import Control.Monad.Trans
import Data.Maybe (fromJust)
import qualified System.IO.UTF8 as U

-- Boolean
primBool :: AtomoVal
primBool = AData "Bool" []

primTrueC :: AtomoVal
primTrueC = AConstruct "True" [] primBool

primFalseC :: AtomoVal
primFalseC = AConstruct "False" [] primBool

primTrue :: AtomoVal
primTrue = AValue "True" [] primTrueC

primFalse :: AtomoVal
primFalse = AValue "False" [] primFalseC

primNot :: AtomoVal -> AtomoVal
primNot (AValue "True"  _ _) = primFalse
primNot (AValue "False" _ _) = primTrue

boolToPrim :: Bool -> AtomoVal
boolToPrim True  = primTrue
boolToPrim False = primFalse

-- Integers
isAInt :: AtomoVal -> Bool
isAInt (AValue "Integer" _ _) = True
isAInt _ = False

primInt :: AtomoVal
primInt = AData "Integer" []

primIntC :: AtomoVal
primIntC = AConstruct "Integer" [Name "a"] primInt

intToPrim :: Integer -> AtomoVal
intToPrim i = AValue "Integer" [AInt i] primIntC

-- Doubles
isADouble :: AtomoVal -> Bool
isADouble (AValue "Double" _ _) = True
isADouble _ = False

primDouble :: AtomoVal
primDouble = AData "Double" []

primDoubleC :: AtomoVal
primDoubleC = AConstruct "Double" [Name "a"] primDouble

doubleToPrim :: Double -> AtomoVal
doubleToPrim d = AValue "Double" [ADouble d] primDoubleC

-- Characters
isAChar :: AtomoVal -> Bool
isAChar (AValue "Char" _ _) = True
isAChar _ = False

primChar :: AtomoVal
primChar = AData "Char" []

primCharC :: AtomoVal
primCharC = AConstruct "Char" [Name "a"] primChar

charToPrim :: Char -> AtomoVal
charToPrim c = AValue "Char" [AChar c] primCharC

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
primFuncs = [ ("++", (["a", "b"], concatFunc))
            , ("|", (["a", "b"], consFunc))
            , ("==", (["a", "b"], equalityFunc))
            , ("/=", (["a", "b"], inequalityFunc))
            , ("+", (["a", "b"], addFunc)) -- Where "a" is `int` or `double`.
            , ("-", (["a", "b"], subFunc)) -- However, this needs to be expanded
            , ("*", (["a", "b"], mulFunc)) -- to typeclasses eventually. (TODO)
            , ("/", (["a", "b"], divFunc))
            , ("<", (["a", "b"], lessFunc))
            , (">", (["a", "b"], greaterFunc))
            , ("!", (["a", "b"], sendFunc))
            , ("show", (["a"], showFunc))
            , ("error", (["a"], errorFunc))

            -- IO Primitives
            , ("print", (["a"], printFunc))
            , ("dump", (["a"], dumpFunc))
            ]
            where
                addFunc [a, b] = return $ primAdd a b
                subFunc [a, b] = return $ primSub a b
                mulFunc [a, b] = return $ primMul a b
                divFunc [a, b] = return $ primDiv a b

                concatFunc [a, b] = return $ AList ((fromAList a) ++ (fromAList b))

                consFunc [a, (AList xs)] = return $ AList (a:xs)

                equalityFunc [a, b] = return $ boolToPrim (a == b)

                inequalityFunc [a, b] = equalityFunc [a, b] >>= return . primNot

                lessFunc [a, b] = return $ boolToPrim $ (<) (fromAInt a) (fromAInt b)
                greaterFunc [a, b] = return $ boolToPrim $ (>) (fromAInt a) (fromAInt b)

                sendFunc [(AProcess _ chan), v] = do liftIO (writeChan chan v)
                                                     return ANone

                showFunc [a] = return $ toAString $ pretty a

                errorFunc [a] = error (fromAString a)

                printFunc [x] = liftIO $ (U.putStrLn . fromAString) x >> return ANone
                dumpFunc [x] = liftIO $ (U.putStrLn . pretty) x >> return ANone


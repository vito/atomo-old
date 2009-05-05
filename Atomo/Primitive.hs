module Atomo.Primitive where

import Atomo.Internals

import Control.Monad.Trans
import Data.Maybe (fromJust)

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
primNot (AValue "true"  _ _) = primFalse
primNot (AValue "false" _ _) = primTrue

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

-- Primitive functions
getAIOPrim :: String -> AtomoVal
getAIOPrim = fst . fromJust . (flip lookup ioPrims)

getIOPrim :: String -> (AtomoVal -> IOThrowsError AtomoVal)
getIOPrim = snd . fromJust . (flip lookup ioPrims)

getPrim :: String -> ([AtomoVal] -> ThrowsError AtomoVal)
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

primFuncs :: [(String, ([String], [AtomoVal] -> ThrowsError AtomoVal))]
primFuncs = [ ("++", (["a", "b"], concatFunc))
            , ("==", (["a", "b"], equalityFunc))
            , ("/=", (["a", "b"], inequalityFunc))
            , ("+", (["a", "b"], addFunc)) -- Where "a" is `int` or `double`.
            , ("-", (["a", "b"], subFunc)) -- However, this needs to be expanded
            , ("*", (["a", "b"], mulFunc)) -- to typeclasses eventually. (TODO)
            , ("/", (["a", "b"], divFunc))
            , ("<", (["a", "b"], lessFunc))
            , ("show", (["a"], showFunc))
            , ("typeOf", (["a"], typeFunc))
            ]
            where
                addFunc [a, b] = return $ primAdd a b
                subFunc [a, b] = return $ primSub a b
                mulFunc [a, b] = return $ primMul a b
                divFunc [a, b] = return $ primDiv a b
                
                showFunc [a] = return $ toAString $ pretty a

                concatFunc [a, b] = return $ AList ((fromAList a) ++ (fromAList b))

                equalityFunc [(AInt a), (AInt b)] = return $ boolToPrim (a == b)
                equalityFunc [(AChar a), (AChar b)] = return $ boolToPrim (a == b)
                equalityFunc [(ADouble a), (ADouble b)] = return $ boolToPrim (a == b)
                equalityFunc [(AList a), (AList b)] = return $ boolToPrim (a == b)
                equalityFunc [(AString a), (AString b)] = return $ boolToPrim (a == b)
                equalityFunc [(AVariable a), (AVariable b)] = return $ boolToPrim (a == b)
                equalityFunc [(AMutate _ a), (AMutate _ b)] = return $ boolToPrim (a == b)
                equalityFunc [(AData a as _), (AData b bs _)] = return $ boolToPrim (a == b && as == bs)
                equalityFunc [(AValue a as _), (AValue b bs _)] = return $ boolToPrim (a == b && as == bs)
                equalityFunc [(AConstruct a as _), (AConstruct b bs _)] = return $ boolToPrim (a == b && as == bs)
                equalityFunc [a, b] = return $ boolToPrim (a == b)

                inequalityFunc [a, b] = equalityFunc [a, b] >>= return . primNot

                lessFunc [a, b] = return $ boolToPrim $ (<) (fromAInt a) (fromAInt b)

                typeFunc [a] = return . toAString . prettyType $ getType a

-- Primitive I/O functions
ioPrims :: [(String, (AtomoVal, AtomoVal -> IOThrowsError AtomoVal))]
ioPrims = [ ("print", (AIOFunc "print", printFunc))
          , ("dump", (AIOFunc "dump", dumpFunc))
          ]
          where
              printFunc x = liftIO $ (putStrLn . fromAString) x >> return ANone
              dumpFunc x = liftIO $ (putStrLn . pretty) x >> return ANone


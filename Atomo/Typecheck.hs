module Atomo.Typecheck where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive (primFuncs, ioPrims)

import Control.Monad.Error

type CheckEnv = ([(String, Type)], [(String, Type)])
data TypeCheck = Pass (CheckEnv, Type) | Poly (CheckEnv, [(Type, Type)]) | Error AtomoError
                 deriving (Show)

getAnyType :: CheckEnv -> String -> Maybe Type
getAnyType e n = case lookup n (fst e) of
                      Just t -> Just t
                      Nothing -> lookup n (snd e)

Pass _ >>* k = k
Poly _ >>* k = k
Error e >>* _ = Error e

-- todo: type aliases, and alias string to [char] in the prelude
--       so we don't have to do this silliness
checkType :: CheckEnv -> Type -> Type -> TypeCheck
checkType e (Name "string") (Type (Name "[]", [Name "char"]))
          = Pass (e, Name "string")
checkType e (Type (Name "[]", [Name "char"])) (Name "string")
          = Pass (e, Name "string")
checkType e _ (Name [a]) = Pass (e, Name [a])
checkType e (Name [a]) _ = Pass (e, Name [a])
-- Constructors that take no argument should always match against their constructor
checkType e (Type (Type (Name n, as), [])) t@(Type (Name n', _)) | n == n' = Pass (e, t)
                                                                 | otherwise = Error $ TypeMismatch t (Type (Name n, as))
checkType e a b = matchTypes e a b

matchTypes :: CheckEnv -> Type -> Type -> TypeCheck
{- matchTypes e a b = Pass e -}
matchTypes e (Name a) (Name b) | a == b || length a == 1 || length b == 1 = Pass (e, Name a)
                               | otherwise = Error $ TypeMismatch (Name a) (Name b)
matchTypes e (Type (a, as)) (Type (b, bs)) | consEq && numArgsEq && argsEq = Pass (e, Type (a, as))
                                           | otherwise = Error $ TypeMismatch (Type (a, as)) (Type (b, bs))
                                           where
                                               consEq = case matchTypes e a b of
                                                             Pass _ -> True
                                                             _ -> False
                                               numArgsEq = length as == length bs
                                               argsEq = and $ map (\ a -> case a of
                                                                               Pass _ -> True
                                                                               _ -> False) (zipWith (matchTypes e) as bs)
matchTypes e a b = Error $ TypeMismatch a b

-- Deep-replace a type with another type (used for replacing polymorphic types)
swapType :: [Type] -> Type -> Type -> [Type]
swapType ts t n = swapType' ts [] t n
                  where
                      swapType' [] acc _ _ = acc
                      swapType' (t@(Type (a, ts')):ts) acc f r = swapType' ts (acc ++ [Type (a, swapType ts' f r)]) f r
                      swapType' (t@(Name _):ts) acc f r | t == f = swapType' ts (acc ++ [r]) f r
                                                            | otherwise = swapType' ts (acc ++ [t]) f r

-- Ensure that all AtomoVals match a specified type
allType :: CheckEnv -> Type -> [Type] -> TypeCheck
allType e t [] = Pass (e, t)
allType e t (x:xs) = case checkType e x t of
                          Pass _ -> allType e t xs
                          a -> a

verifyList :: CheckEnv -> [AtomoVal] -> TypeCheck
verifyList e (x:xs) = allType e (exprType e x) (map (exprType e) xs)

verifyTuple :: CheckEnv -> [(Type, AtomoVal)] -> TypeCheck
verifyTuple e [] = Pass (e, Name "tuple")
verifyTuple e ((t, v):vs) = case checkType e (exprType e v) t of
                                 Pass (e, _) -> verifyTuple e vs
                                 a -> a

verifyHash :: CheckEnv -> [(String, (Type, AtomoVal))] -> TypeCheck
verifyHash e [] = Pass (e, Name "hash")
verifyHash e ((_, (t, v)):vs) = case checkType e (exprType e v) t of
                                     Pass (e, _) ->  verifyHash e vs
                                     a -> a

checkAST :: ThrowsError [AtomoVal] -> ThrowsError [AtomoVal]
checkAST (Right a) = checkExprs env a a
                     where
                        funEnv = map (\(n, (a, f)) -> (n, getType a)) primFuncs
                        ioEnv = map (\(n, (a, f)) -> (n, getType a)) ioPrims
                        env = (funEnv ++ ioEnv, [])
checkAST (Left err) = throwError err

checkExprs :: CheckEnv -> [AtomoVal] -> [AtomoVal] -> ThrowsError [AtomoVal]
checkExprs _ [] t = return t
checkExprs e (a:as) t = case checkExpr e a of
                             Pass (e, _) -> checkExprs e as t
                             Poly (e, _) -> checkExprs e as t
                             Error e -> throwError e

exprType :: CheckEnv -> AtomoVal -> Type
exprType e v = case checkExpr e v of
                    Pass (_, t) -> t
                    Error e -> error (show e) -- TODO

checkExpr :: CheckEnv -> AtomoVal -> TypeCheck
checkExpr e (AList as) = verifyList e as
checkExpr e (ATuple as) = verifyTuple e as
checkExpr e (AHash as) = verifyHash e as
checkExpr e (ADefine t n v) = checkExpr e v >>* checkType e t (exprType e v) >>* Pass (newEnv, t)
                              where
                                  newEnv = (fst e, (n, t) : snd e)
checkExpr e (AData n [] cs) = Pass (newEnv, Name n)
                              where
                                  newGlobal = map (\c -> (fromAConstruct c, exprType e c)) cs ++ fst e
                                  newEnv = (newGlobal, snd e)
checkExpr e (AData n as cs) = Pass (newEnv, Type (Name n, as))
                              where
                                  newGlobal = map (\c -> (fromAConstruct c, exprType e c)) cs ++ fst e
                                  newEnv = (newGlobal, snd e)
checkExpr e (ACall (AIOFunc t n ps) as) = checkArgs e ps (map (exprType e) as) >>* Pass (e, t)
checkExpr e (ACall (APrimFunc t n ps) as) = checkArgs e ps (map (exprType e) as) >>* Pass (e, t)
checkExpr e (ACall (AVariable n) as) = case getAnyType e n of
                                            Just (Type (f, ts)) -> case checkArgs e ts (map (exprType e) as) of
                                                                        Poly (e, rs) -> Pass (e, findDiff f rs)
                                                                        a -> a
                                            Just a -> Error $ NotFunction n
                                            Nothing -> Error $ UnboundVar n
                                       where
                                           findDiff d [] = d
                                           findDiff d ((f, r):ts) | replaced /= d = replaced
                                                                  | otherwise = findDiff d ts
                                                                  where
                                                                      replaced = head $ swapType [d] f r
checkExpr e (AIf c t f) = checkType e (Name "bool") (exprType e c) >>* checkExpr e t >>* checkExpr e f
checkExpr e (ABlock as) = checkAll e as
checkExpr e (AFunc t n ps b) = case checkExpr newEnv b of
                                    Pass (e, r) -> if r == t then Pass (e, t) else Error $ TypeMismatch t r
                                    a -> a
                               where
                                   globalEnv = (n, Type (t, map fst ps)) : fst e
                                   localEnv = map (\(a, b) -> (b, a)) ps ++ snd e
                                   newEnv = (globalEnv, localEnv)
checkExpr e (AVariable n) = case getAnyType e n of
                                 Just t -> Pass (e, t)
                                 Nothing -> Error $ UnboundVar n
checkExpr e (AValue n as d) = Pass (e, exprType e d)
checkExpr e (AString as) = Pass (e, Name "string")
checkExpr e (AConstruct n [] (AData d [] _)) = Pass (e, Name d)
checkExpr e (AConstruct n [] (AData d ps _)) = Pass (e, Type (Name d, ps))
checkExpr e (AConstruct n as (AData d ps _)) = Pass (e, Type (Type (Name d, ps), as))
checkExpr e v = Pass (e, Name (show v)) -- TODO: This is for debugging.

-- Check all expressions and return the return type.
checkAll :: CheckEnv -> [AtomoVal] -> TypeCheck
checkAll e as = checkAll' e as (Name "void")
                where
                    checkAll' e [] t = Pass (e, t)
                    checkAll' e (AReturn r:_) _ = Pass (e, exprType e r)
                    checkAll' e (a:as) t = case checkExpr e a of
                                                Pass (e, t) -> checkAll' e as t
                                                a -> a

-- Check the arguments against the types the function defines,
-- and return any polymorphic types along with their replacement.
checkArgs :: CheckEnv -> [Type] -> [Type] -> TypeCheck
checkArgs e ts' as' | length ts' /= length as' = Error $ NumArgs (length ts') (length as')
                    | otherwise = checkArgs' e ts' as' []
                      where
                          checkArgs' e [] [] rs = Poly (e, rs)
                          checkArgs' e (t:ts) (a:as) rs = checkType e t a >>* checkArgs' e (swapPoly ts t a) as polys
                                                          where
                                                              polys = case t of
                                                                           (Name [_]) -> (t, a) : rs
                                                                           _ -> rs
                          swapPoly ts t@(Name [_]) n = swapType ts t n
                          swapPoly ts _ _ = ts

module Atomo.Typecheck where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive (primFuncs)

import Control.Monad.Error
import Text.Parsec.Pos (SourcePos, newPos)

type CheckEnv = [[(Index, Type)]]
data TypeCheck = Pass (CheckEnv, Type) | Polys (CheckEnv, [(Type, Type)]) | Error (SourcePos -> AtomoError)

instance Show TypeCheck where
    show (Pass (e, t)) = "Pass (" ++ show e ++ ", " ++ show t ++ ")"
    show (Polys (e, ts)) = "Polys (" ++ show e ++ ", " ++ show ts ++ ")"
    show (Error e) = "Error (...)"

getAnyType :: CheckEnv -> Index -> Maybe Type
getAnyType [] n = Nothing
getAnyType (s:ss) n = case lookup n s of
                           Just t -> Just t
                           Nothing -> getAnyType ss n

Pass _ >>* k = k
Polys _ >>* k = k
Error e >>* _ = Error e

-- todo: type aliases, and alias string to [char] in the prelude
--       so we don't have to do this silliness
checkType :: CheckEnv -> Type -> Type -> TypeCheck
checkType e _ (Poly a) = Pass (e, Poly a)
checkType e (Poly a) _ = Pass (e, Poly a)
-- Constructors that take no argument should always match against their constructor
checkType e (Func None (Type (Name c) as)) t@(Type (Name d) _) | c == d = Pass (e, t)
                                                               | otherwise = Error $ TypeMismatch t (Type (Name c) as)
checkType e t@(Type (Name a) [aa]) f@(Type (Name b) [Poly _]) | a == b = Pass (e, t)
                                                              | otherwise = checkType e t f
checkType e a@(Name n) b = case getAnyType e (Define n) of
                                Just t -> checkType e t b
                                Nothing -> matchTypes e a b
checkType e a b@(Name n) = case getAnyType e (Define n) of
                                Just t -> checkType e a t
                                Nothing -> matchTypes e a b
checkType e a b = matchTypes e a b

matchTypes :: CheckEnv -> Type -> Type -> TypeCheck
matchTypes e (Name a) (Name b) | a == b || length a == 1 || length b == 1 = Pass (e, Name a)
                               | otherwise = Error $ TypeMismatch (Name a) (Name b)
matchTypes e (Type a as) (Type b bs) | consEq && numArgsEq && argsEq = Pass (e, Type a as)
                                     | otherwise = Error $ TypeMismatch (Type a as) (Type b bs)
                                     where
                                         consEq = case checkType e a b of
                                                       Pass _ -> True
                                                       _ -> False
                                         numArgsEq = length as == length bs
                                         argsEq = and $ map (\ a -> case a of
                                                                         Pass _ -> True
                                                                         _ -> False) (zipWith (checkType e) as bs)
matchTypes e a b = Error $ TypeMismatch a b

-- Ensure that all AtomoVals match a specified type
allType :: CheckEnv -> Type -> [Type] -> TypeCheck
allType e t [] = Pass (e, t)
allType e t (x:xs) = case checkType e x t of
                          Pass _ -> allType e t xs
                          a -> a

verifyList :: CheckEnv -> [AtomoVal] -> TypeCheck
verifyList e [] = Pass (e, Type (Name "[]") [Name "a"])
verifyList e xs = case either id (\(h:ts) -> allType e h ts) $ checkTypes e xs [] of
                       Pass (e, t) -> Pass (e, Type (Name "[]") [t])
                       e -> e

verifyHash :: CheckEnv -> [(String, (Type, AtomoVal))] -> TypeCheck
verifyHash e [] = Pass (e, Name "hash")
verifyHash e ((_, (t, v)):vs) = either id (\r -> case checkType e r t of
                                                      Pass (e, _) -> verifyHash e vs
                                                      a -> a) $ exprType e v

checkAST :: ThrowsError [(SourcePos, AtomoVal)] -> ThrowsError [AtomoVal]
checkAST (Right a) = checkExprs env a (map snd a)
                     where
                        env = []
checkAST (Left err) = throwError err

checkExprs :: CheckEnv -> [(SourcePos, AtomoVal)] -> [AtomoVal] -> ThrowsError [AtomoVal]
checkExprs _ [] t = return t
checkExprs e ((p, a):as) t = case checkExpr e a of
                                  Pass (e, _) -> checkExprs e as t
                                  Polys (e, _) -> checkExprs e as t
                                  Error e -> throwError (e p)

exprType :: CheckEnv -> AtomoVal -> Either TypeCheck Type
exprType e v = case checkExpr e v of
                    Pass (_, t) -> Right t
                    a -> Left a

checkExpr :: CheckEnv -> AtomoVal -> TypeCheck
{- checkExpr e (AList as) = verifyList e as -}
checkExpr e (ATuple as) = either id (\ts -> Pass (e, Type (Name "()") ts)) $ checkTypes e as []
checkExpr e (AHash as) = verifyHash e as
checkExpr e (ADefine n v) = either id (\r -> Pass (newEnv r, r)) $ exprType e v
                            where
                                newEnv r = ((n, r) : head e) : tail e
checkExpr e (AIf c t f) = either id (\r -> checkType e (Name "Bool") r >>*
                                           checkExpr e t >>*
                                           checkExpr e f) $ exprType e c
checkExpr e (ABlock as) = checkAll e as
checkExpr e (AVariable n) = case getAnyType e (Define n) of
                                 Just t -> Pass (e, t)
                                 Nothing -> Error $ UnboundVar n
checkExpr e (AString as) = Pass (e, Type (Name "[]") [Name "Char"])
checkExpr e (AType n t) = Pass ((((Define n), t) : head e) : tail e, t)
checkExpr e (AAnnot n t) = Pass ((((Define n), t) : head e) : tail e, t)
checkExpr e v = Pass (e, Name (show v)) -- TODO: This is for debugging.

-- Check all expressions and return the return type.
checkAll :: CheckEnv -> [AtomoVal] -> TypeCheck
checkAll e as = checkAll' e as (Name "()")
                where
                    checkAll' e [] t = Pass (e, t)
                    checkAll' e (AReturn r:_) _ = either id (\t -> Pass (e, t)) $ exprType e r
                    checkAll' e (a:as) t = case checkExpr e a of
                                                Pass (e, t) -> checkAll' e as t
                                                a -> a

-- Check the arguments against the types the function defines,
-- and return any polymorphic types along with their replacement.
checkArgs :: CheckEnv -> [Type] -> [Type] -> TypeCheck
checkArgs e ts' as' | length ts' /= length as' = Error $ NumArgs (length ts') (length as')
                    | otherwise = checkArgs' e ts' as' []
                      where
                          checkArgs' e [] [] rs = Polys (e, rs)
                          checkArgs' e (t:ts) (a:as) rs = checkType e t a >>* checkArgs' e (swapPoly ts t a) as polys
                                                          where
                                                              polys = case t of
                                                                           (Poly _) -> (t, a) : rs
                                                                           _ -> rs
                          swapPoly ts t@(Poly _) n = swapType ts t n
                          swapPoly ts _ _ = ts

checkTypes :: CheckEnv -> [AtomoVal] -> [Type] -> Either TypeCheck [Type]
checkTypes e [] acc = Right acc
checkTypes e (a:as) acc = either Left (\t -> checkTypes e as (acc ++ [t])) $ exprType e a

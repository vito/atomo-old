module Atomo.Typecheck where

import Atomo.Error
import Atomo.Internals

import Control.Monad.Error

type CheckEnv = ([(String, Type)], [(String, Type)])
data TypeCheck = Pass CheckEnv | Mismatch (Type, Type)
                 deriving (Eq, Show)

getAnyType :: CheckEnv -> String -> Maybe Type
getAnyType e n = case lookup n (fst e) of
                      Just t -> Just t
                      Nothing -> lookup n (snd e)

Pass e >>* k = k
Mismatch (f, r) >>* _ = Mismatch (f, r)

getType :: CheckEnv -> AtomoVal -> Type
getType e (ADouble _) = Name "double"
getType e (AList []) = Name "[]"
getType e (AList as) = Type (Name "[]", [getType e (head as)])
getType e (ATuple _) = Name "tuple"
getType e (AHash _) = Name "hash"
getType e (AString _) = Name "string" -- todo: make type aliases work
getType e (AConstruct _ [] d@(AData n [] _)) = getType e d
getType e (AConstruct _ ts d@(AData n ps _)) = Type (getType e d, ts)
getType e (AData n [] _) = Name n
getType e (AData n as _) = Type (Name n, as)
getType e (AFunc t _ as _) = Type (t, map fst as)
getType e (AReturn r) = getType e r
getType e (ADefine _ _ v) = getType e v
getType e (AValue _ _ d@(AData n [] _)) = getType e d
getType e (AValue c as (AData n ps cs)) = Type (Name n, args)
                                          where
                                              args = map (\ a -> case lookup a values of
                                                                      Just v -> getType e v
                                                                      Nothing -> a) ps
                                              values = zip (argNames cs) as
                                              argNames [] = []
                                              argNames ((AConstruct n v _):ps) | n == c = v
                                                                  | otherwise = argNames ps
getType e (AVariable n) = case getAnyType e n of
                               Just t -> t
                               Nothing -> error ("Reference to undefined variable `" ++ n ++ "'")
getType e c@(ACall (AVariable n) _) = case getAnyType e n of
                                           Just (Type (a, _)) -> case checkExpr e c of
                                                                      Pass e -> a
                                                                      _ -> error $ "TODO"
                                           Nothing -> error ("Reference to undefined variable `" ++ n ++ "'")
getType _ a = error ("Cannot get type of `" ++ pretty a ++ "'")

getReturnType e (AFunc t _ _ _) = t
getReturnType e a = getType e a

-- todo: type aliases, and alias string to [char] in the prelude
--       so we don't have to do this silliness
checkType :: CheckEnv -> Type -> Type -> TypeCheck
checkType e (Name "string") (Type (Name "[]", [Name "char"]))
          = Pass e
checkType e (Type (Name "[]", [Name "char"])) (Name "string")
          = Pass e
checkType e _ (Name [_]) = Pass e
checkType e (Name [_]) _ = Pass e
-- Constructors that take no argument should always match against their constructor
checkType e (Type (Type (Name n, as), [])) t@(Type (Name n', _)) | n == n' = Pass e
                                                                 | otherwise = Mismatch (t, Type (Name n, as))
checkType e a b = matchTypes e a b

matchTypes :: CheckEnv -> Type -> Type -> TypeCheck
{- matchTypes e a b = Pass e -}
matchTypes e (Name a) (Name b) | a == b || length a == 1 || length b == 1 = Pass e
                               | otherwise = Mismatch (Name a, Name b)
matchTypes e (Type (a, as)) (Type (b, bs)) | consEq && numArgsEq && argsEq = Pass e
                                           | otherwise = Mismatch (Type (a, as), Type (b, bs))
                                           where
                                               consEq = matchTypes e a b == Pass e
                                               numArgsEq = length as == length bs
                                               argsEq = and $ map (\ a -> case a of
                                                                               Pass _ -> True
                                                                               _ -> False) (zipWith (matchTypes e) as bs)
matchTypes e a b = Mismatch (a, b)

-- Deep-replace a type with another type (used for replacing polymorphic types)
swapType :: [Type] -> Type -> Type-> [Type]
swapType ts t n = swapType' ts [] t n
                  where
                      swapType' [] acc _ _ = acc
                      swapType' (t@(Type (a, ts')):ts) acc f r = swapType' ts (acc ++ [Type (a, swapType ts' f r)]) f r
                      swapType' (t@(Name _):ts) acc f r | t == f = swapType' ts (acc ++ [r]) f r
                                                            | otherwise = swapType' ts (acc ++ [t]) f r

-- Ensure that all AtomoVals match a specified type
allType :: CheckEnv -> Type -> [Type] -> TypeCheck
allType e t [] = Pass e
allType e t (x:xs) = case checkType e x t of
                          Pass e -> allType e t xs
                          _ -> Mismatch (t, x)

verifyList :: CheckEnv -> [AtomoVal] -> TypeCheck
verifyList e [] = Pass e
verifyList e (x:xs) = allType e (getType e x) (map (getType e) xs)

verifyTuple :: CheckEnv -> [(Type, AtomoVal)] -> TypeCheck
verifyTuple e [] = Pass e
verifyTuple e ((t, v):vs) = case checkType e (getType e v) t of
                                 Pass e -> verifyTuple e vs
                                 _ -> Mismatch (t, getType e v)

verifyHash :: CheckEnv -> [(String, (Type, AtomoVal))] -> TypeCheck
verifyHash e [] = Pass e
verifyHash e ((_, (t, v)):vs) = case checkType e (getType e v) t of
                                     Pass e ->  verifyHash e vs
                                     _ -> Mismatch (t, getType e v)

checkAST :: ThrowsError [AtomoVal] -> ThrowsError [AtomoVal]
checkAST (Right a) = checkExprs ([], []) a a
checkAST (Left err) = throwError err

checkExprs _ [] t = return t
checkExprs e (a:as) t = case checkExpr e a of
                             Pass e -> checkExprs e as t
                             Mismatch (e, f) -> throwError $ TypeMismatch e f

checkExpr :: CheckEnv -> AtomoVal -> TypeCheck
checkExpr e (AList as) = verifyList e as >>* Pass e
checkExpr e (ATuple as) = verifyTuple e as >>* Pass e
checkExpr e (AHash as) = verifyHash e as >>* Pass e
checkExpr e (ADefine t _ v) = checkExpr e v >>* checkType e (getType e v) t >>* Pass e
checkExpr e (AData _ as cs) = Pass newEnv
                              where
                                  newGlobal = map (\c -> (fromAConstruct c, getType e c)) cs ++ fst e
                                  newEnv = (newGlobal, snd e)
checkExpr e (ACall (AIOFunc "print") as) = allType e (Name "string") (map (getType e) as) >>* Pass e
checkExpr e (ACall (AIOFunc "dump") as) = checkAll e as
checkExpr e (ACall (AVariable n) as) = case getAnyType e n of
                                            Just (Type (f, ts)) -> checkArgs e ts (map (getType e) as)
                                            Nothing -> Pass e
checkExpr e (AIf c t f) = checkType e (getType e c) (Name "true") >>* checkExpr e t >>* checkExpr e f
checkExpr e (ABlock as) = checkAll e as
checkExpr e (AFunc t n ps b) = checkExpr e b -- TODO
{- checkExpr e a = error (show a) -}
checkExpr e _ = Pass e

checkAll :: CheckEnv -> [AtomoVal] -> TypeCheck
checkAll e [] = Pass e
checkAll e (a:as) = case checkExpr e a of
                         Pass e -> checkAll e as
                         Mismatch (e, f) -> Mismatch (e, f)

-- Check the arguments against the types the function defines,
-- and return any polymorphic types along with their replacement.
checkArgs :: CheckEnv -> [Type] -> [Type] -> TypeCheck
checkArgs e ts' as' | length ts' /= length as' = Mismatch (Name "THIS IS REALLY A NUM ARGS ERROR", Name "TODO :-D")
                    | otherwise = checkArgs' e ts' as'
                      where
                          checkArgs' e [] [] = Pass e
                          checkArgs' e (t:ts) (a:as) = checkType e a t >>* checkArgs' e (swapPoly ts t a) as --((t, getType a) : rs)
                          swapPoly ts (Name [_]) _ = ts
                          swapPoly ts p n = swapType ts p n

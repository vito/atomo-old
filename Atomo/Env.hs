module Atomo.Env where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive (primFuncs)

import Control.Monad.Error
import Data.IORef
import Data.Maybe (fromJust)
import Debug.Trace

type Scope = IORef [(Index, IORef AtomoVal)]
type Env = [Scope]

nullScope :: IO Scope
nullScope = newIORef []

nullEnv :: IO Env
nullEnv = do prims <- mapM (\(n, (a, _)) -> do val <- newIORef (lambdify (map PName a) (ABlock [APrimCall n (map AVariable a)]))
                                               return (Define n, val)) primFuncs
             global <- newIORef prims
             return [global]

mutateVal :: Env -> Index -> AtomoVal -> IOThrowsError AtomoVal
mutateVal e n v = do ref <- getRef e n
                     liftIO $ writeIORef ref v
                     return v

defineVal :: Env -> Index -> AtomoVal -> IOThrowsError AtomoVal
defineVal e n v = do val <- liftIO $ newIORef v
                     env <- liftIO $ readIORef (head e)
                     liftIO $ writeIORef (head e) ((n, val) : env)
                     return v

getVal :: Env -> String -> IOThrowsError AtomoVal
getVal e n = do m <- maybeRef e (Define n)
                case m of
                     Just v -> (liftIO . readIORef) v
                     Nothing -> do c <- maybeRef e (Class (Name n))
                                   case c of
                                        Just v -> (liftIO . readIORef) v
                                        Nothing -> error $ "Could not find definition `" ++ n ++ "'"

getDef :: Env -> Index -> IOThrowsError AtomoVal
getDef e i = getRef e i >>= liftIO . readIORef

getRef :: Env -> Index -> IOThrowsError (IORef AtomoVal)
getRef e i = do m <- maybeRef e i
                case m of
                     Nothing -> case i of
                                     (Define n) -> error $ "Could not find definition `" ++ n ++ "'"
                                     (Class t) -> error $ "Could not find class `" ++ prettyType t ++ "'"
                                     (Process t) -> error $ "Could not find process `" ++ show t ++ "'"
                                     (Typeclass n) -> error $ "Could not find typeclass `" ++ n ++ "'"
                                     (Instance n t) -> error $ "Typeclass `" ++ n ++ "' does not have instance for `" ++ t ++ "'"
                     Just v -> return v

maybeRef :: Env -> Index -> IOThrowsError (Maybe (IORef AtomoVal))
maybeRef [] _ = return Nothing
maybeRef (s:ss) n = do env <- liftIO $ readIORef s
                       case lookup n env of
                            Just v -> return $ Just v
                            Nothing -> maybeRef ss n

getClasses :: Env -> Type -> IOThrowsError [(Index, IORef AtomoVal)]
getClasses [] _ = return []
getClasses (s:ss) t = do env <- liftIO $ readIORef s
                         rest <- getClasses ss t
                         return (findClasses env t [] ++ rest)
                 where
                    findClasses [] _ acc = acc
                    findClasses ((Class f, v):es) t acc | match t f = findClasses es t ((Class f, v) : acc)
                                                        | otherwise = findClasses es t acc
                    findClasses (_:es) t acc = findClasses es t acc

pMatch :: Env -> PatternMatch -> AtomoVal -> IOThrowsError Bool
pMatch e PAny _ = return True
pMatch e (PMatch a) b = return (a == b)
pMatch e (PName n) v = defineVal e (Define n) v >> return True
pMatch e (PNamed n p) v = do m <- pMatch e p v
                             if m
                                then defineVal e (Define n) v >> return True
                                else return False
pMatch e (PCons a as) (AValue b bs _) | a == b = matchAll e as bs
                                      | otherwise = return False
pMatch e (PHeadTail h t) (AList (x:xs)) = do head <- pMatch e h x
                                             if head
                                                then pMatch e t (AList xs)
                                                else return False
pMatch e (PList as) (AList bs) = matchAll e as bs
pMatch e (PTuple as) (ATuple bs) = matchAll e as bs
pMatch e _ _ = return False

pMatches :: PatternMatch -> AtomoVal -> Bool
pMatches PAny _ = True
pMatches (PMatch a) b = a == b
pMatches (PName _) _ = True
pMatches (PNamed _ p) v = pMatches p v
pMatches (PCons a as) (AValue b bs _) = a == b && matchesAll as bs
pMatches (PHeadTail _ _) (AList (_:_)) = True
pMatches (PList as) (AList bs) = matchesAll as bs
pMatches (PTuple as) (ATuple bs) = matchesAll as bs
pMatches _ _ = False

matchesAll :: [PatternMatch] -> [AtomoVal] -> Bool
matchesAll [] [] = True
matchesAll [] _  = False
matchesAll _  [] = False
matchesAll (a:as) (b:bs) | pMatches a b = matchesAll as bs
                         | otherwise = False

matchAll :: Env -> [PatternMatch] -> [AtomoVal] -> IOThrowsError Bool
matchAll e [] [] = return True
matchAll e [] _  = return False
matchAll e _  [] = return False
matchAll e (a:as) (b:bs) = do match <- pMatch e a b
                              if match
                                 then matchAll e as bs
                                 else return False

matchExec :: Env -> [AtomoVal] -> AtomoVal -> IOThrowsError AtomoVal
matchExec e [] _ = error "Non-exhaustive pattern match."
matchExec e ((APattern p c):ps) v = do m <- pMatch e p v
                                       if m
                                          then return c
                                          else matchExec e ps v

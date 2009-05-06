module Atomo.Env where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive (primFuncs, ioPrims)

import Control.Monad.Error
import Data.IORef
import Data.Maybe (fromJust)

type Scope = IORef [(String, IORef AtomoVal)]
type Env = [Scope]

nullScope :: IO Scope
nullScope = newIORef []

nullEnv :: IO Env
nullEnv = do prims <- mapM (\(n, (a, f)) -> do val <- newIORef (lambdify a (ABlock [APrimCall n (map AVariable a)]))
                                               return (n, val)) primFuncs
             io <- mapM (\(n, (a, f)) -> do val <- newIORef a
                                            return (n, val)) ioPrims
             global <- newIORef (prims ++ io)
             return [global]

mutateVal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
mutateVal e n v = do ref <- liftIO $ getRef e n
                     case ref of
                          Just r -> liftIO $ writeIORef r v
                          Nothing -> error $ "Attempt to mutate undefined value `" ++ n ++ "'"

                     return v

defineVal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
defineVal e n v = do val <- liftIO $ newIORef v
                     env <- liftIO $ readIORef (head e)

                     case lookup n env of
                          Nothing -> do liftIO $ writeIORef (head e) ((n, val) : env)
                                        return v
                          Just r -> do val <- liftIO $ readIORef r
                                       error $ "Multiple definitions of `" ++ n ++ "':\n\t" ++ pretty val ++ "\n\n\t" ++ pretty v

getVal :: Env -> String -> IOThrowsError AtomoVal
getVal [] n = error $ "Could not find variable `" ++ n ++ "'"
getVal (s:ss) n = do env <- liftIO $ readIORef s
                     case lookup n env of
                          Just v -> (liftIO . readIORef) v >>= return
                          Nothing -> getVal ss n

getRef :: Env -> String -> IO (Maybe (IORef AtomoVal))
getRef [] n = return Nothing
getRef (s:ss) n = do env <- readIORef s

                     case lookup n env of
                          Just v -> return $ Just v
                          Nothing -> getRef ss n

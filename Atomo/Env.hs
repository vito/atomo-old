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
nullEnv = do prims <- mapM (\(n, (a, _)) -> do val <- newIORef (lambdify a (ABlock [APrimCall n (map AVariable a)]))
                                               return (n, val)) primFuncs
             io <- mapM (\(n, (a, f)) -> do val <- newIORef a
                                            return (n, val)) ioPrims
             global <- newIORef (prims ++ io)
             return [global]

mutateVal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
mutateVal e n v = do ref <- getRef e n
                     liftIO $ writeIORef ref v
                     return v

defineVal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
defineVal e n v = do val <- liftIO $ newIORef v
                     env <- liftIO $ readIORef (head e)
                     liftIO $ writeIORef (head e) ((n, val) : env)
                     return v

getVal :: Env -> String -> IOThrowsError AtomoVal
getVal e n = getRef e n >>= liftIO . readIORef

getRef :: Env -> String -> IOThrowsError (IORef AtomoVal)
getRef [] n = error $ "Could not find variable `" ++ n ++ "'"
getRef (s:ss) n = do env <- liftIO $ readIORef s

                     case lookup n env of
                          Just v -> return v
                          Nothing -> getRef ss n

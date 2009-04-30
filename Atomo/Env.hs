module Atomo.Env where

import Atomo.Error
import Atomo.Internals
import Atomo.Primitive (primFuncs, ioPrims)

import Control.Monad.Error
import Data.IORef
import Data.Maybe (fromJust)

type Scope = IORef [(String, IORef AtomoVal)]
type Env = (Scope, Scope) -- (Global, Local)

nullScope :: IO Scope
nullScope = newIORef []

nullEnv :: IO Env
nullEnv = do prims <- mapM (\(n, (a, f)) -> do val <- newIORef a
                                               return (n, val)) primFuncs
             io <- mapM (\(n, (a, f)) -> do val <- newIORef a
                                            return (n, val)) ioPrims
             global <- newIORef (prims ++ io)
             local <- nullScope
             return (global, local)

globalScope :: Env -> Scope
globalScope = fst

localScope :: Env -> Scope
localScope = snd

isBound :: Scope -> String -> IO Bool
isBound e s = readIORef e >>= return . maybe False (const True) . lookup s

setVal :: Scope -> String -> AtomoVal -> IOThrowsError AtomoVal
setVal e s v = do env <- liftIO $ readIORef e

                  case lookup s env of
                       Just ref -> liftIO $ writeIORef ref v
                       Nothing -> error "Attempt to set undefined value."

                  return v

defineVal :: Scope -> String -> AtomoVal -> IOThrowsError AtomoVal
defineVal e s v = do defined <- liftIO $ isBound e s
                     if defined
                        then setVal e s v >> return v
                        else liftIO $ do val <- newIORef v
                                         env <- readIORef e
                                         writeIORef e ((s, val) : env)
                                         return v

getVal :: Scope -> String -> IOThrowsError AtomoVal
getVal e s = do env <- liftIO $ readIORef e
                case lookup s env of
                     Just v -> (liftIO . readIORef) v >>= return
                     Nothing -> error $ "Could not find variable `" ++ s ++ "'"

setLocal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
setLocal e s v = defineVal (localScope e) s v

setGlobal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
setGlobal e s v = defineVal (globalScope e) s v

getLocal :: Env -> String -> IOThrowsError AtomoVal
getLocal e s = getVal (localScope e) s

getGlobal :: Env -> String -> IOThrowsError AtomoVal
getGlobal e s = getVal (globalScope e) s

getAny :: Env -> String -> IOThrowsError AtomoVal
getAny e s = do local <- liftIO . readIORef $ localScope e
                case lookup s local of
                  Just v -> (liftIO . readIORef) v >>= return
                  Nothing -> getGlobal e s

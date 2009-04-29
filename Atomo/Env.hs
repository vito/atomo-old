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

isImmutable :: Scope -> String -> IO Bool
isImmutable e s = do env <- readIORef e
                     let av = fromJust $ lookup s env
                     val <- readIORef av
                     case val of
                          (AConstruct _ _ _) -> return True
                          _ -> return False

setVal :: Scope -> String -> AtomoVal -> IOThrowsError AtomoVal
setVal e s v = do env <- liftIO $ readIORef e
                  immutable <- liftIO $ isImmutable e s
                  if immutable
                     then throwError $ ImmutableVar s
                     else case lookup s env of
                               Just ref -> liftIO $ writeIORef ref v
                               Nothing -> throwError $ UnboundVar s
                  return v

defineVal :: Scope -> String -> AtomoVal -> IOThrowsError AtomoVal
defineVal e s v = do defined <- liftIO $ isBound e s
                     if defined
                        then setVal e s v >> return v
                        else liftIO $ do val <- newIORef v
                                         env <- readIORef e
                                         writeIORef e ((s, val) : env)
                                         return v

getVal :: Scope -> String -> IOThrowsError AtomoVal -> IOThrowsError AtomoVal
getVal e s f = do env <- liftIO $ readIORef e
                  maybe f (liftIO . readIORef) (lookup s env)

setLocal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
setLocal e s v = defineVal (localScope e) s v

setGlobal :: Env -> String -> AtomoVal -> IOThrowsError AtomoVal
setGlobal e s v = defineVal (globalScope e) s v

getLocal :: Env -> String -> IOThrowsError AtomoVal
getLocal e s = getVal (localScope e) s (throwError $ UnboundVar s)

getGlobal :: Env -> String -> IOThrowsError AtomoVal
getGlobal e s = getVal (globalScope e) s (throwError $ UnboundVar s)

getAny :: Env -> String -> IOThrowsError AtomoVal
getAny e s = getVal (globalScope e) s (tryLocal)
             where
                 tryLocal = getVal (localScope e) s (error)
                 error = throwError $ UnboundVar s

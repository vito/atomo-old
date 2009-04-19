module Main where

import Atomo.Compiler
import Atomo.Env
import Atomo.Error
import Atomo.Internals
import Atomo.Parser

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import System
import System.Console.Haskeline

-- Primitive if-else
primIf :: Env -> AtomoVal -> AtomoVal -> AtomoVal -> IOThrowsError AtomoVal
primIf e (AConstruct "true" _) b _ = eval e b
primIf e _ _ f                     = eval e f -- Condition is false, evaluate "else" if any

patternMatch :: Scope -> [String] -> [AtomoVal] -> IOThrowsError ()
patternMatch s ps as = return ()

apply :: Env -> AtomoVal -> [AtomoVal] -> IOThrowsError AtomoVal
apply e (APrimFunc _ f) as = liftThrows $ f as
apply e (AIOFunc _ f) as   = f as
apply e (AFunc t _ ps b) as = do new <- liftIO $ nullScope
                                 patternMatch new (map snd ps) as
                                 let env = (globalScope e, new)
                                 setLocals env ps as
                                 res <- eval env b
                                 if getType res /= t
                                    then throwError $ TypeMismatch t (getType res)
                                    else return res
                              where setLocals _ [] [] = return ()
                                    setLocals _ (x:_) [] = throwError $ NumArgs (length ps) (length as)
                                    setLocals _ [] (a:_) = throwError $ NumArgs (length ps) (length as)
                                    setLocals e (x:xs) (a:as) | getType a /= (fst x) = throwError $ TypeMismatch (fst x) (getType a)
                                                              | otherwise = setLocal e (snd x) a >> setLocals e xs as

eval :: Env -> AtomoVal -> IOThrowsError AtomoVal
eval e val@(AInt _)          = return val
eval e val@(AChar _)         = return val
eval e val@(ADouble _)       = return val
eval e val@(APrimFunc _ _)   = return val
eval e val@(AIOFunc _ _)     = return val
eval e val@(AString _)       = return val
eval e val@(AConstruct c d)  = return val
eval e val@(AFunc _ n _ _)   = setGlobal e n val
eval e (ATuple vs)           = do tuple <- mapM (\(t, v) -> do val <- eval e v
                                                               return (t, val)) vs
                                  case verifyTuple tuple of
                                       Nothing -> return $ ATuple tuple
                                       Just (expect, found) -> throwError $ TypeMismatch expect found
eval e (AHash vs)            = do hash <- mapM (\(n, (t, v)) -> do val <- eval e v
                                                                   return (n, (t, val))) vs
                                  case verifyHash hash of
                                       Nothing -> return $ AHash hash
                                       Just (expect, found) -> throwError $ TypeMismatch expect found
eval e (AList as)            = do list <- mapM (eval e) as
                                  case verifyList list of
                                       Nothing -> return $ AList list
                                       Just (expect, found) -> throwError $ TypeMismatch expect found
eval e (AVariable s)         = getAny e s
eval e (ADefine t s v)       = do val <- eval e v
                                  if getType val == t
                                     then setLocal e s val
                                     else throwError $ TypeMismatch t (getType val)
eval e (AAssign s v)         = do val <- eval e v
                                  orig <- getAny e s
                                  if getType val == getType orig
                                     then setLocal e s val
                                     else throwError $ TypeMismatch (getType orig) (getType val)
eval e (ACall f as)          = do fun <- eval e f
                                  args <- mapM (eval e) as
                                  apply e fun args
eval e (ABlock es)           = evalAll e es
eval e (AData s cs)          = mapM_ (\c -> setGlobal e (fromAConstruct c) c) cs >> return ANone
eval e (AIf c b f)           = do cond <- eval e c
                                  primIf e cond b f
eval e v                     = throwError $ Default $ "Can't evaluate: " ++ show v

evalAll :: Env -> [AtomoVal] -> IOThrowsError AtomoVal
evalAll e es = evalAll' e es ANone
               where
                   evalAll' :: Env -> [AtomoVal] -> AtomoVal -> IOThrowsError AtomoVal
                   evalAll' e [] r     = return $ r
                   evalAll' e (x:xs) _ = do res <- eval e x
                                            evalAll' e xs res

evalAndPrint :: Env -> String -> IO ()
evalAndPrint e s = evalString e s >>= putStrLn

evalString :: Env -> String -> IO String
evalString e s = runIOThrows $ liftM show $ (liftThrows $ readExpr s) >>= eval e

-- Execute a string
execute :: Env -> String -> IO ()
execute e s = do let parsed = readExprs s
                 case parsed of -- Catch parse errors
                      Left err -> print err
                      Right v -> do res <- runErrorT (evalAll e (extractValue parsed))
                                    case res of
                                         Left err -> print err -- Runtime error
                                         Right v -> return ()


-- The Read-Evaluate-Print Loop
repl :: Env -> InputT IO ()
repl e = do input <- getInputLine "> "
            case input of
                 Nothing -> return ()
                 Just "exit()" -> return ()
                 Just input -> do result <- liftIO $ evalString e input
                                  outputStrLn result
                                  repl e
                                  return ()

main = do args <- getArgs
          env <- nullEnv
          case length args of
               0 -> do putStrLn "Atomo 0.0"
                       runInputT defaultSettings (repl env)
               1 -> do source <- readFile (args !! 0)
                       execute env source
               2 -> do source <- readFile (args !! 1)
                       compile source
               otherwise -> putStrLn "`atomo` only takes 1 or 0 arguments."

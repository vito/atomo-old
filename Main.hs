module Main where

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
apply e (APrimFunc f) as = liftThrows $ f as
apply e (AIOFunc f) as   = f as
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
eval e val@(APrimFunc _)     = return val
eval e val@(AIOFunc _)       = return val
eval e val@(AFunc _ n _ _)   = setGlobal e n val
eval e val@(AList as)        = do ev <- mapM (eval e) as
                                  return $ AList ev
eval e val@(ATuple _)        = return val
eval e val@(AHash _)         = return val
eval e val@(AString _)       = return val
eval e val@(AConstruct c d)  = return val
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
execute e s = (runIOThrows $ (liftM show $ (liftThrows $ readExprs s) >>= evalAll e)) >>= putStrLn


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
               1 -> do let file = args !! 0
                       source <- readFile file
                       execute env source
                       return ()
               otherwise -> putStrLn "`atomo` only takes 1 or 0 arguments."

module Main where

import Atomo.Compiler
import Atomo.Env
import Atomo.Error
import Atomo.Internals
import Atomo.Parser
import Atomo.Typecheck

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import System
import System.Console.Haskeline

-- Primitive if-else
primIf :: Env -> AtomoVal -> AtomoVal -> AtomoVal -> IOThrowsError AtomoVal
primIf e (AValue "true" _ _)  a _ = eval e a
primIf e (AValue "false" _ _) _ b = eval e b

patternMatch :: Scope -> [String] -> [AtomoVal] -> IOThrowsError ()
patternMatch s ps as = return ()

-- Function/Constructor application
apply :: Env -> AtomoVal -> [AtomoVal] -> IOThrowsError AtomoVal
apply e (APrimFunc _ n _) as = liftThrows $ (getPrim n) as
apply e (AIOFunc _ n _) as   = (getIOPrim n) as
apply e (AFunc t _ ps b) as  = do new <- liftIO $ nullScope
                                  patternMatch new (map snd ps) as

                                  -- Set local variables for arguments
                                  let env = (globalScope e, new)
                                  zipWithM_ (setLocal env) (map snd ps) as

                                  -- Evaluate the function
                                  res <- eval env b
                                  returned <- (case res of
                                                    AReturn r -> eval env r
                                                    a -> return a)

                                  return returned
apply e (AConstruct n _ d) as = return $ AValue n as d

eval :: Env -> AtomoVal -> IOThrowsError AtomoVal
eval e v@(AInt _)           = return v
eval e v@(AChar _)          = return v
eval e v@(ADouble _)        = return v
eval e v@(AValue _ _ _)     = return v
eval e v@(APrimFunc _ _ _)  = return v
eval e v@(AIOFunc _ _ _)    = return v
eval e v@(AString _)        = return v
eval e v@(AConstruct _ _ _) = return v
eval e v@(AReturn _)        = return v
eval e v@(AFunc _ n _ _)    = setGlobal e n v
eval e (ATuple vs)     = do tuple <- mapM (\(t, v) -> do val <- eval e v
                                                         return (t, val)) vs
                            return $ ATuple tuple
eval e (AHash vs)      = do hash <- mapM (\(n, (t, v)) -> do val <- eval e v
                                                             return (n, (t, val))) vs
                            return $ AHash hash
eval e (AList as)      = do list <- mapM (eval e) as
                            return $ AList list
eval e (AVariable s)   = getAny e s
eval e (ADefine t s v) = do val <- eval e v
                            setLocal e s val
eval e (AAssign s v)   = do val <- eval e v
                            orig <- getAny e s
                            setLocal e s val
eval e (ACall f as)    = do fun <- eval e f
                            args <- mapM (eval e) as
                            apply e fun args
eval e (ABlock es)     = evalAll e es
eval e (AData s _ cs)  = mapM_ (\c -> setGlobal e (fromAConstruct c) c) cs >> return ANone
eval e (AIf c b f)     = do cond <- eval e c
                            primIf e cond b f
eval e v               = throwError $ Default $ "Can't evaluate: " ++ show v

evalAll :: Env -> [AtomoVal] -> IOThrowsError AtomoVal
evalAll e es = evalAll' e es ANone
               where
                   evalAll' :: Env -> [AtomoVal] -> AtomoVal -> IOThrowsError AtomoVal
                   evalAll' e [] r     = return r
                   evalAll' e (x:xs) _ = do res <- eval e x
                                            case res of
                                                 r@(AReturn _) -> return r
                                                 otherwise -> evalAll' e xs res

evalString :: Env -> String -> IO String
evalString e s = runIOThrows $ liftM pretty $ (liftThrows $ readExpr s) >>= eval e

evalAndPrint :: Env -> String -> IO ()
evalAndPrint e s = evalString e s >>= putStrLn

-- Execute a string
execute :: Env -> String -> IO ()
execute e s = do let parsed = checkAST $ readExprs s
                 case parsed of -- Catch parse errors
                      Left err -> print err
                      Right v -> do res <- runErrorT (evalAll e (extractValue parsed))
                                    case res of
                                         Left err -> print err -- Runtime error
                                         Right v -> return ()

-- Dump an abstract syntax tree
dumpAST :: String -> IO ()
dumpAST s = dumpPretty ugly 0
            where
                dumpPretty [] _ = putStrLn ""
                dumpPretty (c:cs) i | c `elem` "[(" = do putChar c
                                                         putStrLn ""
                                                         putStr (replicate ((i + 1) * 4) ' ')
                                                         dumpPretty cs (i + 1)
                                    | c `elem` "])" = do putStrLn ""
                                                         putStr (replicate ((i - 1) * 4) ' ')
                                                         putChar c
                                                         dumpPretty cs (i - 1)
                                    | c == ',' = do putStrLn [c]
                                                    putStr (replicate (i * 4) ' ')
                                                    dumpPretty cs i
                                    | otherwise = do putChar c
                                                     dumpPretty cs i
                ugly = show (extractValue (readExprs s))

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
               3 -> do source <- readFile (args !! 0)
                       dumpAST source
               otherwise -> putStrLn "`atomo` only takes 1 or 0 arguments."

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
primIf e (AConstruct "true" _ _)  a _ = eval e a
primIf e (AConstruct "false" _ _) _ b = eval e b
primIf e a _ _                        = throwError $ TypeMismatch (Name "bool") (getType a)

patternMatch :: Scope -> [String] -> [AtomoVal] -> IOThrowsError ()
patternMatch s ps as = return ()

-- Function/Constructor application
apply :: Env -> AtomoVal -> [AtomoVal] -> IOThrowsError AtomoVal
apply e (APrimFunc n) as = liftThrows $ (getPrim n) as
apply e (AIOFunc n) as   = (getIOPrim n) as
apply e (AFunc t _ ps b) as = do new <- liftIO $ nullScope
                                 patternMatch new (map snd ps) as
                                 let env = (globalScope e, new)
                                 setLocals env ps as
                                 res <- eval env b
                                 returned <- (case res of
                                                   AReturn r -> eval env r
                                                   a -> return a)
                                 if checkType returned t
                                    then return returned
                                    else throwError $ TypeMismatch t (getType returned)
                              where
                                  setLocals _ [] [] = return ()
                                  setLocals _ _ []  = throwError $ NumArgs (length ps) (length as)
                                  setLocals _ [] _  = throwError $ NumArgs (length ps) (length as)
                                  setLocals e (x:xs) (a:as) | checkType a (fst x) = do setLocal e (snd x) a
                                                                                       setLocals e xs' as
                                                            | otherwise = throwError $ TypeMismatch (fst x) (getType a)
                                                            where
                                                                xs' = zip replacedTypes argNames
                                                                replacedTypes = swapType (map fst xs) (fst x) (getType a)
                                                                argNames = map snd xs
apply e (AConstruct n [] d@(AData _ _ ps)) as = do case lookup n ps of
                                                      Just ts -> do checkArgs ts as
                                                                    return $ AConstruct n as d
                                                      Nothing -> throwError $ Default "Constructor/Data mismatch."
                                                where
                                                    checkArgs [] [] = return ()
                                                    checkArgs _ [] = throwError $ NumArgs (length ps) (length as)
                                                    checkArgs [] _ = throwError $ NumArgs (length ps) (length as)
                                                    checkArgs (p:ps) (a:as) | checkType a p = checkArgs (swapType ps p (getType a)) as
                                                                            | otherwise = throwError $ TypeMismatch p (getType a)

eval :: Env -> AtomoVal -> IOThrowsError AtomoVal
eval e v@(AInt _)           = return v
eval e v@(AChar _)          = return v
eval e v@(ADouble _)        = return v
eval e v@(APrimFunc _)      = return v
eval e v@(AIOFunc _)        = return v
eval e v@(AString _)        = return v
eval e v@(AConstruct _ _ _) = return v
eval e v@(AReturn _)        = return v
eval e v@(AFunc _ n _ _)    = setGlobal e n v
eval e (ATuple vs)     = do tuple <- mapM (\(t, v) -> do val <- eval e v
                                                         return (t, val)) vs
                            case verifyTuple tuple of
                                 Nothing -> return $ ATuple tuple
                                 Just (expect, found) -> throwError $ TypeMismatch expect found
eval e (AHash vs)      = do hash <- mapM (\(n, (t, v)) -> do val <- eval e v
                                                             return (n, (t, val))) vs
                            case verifyHash hash of
                                 Nothing -> return $ AHash hash
                                 Just (expect, found) -> throwError $ TypeMismatch expect found
eval e (AList as)      = do list <- mapM (eval e) as
                            case verifyList list of
                                 Nothing -> return $ AList list
                                 Just (expect, found) -> throwError $ TypeMismatch expect found
eval e (AVariable s)   = getAny e s
eval e (ADefine t s v) = do val <- eval e v
                            if checkType val t
                               then setLocal e s val
                               else throwError $ TypeMismatch t (getType val)
eval e (AAssign s v)   = do val <- eval e v
                            orig <- getAny e s
                            if checkType val (getType orig)
                               then setLocal e s val
                               else throwError $ TypeMismatch (getType orig) (getType val)
eval e (ACall f as)    = do fun <- eval e f
                            args <- mapM (eval e) as
                            apply e fun args
eval e (ABlock es)     = evalAll e es
eval e v@(AData s _ cs)  = mapM_ (\c -> setGlobal e (fst c) (AConstruct (fst c) [] v)) cs >> return ANone
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
execute e s = do let parsed = readExprs s
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

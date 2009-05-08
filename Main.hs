module Main where

import Atomo.Compiler
import Atomo.Env
import Atomo.Error
import Atomo.Internals
import Atomo.Parser
import Atomo.Primitive
import Atomo.Typecheck

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import System
import System.Console.Haskeline

-- Primitive if-else
primIf :: Env -> AtomoVal -> AtomoVal -> AtomoVal -> IOThrowsError AtomoVal
primIf e (AConstruct "true" _ _)  a _ = eval e a
primIf e (AConstruct "false" _ _) _ b = eval e b
primIf e (AValue "true" _ _)  a _ = eval e a
primIf e (AValue "false" _ _) _ b = eval e b
primIf e c a b = error (show c)

patternMatch :: Scope -> String -> AtomoVal -> IOThrowsError ()
patternMatch s ps as = return ()

-- Function/Constructor application
apply :: Env -> AtomoVal -> AtomoVal -> IOThrowsError AtomoVal
apply e (ALambda p c bs) a = case c of
                                  (ALambda p' c' bs') -> return $ ALambda p' c' $ ((p, a) : (bs ++ bs'))
                                  (AValue n as d) -> do env <- bind
                                                        args <- mapM (eval env) as
                                                        return $ AValue n args d
                                  (ABlock c) -> do env <- bind
                                                   
                                                   res <- eval env (ABlock c)
                                                   returned <- (case res of
                                                                     AReturn r -> eval e r
                                                                     a -> return a)

                                                   return returned
                             where
                                 bind = do new <- liftIO $ nullScope
                                           let env = new : e
                                           mapM_ (\(n, v) -> defineVal env n v) $ ((p, a) : bs)
                                           return env
                                              
apply e t ANone = eval e t
apply _ t a = error ("Cannot apply `" ++ pretty a ++ "' on `" ++ pretty t ++ "'")

eval :: Env -> AtomoVal -> IOThrowsError AtomoVal
eval e v@(AType n _)  = defineVal e n v
eval e (ATuple vs)    = do tuple <- mapM (eval e) vs
                           return $ ATuple tuple
eval e (AHash vs)     = do hash <- mapM (\(n, (t, v)) -> do val <- eval e v
                                                            return (n, (t, val))) vs
                           return $ AHash hash
eval e (AList as)     = do list <- mapM (eval e) as
                           return $ AList list
eval e (AVariable n)  = getVal e n >>= eval e
eval e (ADefine n v)  = defineVal e n v
eval e (AMutate n v)  = eval e v >>= mutateVal e n
eval e (ACall f a)    = do fun <- eval e f
                           arg <- eval e a
                           apply e fun arg
eval e (ABlock es)     = evalAll e es
eval e (AData s _ cs)  = mapM_ (\c -> defineVal e (fromAConstruct c) (cons c)) cs >> return ANone
                         where
                             cons (AConstruct n ts d) = lambdify as (AValue n (map AVariable as) d)
                                                        where as = map (\c -> [c]) $ take (length ts) ['a'..]
eval e (AIf c b f)     = do cond <- eval e c
                            primIf e cond b f
eval e (APrimCall n as) = do args <- mapM (eval e) as
                             (getPrim n) args
eval _ v = return v

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
execute e s = do let parsed = checkAST $ readScript s
                 case parsed of -- Catch parse errors
                      Left err -> print err
                      Right v -> do res <- runErrorT (evalAll e (extractValue parsed ++ [ACall (AVariable "main") ANone]))
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

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
import Data.List (intercalate)
import Debug.Trace
import System
import System.Console.Haskeline
import System.FilePath.Posix

type Source = String

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
apply e (AClass _ cs) a = case getAVal cs "new" of
                               Nothing -> return object
                               Just (ADefine _ v) -> do new <- apply e v object
                                                        apply e new a
                          where
                              object = AObject cs
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
eval e (ADefine n v)  = eval e v >>= defineVal e n
eval e (ADefAttr n@(AVariable o) a v) = do ev <- eval e n
                                           val <- eval e v
                                           case ev of
                                                (AObject object) -> mutateVal e o (AObject (setAVal object a val))
                                                _ -> throwError $ Unknown $ "Variable `" ++ o ++ "' does not refer to an object."
eval e (AMutate n v)  = eval e v >>= mutateVal e n
eval e (ACall t@(AAttribute o n) a) = do obj <- eval e o
                                         fun <- eval e t
                                         arg <- eval e a
                                         case fun of
                                              (ALambda _ _ _) -> do self'd <- apply e fun obj
                                                                    apply e self'd arg
                                              _ -> return fun
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
eval e (AImport "" ts) = modulize e ts
eval e (AImport n ["*"]) = do source <- liftIO $ readModule n
                              merge e source
                              return ANone
eval e (AImport n ts) = do source <- liftIO $ readModule n
                           include e ts source n
                           return ANone
eval e (AAttribute t n) = do target <- eval e t
                             case target of
                                  (AModule as) -> case getAVal as n of
                                                       Just v -> eval e v
                                                       Nothing -> throwError $ Unknown $ "Module does not have value `" ++ n ++ "'."
                                  (AObject cs) -> case getAVal cs n of
                                                       Just v -> eval e v
                                                       Nothing -> throwError $ Unknown $ "Object does not have attribute `" ++ n ++ "'."
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

merge :: Env -> Source -> IOThrowsError AtomoVal
merge e s = case parsed of
                 Left err -> throwError err
                 Right v -> do res <- liftIO $ runErrorT (evalAll e (extractValue parsed))
                               case res of
                                    Left err -> throwError err
                                    Right _ -> return ANone
            where
                parsed = checkAST $ readScript s

include :: Env -> [String] -> Source -> String -> IOThrowsError AtomoVal
include e ts s m = case tree of
                        Left err -> throwError err
                        Right as -> include' e ts as as
                   where
                       tree = checkAST $ readScript s
                       include' e (t:ts) [] _ = throwError $ Unknown $ "Module `" ++ m ++ "' does not export `" ++ t ++ "'"
                       include' e [] _ _ = return ANone
                       include' e (t:ts) (a:as) tree = case a of
                                                            (AData n _ _) -> check a n t
                                                            (AType n _ ) -> check a n t
                                                            (ADefine n _) -> check a n t
                                                            (AAnnot n _ ) -> check a n t
                                                            _ -> include' e ts as tree
                                                        where
                                                            check a n t = if n == t
                                                                             then do eval e a
                                                                                     include' e ts tree tree
                                                                             else include' e (t:ts) as tree

modulize :: Env -> [String] -> IOThrowsError AtomoVal
modulize e [] = return ANone
modulize e (t:ts) = do parsed <- tree
                       case parsed of
                            Left err -> throwError err
                            Right as -> do defineVal e (last . dots $ t) (AModule as)
                                           modulize e ts
                    where
                        tree = do source <- liftIO $ readModule t
                                  return $ checkAST $ readScript source

getAVal :: [AtomoVal] -> String -> Maybe AtomoVal
getAVal [] _ = Nothing
getAVal (a:as) t = case a of
                        (AData _ _ cs) -> case getAVal cs t of
                                               Just v -> Just v
                                               Nothing -> getAVal as t
                        (AConstruct n _ _) -> check a n t
                        (AType n _ ) -> check a n t
                        (ADefine n _) -> check a n t
                        (AAnnot n _ ) -> check a n t
                        _ -> getAVal as t
                   where
                       check a n t = if n == t
                                        then Just a
                                        else getAVal as t

setAVal :: [AtomoVal] -> String -> AtomoVal -> [AtomoVal]
setAVal as n v = setAVal' as n v []
                 where
                     setAVal' [] t v acc = (ADefine t v) : acc
                     setAVal' (a:as) t v acc = case a of
                                                    (ADefine n _) -> if n == t
                                                                        then acc ++ (ADefine n v) : as
                                                                        else setAVal' as t v (a : acc)
                                                    _ -> setAVal' as t v (a : acc)

-- Execute a string
execute :: Env -> Source -> IO ()
execute e s = case parsed of -- Catch parse errors
                   Left err -> print err
                   Right v -> do res <- runErrorT (evalAll e (extractValue parsed ++ [ACall (AVariable "main") ANone]))
                                 case res of
                                      Left err -> print err -- Runtime error
                                      Right _ -> return ()
              where
                  parsed = checkAST $ readScript s

-- Dump an abstract syntax tree
dumpAST :: Source -> IO ()
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

getPath :: IO String
getPath = do args <- liftIO $ getArgs
             return $ takeDirectory (head args) ++ "/"

readModule :: String -> IO Source
readModule s = do path <- getPath
                  readFile (path ++ pathify s ++ ".at")

pathify :: String -> String
pathify s = intercalate "/" (dots s)

dots :: String -> [String]
dots s = l : case s' of
                  [] -> []
                  (_:s'') -> dots s''
        where
            (l, s') = break (== '.') s

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

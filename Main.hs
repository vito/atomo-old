module Main where

import Atomo.Compiler
import Atomo.Env
import Atomo.Error
import Atomo.Internals
import Atomo.Parser
import Atomo.Primitive
import Atomo.Typecheck

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.List (intercalate, sortBy)
import Data.IORef
import Debug.Trace
import System
import System.Console.Haskeline
import System.FilePath.Posix
import qualified System.IO.UTF8 as U

type Source = String

-- Primitive if-else
primIf :: Env -> AtomoVal -> AtomoVal -> AtomoVal -> IOThrowsError AtomoVal
primIf e (AValue "True" _ _)  a _ = eval e a
primIf e (AValue "False" _ _) _ b = eval e b
primIf e c a b = error ("Value is not boolean: " ++ pretty c)

-- Function/Constructor application
apply :: Env -> AtomoVal -> AtomoVal -> IOThrowsError AtomoVal
apply e t ANone = eval e t
apply e (ATypeFunc n f) a = do (AInstance _ _ (ABlock fs)) <- getDef e (Instance n t)
                               case getAVal fs (Define f) of
                                    Just f -> apply e f a
                                    Nothing -> throwError $ Unknown $ "Instance does not declare function `" ++ f ++ "'"
                            where
                                t = getData a
apply e (AClass _ cs) a = case getAVal cs (Define "new") of
                               Nothing -> return object
                               Just v -> do new <- apply e v object
                                            apply e new a
                          where
                              object = AObject cs
apply e (ALambda p c bs) a = case c of
                                  (ALambda p' c' bs') -> return $ ALambda p' c' $ ((p, a) : (bs ++ bs'))
                                  (AValue n as c) -> do env <- bind
                                                        args <- mapM (eval env) as
                                                        return $ AValue n args c
                                  (ABlock c) -> do env <- bind
                                                   
                                                   res <- eval env (ABlock c)
                                                   returned <- (case res of
                                                                     AReturn r -> eval env r
                                                                     a -> return a)

                                                   return returned
                             where
                                 bind = do new <- liftIO $ nullScope
                                           let env = new : e
                                           mapM_ (\(p, v) -> pMatch env p v) $ ((p, a) : bs)
                                           return env
apply e (AFunction ls) a = case head ls of
                                ALambda _ _ _ -> notFinal
                                v -> do dump "Not a lambda" (take 100 (show v))
                                        dump "Lambdas" ls
                                        eval e v
                           where
                               notFinal = if null valid
                                             then error $ "Non-exhaustive pattern match in function."
                                             else case (head valid) of
                                                       ALambda _ (ALambda _ _ _) _ -> return $ AFunction bound
                                                       l -> apply e l a
                               valid = filter (\(ALambda p _ _) -> pMatches p a) ls
                               bound = map setBound valid
                               setBound (ALambda p (ALambda p' c' bs') bs) = ALambda p' c' ((p, a) : (bs ++ bs'))
apply _ t a = error ("Cannot apply `" ++ pretty a ++ "' on `" ++ pretty t ++ "'")

eval :: Env -> AtomoVal -> IOThrowsError AtomoVal
eval e v@(AType n _)  = defineVal e (Define n) v
eval e (ATuple vs)    = do tuple <- mapM (eval e) vs
                           return $ ATuple tuple
eval e (AHash vs)     = do hash <- mapM (\(n, (t, v)) -> do val <- eval e v
                                                            return (n, (t, val))) vs
                           return $ AHash hash
eval e (AList as)     = do list <- mapM (eval e) as
                           return $ AList list
eval e (AVariable "this") = do tid <- liftIO myThreadId
                               getDef e (Process tid)
eval e (AVariable n)  = getVal e n >>= eval e
eval e (ADefine n v) = case v of
                            ALambda _ _ _ -> defineVal e n v
                            ABlock _ -> defineVal e n v
                            AFunction _ -> defineVal e n v
                            _ -> eval e v >>= defineVal e n
eval e (ADefAttr n@(AVariable o) a v) = do ev <- eval e n
                                           val <- eval e v
                                           case ev of
                                                (AObject object) -> mutateVal e (Define o) (AObject (setAVal object (Define a) val))
                                                _ -> throwError $ Unknown $ "Variable `" ++ o ++ "' does not refer to an object."
eval e (ACall t@(AAttribute o n) a) = do obj <- eval e o
                                         fun <- eval e t
                                         arg <- eval e a

                                         case fun of
                                              (AFunction _) -> do self'd <- apply e fun obj
                                                                  apply e self'd arg
                                              _ -> return fun
eval e (ACall f a)    = do fun <- eval e f
                           arg <- eval e a
                           apply e fun arg
eval e (ABlock es)     = evalAll e es
eval e (AIf c b f)     = do cond <- eval e c
                            primIf e cond b f
eval e (APrimCall n as) = do args <- mapM (eval e) as
                             (getPrim n) args
eval e (AImport "" ts) = modulize e ts
eval e (AImport n ["*"]) = do source <- liftIO $ readModule n
                              merge e source
                              return ANone
eval e (AImport n ts) = do source <- liftIO $ readModule n
                           include e (map Define ts) source n
                           return ANone
eval e (AAttribute t n) = do target <- eval e t
                             case target of
                                  (AObject cs) -> case getAVal cs (Define n) of
                                                       Just v -> eval e v
                                                       Nothing -> throwError $ Unknown $ "Object does not have attribute `" ++ n ++ "'."
                                  (AModule as) -> case getAVal as (Define n) of
                                                       Just v -> eval e v
                                                       Nothing -> throwError $ Unknown $ "Module does not have value `" ++ n ++ "'."
                                  (AClass ss _) -> case getAVal ss (Define n) of
                                                        Just v -> eval e v
                                                        Nothing -> throwError $ Unknown $ "Class does not have static method `" ++ n ++ "'."
                                  _ -> do classes <- getClasses e (getType target)
                                          if null classes
                                             then error ("Could not find class `" ++ prettyType (getType target) ++ "'")
                                             else do
                                          clss <- mapM (\(i, v) -> liftIO (readIORef v)) (sortBy (\(Class a, _) (Class b, _) -> compare b a) classes)
                                          findMethod clss n
                          where
                              findMethod [] n = throwError $ Unknown $ "Class does not have attribute `" ++ n ++ "'."
                              findMethod ((AClass _ cs):clss) n = case getAVal cs (Define n) of
                                                                       Just v -> eval e v
                                                                       Nothing -> findMethod clss n
eval e (ASpawn c) = do pid <- liftIO $ forkIO (runIOThrows (eval e c) >> return ())
                       chan <- liftIO newChan
                       defineVal e (Process pid) (AProcess pid chan)
                       return (AProcess pid chan)
eval e (AReceive (ABlock ps)) = do id <- liftIO myThreadId
                                   (AProcess pid chan) <- getDef e (Process id)
                                   val <- liftIO (readChan chan)
                                   run <- matchExec e ps val
                                   eval e run
eval e v@(ATypeclass n t (ABlock vs)) = do mapM (\v -> setFunc v) vs
                                           defineVal e (Typeclass n) v
                                           return ANone
                                      where
                                          setFunc (AAnnot f _) = defineVal e (Define f) (ATypeFunc n f)
eval e v@(AInstance n t vs) = defineVal e (Instance n t) v
eval e v@(AFunction (l@(ABlock _):_)) = eval e l
eval _ v = return v

evalAll :: Env -> [AtomoVal] -> IOThrowsError AtomoVal
evalAll e es = evalAll' e es ANone
               where
                   evalAll' :: Env -> [AtomoVal] -> AtomoVal -> IOThrowsError AtomoVal
                   evalAll' e [] r     = return r
                   evalAll' e (x:xs) _ = do res <- eval e x
                                            case res of
                                                 r@(AReturn _) -> return r
                                                 _ -> evalAll' e xs res

evalString :: Env -> String -> IO AtomoVal
evalString e s = runIOThrows $ (liftThrows $ readExpr s) >>= eval e

evalAndPrint :: Env -> String -> IO ()
evalAndPrint e s = evalString e s >>= U.putStrLn . pretty

-- Merge source into the environemt (e.g. import from Foo: *)
merge :: Env -> Source -> IOThrowsError AtomoVal
merge e s = case parsed of
                 Left err -> throwError err
                 Right v -> do res <- liftIO $ runErrorT (evalAll e (extractValue parsed))
                               case res of
                                    Left err -> throwError err
                                    Right _ -> return ANone
            where
                parsed = checkAST $ readScript s

-- Include targets from a module (e.g. import from Foo: bar, baz)
include :: Env -> [Index] -> Source -> String -> IOThrowsError AtomoVal
include e ts s m = case tree of
                        Left err -> throwError err
                        Right as -> include' e ts as as
                   where
                       tree = checkAST $ readScript s
                       include' :: Env -> [Index] -> [AtomoVal] -> [AtomoVal] -> IOThrowsError AtomoVal
                       include' e (t:ts) [] _ = throwError $ Unknown $ "Module `" ++ m ++ "' does not export `" ++ show t ++ "'"
                       include' e [] _ _ = return ANone
                       include' e (t:ts) (a:as) tree = case a of
                                                            (AData n _) -> check a (Define n) t
                                                            (AType n _ ) -> check a (Define n) t
                                                            (ADefine n _) -> check a n t
                                                            _ -> include' e ts as tree
                                                        where
                                                            check :: AtomoVal -> Index -> Index -> IOThrowsError AtomoVal
                                                            check a n t = if n == t
                                                                             then do eval e a
                                                                                     include' e ts tree tree
                                                                             else include' e (t:ts) as tree

-- Import an unqualified module (e.g. import: Foo.Bar)
modulize :: Env -> [String] -> IOThrowsError AtomoVal
modulize e [] = return ANone
modulize e (t:ts) = do parsed <- tree
                       case parsed of
                            Left err -> throwError err
                            Right as -> do defineVal e (Define $ last . dots $ t) (AModule as)
                                           modulize e ts
                    where
                        tree = do source <- liftIO $ readModule t
                                  return $ checkAST $ readScript source

-- Look up a definition in a source tree
getAVal :: [AtomoVal] -> Index -> Maybe AtomoVal
getAVal [] _ = Nothing
getAVal (a:as) t = case a of
                        (ABlock cs) -> case getAVal cs t of
                                            Just v -> Just v
                                            Nothing -> getAVal as t
                        (AConstruct n _ _) -> check a (Define n) t
                        (AType n _ ) -> check a (Define n) t
                        (ADefine n v) -> check v n t
                        (AStatic n v) -> check v (Define n) t
                        (AAnnot _ n) -> check a (Class n) t
                        _ -> getAVal as t
                   where
                       check a n t = if n == t
                                        then Just a
                                        else getAVal as t

-- Define something in a source tree. If already defined, overwrite it.
setAVal :: [AtomoVal] -> Index -> AtomoVal -> [AtomoVal]
setAVal as n v = setAVal' as n v []
                 where
                     setAVal' [] t v acc = (ADefine t v) : acc
                     setAVal' (a:as) t v acc = case a of
                                                    (ADefine n _) -> if n == t
                                                                        then acc ++ (ADefine n v) : as
                                                                        else setAVal' as t v (a : acc)
                                                    _ -> setAVal' as t v (a : acc)

-- Modify the block of a function, traveling through the lambdas.
modifyFunc :: AtomoVal -> ([AtomoVal] -> [AtomoVal]) -> AtomoVal
modifyFunc (ALambda a (ABlock as) b) f = ALambda a (ABlock (f as)) b
modifyFunc (ALambda a v b) f = ALambda a (modifyFunc v f) b

-- Execute a string
execute :: Env -> Source -> IO ()
execute e s = case parsed of -- Catch parse errors
                   Left err -> U.putStrLn . prettyError $ err
                   Right v -> do res <- runErrorT (evalAll e (extractValue parsed))
                                 case res of
                                      Left err -> U.putStrLn . prettyError $ err -- Runtime error
                                      Right _ -> case getAVal v (Define "main") of
                                                      Just f -> do main <- runErrorT $ apply e f ANone
                                                                   case main of
                                                                        Left err -> U.putStrLn . prettyError $ err
                                                                        Right _ -> return ()
                                                      Nothing -> putStrLn "Function `main' is not defined."
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
                ugly = show (map snd (extractValue (readScript s)))

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
                 Just "exit" -> return ()
                 Just input -> do result <- liftIO $ evalString e input

                                  if result == ANone
                                     then return ()
                                     else outputStrLn (pretty result)

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
               _ -> putStrLn "`atomo` only takes 1 or 0 arguments."

          threadDelay 1000 -- Temporary tweak so that threads don't get killed.

module Atomo.Error where

import Atomo.Internals

import Control.Monad.Error

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows a = runErrorT (trapError a) >>= return . extractValue

trapError :: IOThrowsError String -> IOThrowsError String
trapError a = catchError a (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

module Atomo.Error where

import Atomo.Internals

import Control.Monad.Error

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError AtomoVal -> IO AtomoVal
runIOThrows a = runErrorT (trapError a) >>= return . extractValue

trapError :: IOThrowsError AtomoVal -> IOThrowsError AtomoVal
trapError a = a `catchError` (return . AError . prettyError)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

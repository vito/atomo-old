module Atomo.Compiler where

import Atomo.Error
import Atomo.Internals
import Atomo.Parser (readExpr)
import Atomo.Primitive (getIOPrim)

import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.Word
import LLVM.Core
import LLVM.ExecutionEngine

genPrint :: AtomoVal -> CodeGenModule (Function (IO ()))
genPrint (ACall (AIOFunc _ "print" _) [AString s]) = do
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    str <- createStringNul (fromAString s)

    createNamedFunction ExternalLinkage "main" $ do
        tmp <- getElementPtr str (0 :: Word32, (0 :: Word32, ()))
        call puts tmp
        ret ()
genPrint (ACall (AIOFunc _ "dump" _) [s]) = genPrint printCall
                                          where
                                              printCall = ACall printFunc targets
                                              printFunc = AIOFunc (Name "void") "print" [listOf (Name "char")]
                                              targets = [(toAString (pretty s))]

expr = "dump(50.0)"

compile :: String -> IO ()
compile s = do let str = extractValue $ readExpr expr
               m <- newModule
               fn <- defineModule m (genPrint str)
               iorun <- runEngineAccess $ do
                   addModule m
                   generateFunction fn
               iorun
               writeBitcodeToFile "atomo.bc" m
               dumpValue fn

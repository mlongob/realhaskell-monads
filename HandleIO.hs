{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO (
    HandleIO,
    Handle,
    IOMode(..),
    runHandleIO,
    openFile,
    hClose,
    hPutStrLn
) where

import System.IO (Handle, IOMode(..))
import qualified System.IO

newtype HandleIO a = HandleIO { runHandleIO :: IO a }
    deriving (Functor, Applicative, Monad)

openFile = undefined
hClose = undefined
hPutStrLn = undefined

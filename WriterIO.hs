{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances,
             MultiParamTypeClasses #-}
module WriterIO where

import MonadHandle
import System.IO(IOMode(..))
import Control.Monad.Writer

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
    deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle [Char] WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr handle str = tell [Put handle str]
    hClose handle = tell [Close handle]
    hGetContents handle = tell [GetContents handle] >> return "[CONTENTS]"


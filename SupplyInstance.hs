{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances,
             MultiParamTypeClasses #-}

module SupplyInstance where
import Control.Monad.Reader
import SupplyClass

newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ Just <$> ask

xy :: (Num s, MonadSupply s m) => m s
xy = do
    Just x <- next
    Just y <- next
    return (x * y)

runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply

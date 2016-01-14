{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supply
    (
        Supply,
        next,
        runSupply
    ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)
    deriving (Functor, Applicative, Monad)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

-- Not needed because generalized!
--
{-instance Functor (Supply s) where-}
    {-fmap = (<$>)-}
{-instance Applicative (Supply s) where-}
    {-(<*>) = ap-}
    {-pure = S . pure-}
{-instance Monad (Supply s) where-}
    {-s >>= m = S (unwrapS s >>= unwrapS . m)-}
    {-return = pure-}

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) = runState m

showTwo :: (Show s) => Supply s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)

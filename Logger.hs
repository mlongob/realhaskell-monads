-- Logger.hs
module Logger (
    Logger,
    Log,
    runLogger,
    record
) where

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

newtype Logger a = Logger { execLogger :: (a, Log) }

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \x -> return $ f x

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = do
    x <- m1
    y <- m2
    return $ f x y

ap :: (Monad m) => m (a -> b) -> m a -> m b
m `ap` n = m >>= \f -> liftM f n

instance Functor Logger where
    fmap = liftM

instance Applicative Logger where
    pure a = Logger(a, [])
    (<*>) = ap

instance Monad Logger where
    return = pure
    m >>= k = let (a, w) = execLogger m
                  n = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

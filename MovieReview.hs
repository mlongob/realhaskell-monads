module MovieReview where
import Control.Monad

data MovieReview = MovieReview {
    revTitle :: String,
    revUser :: String,
    revReview :: String
} deriving(Show)

-- I modified this to use the Applicative-style operators
-- <$> is the Functor operator to lift (fmap) MoveReview into
-- the Maybe monad. <*> is the Applicative operator to
-- apply the lifted function in the Maybe monad
apReview alist = MovieReview <$> lookup1 "title" alist
                             <*> lookup1 "user" alist
                             <*> lookup1 "review" alist

lookup1 key alist = case lookup key alist of
                        Just (Just s@(_:_)) -> Just s
                        _ -> Nothing

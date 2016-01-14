module Random where

import System.Random
import Control.Monad.State

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
    get >>= \gen ->
    let (val, gen') = random gen in
    put gen' >>
    return val

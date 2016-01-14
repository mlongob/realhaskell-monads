module RandomSupply where
import Supply
import System.Random hiding(next)
import Control.Arrow (first)

randomsIO :: Random a => IO [a]
randomsIO = getStdRandom $ \g ->
                let (a, b) = split g
                in (randoms a, b)

randomsIO' :: Random a => IO [a]
randomsIO' = getStdRandom (first randoms . split)

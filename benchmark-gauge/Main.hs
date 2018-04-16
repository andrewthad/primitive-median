import Common (naiveIntegralMedian)
import Gauge.Main
import System.Random (randoms,mkStdGen)
import qualified Data.Primitive.PrimArray.Median as PAM

elemCount :: Int
elemCount = 50000

sortedList :: [Word]
sortedList = enumFromTo 0 (elemCount - 1)

sortedArray :: PrimArray Word
sortedArray = fromList sortedList

randomList :: [Word]
randomList = randoms (mkStgGen 42)

randomArray :: PrimArray Word
randomArray = fromList randomList

main :: IO ()
main = defaultMain
  [ bgroup "sorted"
    [ bench "naive" $ whnf naiveIntegralMedian sortedList
    , bench "primitive" $ whnf PAM.word sortedArray
    ]
  , bgroup "random"
    [ bench "naive" $ whnf naiveIntegralMedian randomList
    , bench "primitive" $ whnf PAM.word randomArray
    ]
  ]

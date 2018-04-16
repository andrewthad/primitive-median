import Common (naiveIntegralMedian)
import Gauge.Main
import System.Random (randoms,mkStdGen)
import qualified Data.Primitive.PrimArray.Median as PAM

main :: IO ()
main = defaultMain
  [ bgroup "naive"
    [ bench "sorted" $ whnf naiveIntegralMedian sortedList
    , bench "random" $ whnf naiveIntegralMedian randomList
    ]
  , bgroup "primitive"
    [ bench "sorted" $ whnf PAM.word sortedArray
    , bench "random" $ whnf PAM.word randomArray
    , bgroup "sized"
      [ bench "1000" $ whnf PAM.word randomArray3
      , bench "10000" $ whnf PAM.word randomArray4
      , bench "100000" $ whnf PAM.word randomArray5
      , bench "1000000" $ whnf PAM.word randomArray6
      ]
    ]
  ]

elemCount :: Int
elemCount = 50000

sortedList :: [Word]
sortedList = enumFromTo 0 (elemCount - 1)

sortedArray :: PrimArray Word
sortedArray = fromList sortedList

randomList :: [Word]
randomList = take elemCount (randoms (mkStgGen 42))

randomArray :: PrimArray Word
randomArray = fromList randomList

randomArray3 :: PrimArray Word
randomArray3 = fromList (take 1000 (randoms (mkStdGen 75843)))

randomArray4 :: PrimArray Word
randomArray4 = fromList (take 10000 (randoms (mkStdGen 75843)))

randomArray5 :: PrimArray Word
randomArray5 = fromList (take 100000 (randoms (mkStdGen 75843)))

randomArray6 :: PrimArray Word
randomArray6 = fromList (take 1000000 (randoms (mkStdGen 75843)))

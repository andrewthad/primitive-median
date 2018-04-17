import Weigh
import Data.Primitive.PrimArray
import qualified Data.Primitive.PrimArray.Median as PAM

-- This measurement does some sketchy stuff, but it correctly measures
-- what we need it to. The function named io, from the weigh package,
-- will repeatedly call the function it's given for several seconds.
-- But, this means that we are reusing the mutable prim array the the
-- median function operates destructively on. In this situation, we
-- don't actually care. It doesn't matter that we are not taking the
-- median of something we know. We just want to make sure that we don't
-- perform any allocations as take the median.
main :: IO ()
main = do
  let sz = 500000
  mutArr <- newPrimArray sz
  copyPrimArray mutArr 0 (generatePrimArray sz id) 0 sz
  mainWith $ do
    io ("median of " ++ show sz ++ "-element array") PAM.int mutArr


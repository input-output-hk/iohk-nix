import           Cardano.Prelude

import           Criterion.Main
import           Skeleton

benchMessage :: Int -> Text
benchMessage n = stimes n message

main :: IO ()
main = defaultMain [
  bgroup "greeting" [ bench "1"  $ whnf benchMessage 1
                    , bench "5"  $ whnf benchMessage 5
                    , bench "9"  $ whnf benchMessage 9
                    , bench "11" $ whnf benchMessage 11
                    ]
  ]

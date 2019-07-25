module SkeletonSpec (spec) where

import           Cardano.Prelude
import qualified Data.Text       as T
import           Skeleton
import           Test.Hspec

spec :: Spec
spec = describe "Skeleton project" $
  it "should greet user" $
    T.unpack message `shouldContain` "Hello"

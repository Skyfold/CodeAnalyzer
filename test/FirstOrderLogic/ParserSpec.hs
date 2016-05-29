module FirstOrderLogic.ParserSpec (spec) where

import Test.Hspec
import FirstOrderLogic.Parser
import FirstOrderLogic.Syntax
import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary, arbitrary)

spec :: Spec
spec = do
  describe "test" $ do
    it "should" $ do
      pendingWith "Testing is usefull, but time consuming"

instance (Arbitrary a, Arbitrary b) => Arbitrary (Expr a b) where
    arbitrary a = case a of
        liftM2 Expr arbitrary arbitrary

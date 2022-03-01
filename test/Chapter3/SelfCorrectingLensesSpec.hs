module Chapter3.SelfCorrectingLensesSpec where

import Chapter3.SelfCorrectingLenses
import Control.Lens
import Test.Hspec

spec :: Spec
spec = do
  describe "ProducePrices lens setter" $ do
    context "limePrice" $ do
      it "rounded to zero if negative" $
        limePriceEq (set limePrice (-0.1) ProducePrices {_limePrice = 0.1, _lemonPrice = 0.1}) 0.0
      it "round up lemonPrice within 0.5" $
        (set limePrice (0.7) ProducePrices {_limePrice = 0.1, _lemonPrice = 0.1}) `shouldBe` ProducePrices {_limePrice = 0.7, _lemonPrice = 0.2}
      it "round down lemonPrice within 0.5" $
        (set limePrice (0.1) ProducePrices {_limePrice = 0.7, _lemonPrice = 0.7}) `shouldBe` ProducePrices {_limePrice = 0.1, _lemonPrice = 0.6}
    context "lemonPrice" $ do
      it "rounded to zero if negative" $
        lemonPriceEq (set lemonPrice (-0.1) ProducePrices {_limePrice = 0.1, _lemonPrice = 0.1}) 0.0
      it "round up limePrice within 0.5" $
        (set lemonPrice (0.7) ProducePrices {_limePrice = 0.1, _lemonPrice = 0.1}) `shouldBe` ProducePrices {_limePrice = 0.2, _lemonPrice = 0.7}
      it "round down limePrice within 0.5" $
        (set lemonPrice (0.1) ProducePrices {_limePrice = 0.7, _lemonPrice = 0.7}) `shouldBe` ProducePrices {_limePrice = 0.6, _lemonPrice = 0.1}

limePriceEq :: ProducePrices -> Rational -> Expectation
limePriceEq (ProducePrices p1 _) p2 = p1 `shouldBe` p2

lemonPriceEq :: ProducePrices -> Rational -> Expectation
lemonPriceEq (ProducePrices _ p1) p2 = p1 `shouldBe` p2

module Lib.XMonad.UtilsSpec (spec) where

import           Lib.XMonad.ScreensMock
import           Lib.XMonad.Utils
import           Test.Hspec

spec :: Spec
spec = do
    describe "sortedScreenIds" $ do
        it "returns a current screen" $ do
            let stMock = ScreensMock
                    { _current = mkMockScreen 1 "1"
                    , _visible = []
                    }
            sortedScreenIds stMock `shouldBe` [1]
        it "returns all visible screens" $ do
            let stMock = ScreensMock
                    { _current = mkMockScreen 1 "1"
                    , _visible = [mkMockScreen 2 "2", mkMockScreen 3 "3"]
                    }
            sortedScreenIds stMock `shouldBe` [1, 2, 3]
        it "returns a sorted result" $ do
            let stMock = ScreensMock
                    { _current = mkMockScreen 1 "1"
                    , _visible = [mkMockScreen 3 "2", mkMockScreen 2 "3"]
                    }
            sortedScreenIds stMock `shouldBe` [1, 2, 3]

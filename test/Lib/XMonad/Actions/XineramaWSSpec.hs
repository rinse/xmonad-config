module Lib.XMonad.Actions.XineramaWSSpec (spec) where

import           Lib.XMonad.Actions.XineramaWS
import           Test.Hspec

spec :: Spec
spec = do
    describe "correspondence" $ do
        it "corresponds all workspaces to that screen, when there is one screen." $ do
            let screenIds = [1] :: [Int]
                workspaceIds = ["1", "2", "3"]
            correspondence screenIds workspaceIds `shouldBe` [(1, workspaceIds)]
        it "displays '1 2' for workspace 1, '3 4' for workspace 2." $ do
            let screenIds = [1, 2] :: [Int]
                workspaceIds = ["1", "2", "3", "4"]
            correspondence screenIds workspaceIds `shouldBe` [(1, ["1", "3"]), (2, ["2", "4"])]
        it "displays '1 2 3' for workspace 1, '4 5 6' for workspace 2." $ do
            let screenIds = [1, 2, 3] :: [Int]
                workspaceIds = ["1", "2", "3", "4", "5", "6"]
            correspondence screenIds workspaceIds `shouldBe` [(1, ["1", "4"]), (2, ["2", "5"]), (3, ["3", "6"])]
        it "may correspond no workspace for a display" $ do
            let screenIds = [1, 2] :: [Int]
                workspaceIds = ["1", "2", "3"]
            correspondence screenIds workspaceIds `shouldBe` [(1, ["1", "3"]), (2, ["2"])]
        it "returns an empty list when there are no workspaces" $ do
            let screenIds = [1] :: [Int]
                workspaceIds = [] :: [String]
            correspondence screenIds workspaceIds `shouldBe` []
        xit "throws an exception when there are no screens" $ do
            let screenIds = [] :: [Int]
                workspaceIds = ["1"]
            correspondence screenIds workspaceIds `shouldBe` []
    describe "stepElement" $ do
        it "takes a list and a current element and function to iterate the list" $
            stepElement (+ 2) ['W', 'O', 'R', 'L', 'D'] 'O' `shouldBe` Just 'L'
        it "also works for backward iteration" $
            stepElement (subtract 1) ['W', 'O', 'R', 'L', 'D'] 'O' `shouldBe` Just 'W'
        it "returns Nothing when the list doesn't have enough length" $
            stepElement (+ 4) ['W', 'O', 'R', 'L', 'D'] 'O' `shouldBe` Nothing
        it "returns Nothing when a current element doesn't found in a list" $
            stepElement (+ 2) ['W', 'O', 'R', 'L', 'D'] 'M' `shouldBe` Nothing

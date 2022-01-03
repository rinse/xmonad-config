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
    describe "correspondingWorkspaces" $ do
        it "returns corresponding workspaces to a specific screen." $ do
            let screenIds = [1, 2]
                workspaceIds = ["1", "2", "3", "4"]
                currentScreenId = 1 :: Int
            getCorresponding screenIds workspaceIds currentScreenId `shouldBe` ["1", "3"]
        it "returns an empty list for an unknown screen id." $ do
            let screenIds = [1, 2]
                workspaceIds = ["1", "2", "3", "4"]
                currentScreenId = 3 :: Int
            getCorresponding screenIds workspaceIds currentScreenId `shouldBe` []
        it "returns an empty list for no workspaces" $ do
            let screenIds = [1]
                workspaceIds = [] :: String
                currentScreenId = 3 :: Int
            getCorresponding screenIds workspaceIds currentScreenId `shouldBe` []
        xit "throws an exception when there are not screens" $ do
            let screenIds = []
                workspaceIds = ["1", "2", "3", "4"]
                currentScreenId = 3 :: Int
            getCorresponding screenIds workspaceIds currentScreenId `shouldBe` []
    describe "neighbourWorkspace" $ do
        it "takes a list and a current element and function to iterate the list" $
            neighbourWorkspace ['W', 'O', 'R', 'L', 'D'] 'O' (+ 2) `shouldBe` Just 'L'
        it "also works for backward iteration" $
            neighbourWorkspace ['W', 'O', 'R', 'L', 'D'] 'O' (subtract 1) `shouldBe` Just 'W'
        it "returns Nothing when the list doesn't have enough length" $
            neighbourWorkspace ['W', 'O', 'R', 'L', 'D'] 'O' (+ 4) `shouldBe` Nothing
        it "returns Nothing when a current element doesn't found in a list" $
            neighbourWorkspace ['W', 'O', 'R', 'L', 'D'] 'M' (+ 2) `shouldBe` Nothing

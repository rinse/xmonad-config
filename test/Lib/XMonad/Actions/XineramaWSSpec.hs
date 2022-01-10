module Lib.XMonad.Actions.XineramaWSSpec (spec) where

import           Lens.Micro
import           Lib.XMonad.Actions.XineramaWS
import           Lib.XMonad.Classes
import           Lib.XMonad.ScreensMock
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           XMonad

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

    describe "workspaceIdsOfScreen" $ do
        it "returns workspace ids which should be shown in a given screen" $ do
            let envMock = EnvMock { _workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] }
            let stMock = ScreensMock
                            { _current = mkMockScreen 1 "1"
                            , _visible = [mkMockScreen 2 "2", mkMockScreen 3 "3"]
                            }
            let wids = workspaceIdsOfScreen envMock stMock 2
            wids `shouldBe` Just ["2", "5", "8"]
        it "returns Nothing when there is no such screen" $ do
            let envMock = EnvMock { _workspaces = ["1", "2", "3", "4"] }
            let stMock = ScreensMock
                            { _current = mkMockScreen 1 "1"
                            , _visible = [mkMockScreen 2 "2", mkMockScreen 3 "3"]
                            }
            let wids = workspaceIdsOfScreen envMock stMock 4
            wids `shouldBe` Nothing
        context "it may return different numbers of workspaces for each screen" $ do
            let envMock = EnvMock { _workspaces = ["1", "2", "3", "4"] }
            let stMock = ScreensMock
                            { _current = mkMockScreen 1 "1"
                            , _visible = [mkMockScreen 2 "2", mkMockScreen 3 "3"]
                            }
            it "returns two workspaces for screen 1" $ do
                let wids = workspaceIdsOfScreen envMock stMock 1
                wids `shouldBe` Just ["1", "4"]
            it "returns one workspaces for screen 2" $ do
                let wids = workspaceIdsOfScreen envMock stMock 2
                wids `shouldBe` Just ["2"]

    describe "stepWorkspace" $ do
        context "there is only one screen" $ do
            let envMock = EnvMock { _workspaces = ["1", "2"] }
            let stMock = ScreensMock
                            { _current = mkMockScreen 1 "1"
                            , _visible = [] -- There are no other screen.
                            }
            it "steps workspaces 1 by 1"  $ do
                let actual = stepWorkspace envMock stMock (+ 1) 1
                actual `shouldBe` Just "2"
            it "returns Nothing when it gets a screen which does not exist"  $ do
                let actual = stepWorkspace envMock stMock (+ 1) 2
                actual `shouldBe` Nothing
        context "there are three screens" $ do
            let envMock = EnvMock { _workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] }
            let stMock = ScreensMock
                            { _current = mkMockScreen 1 "4"
                            , _visible = [mkMockScreen 2 "5", mkMockScreen 3 "6"]
                            }
            it "steps workspaces"  $ do
                let actual = stepWorkspace envMock stMock (+ 1) 1
                actual `shouldBe` Just "7"
            it "steps backwards workspaces"  $ do
                let actual = stepWorkspace envMock stMock (subtract 1) 1
                actual `shouldBe` Just "1"

    describe "initialWorkspaces" $ do
        prop "returns the first workspace" $ \currentWorkspace -> do
            let envMock = EnvMock { _workspaces = ["5", "a", "3", "hello"] }
            let stMock = ScreensMock
                            { _current = mkMockScreen 1 currentWorkspace
                            , _visible = [] -- There are no other screen.
                            }
            initialWorkspaces envMock stMock 1 `shouldBe` Just "5"

newtype EnvMock = EnvMock
    { _workspaces :: [WorkspaceId]
    }

instance HasWorkspaces EnvMock where
    workspacesL = lens _workspaces $ \x y -> x { _workspaces = y }

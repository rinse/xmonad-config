{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib.XMonad.Actions.XineramaWSSpec (spec) where

import qualified Data.Map                      as M
import           Lens.Micro
import           Lib.XMonad.Actions.XineramaWS
import           Lib.XMonad.Classes
import           Lib.XMonad.Lenses
import           Lib.XMonad.ScreensMock
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           XMonad
import qualified XMonad.StackSet               as W

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

    describe "xviewS" $ do
        context "it switches focus to a visible screen i" $ do
            it "switches a current screen to i" $ do
                let windowSet = mockWindowSet
                        (mockScreen 1 (mockWorkspace "1"))  -- current screen
                        [ mockScreen 2 (mockWorkspace "2")  -- visible screens
                        , mockScreen 3 (mockWorkspace "3")
                        ]
                let actual = xviewS' 2 windowSet
                (actual ^. currentL . to W.screen) `shouldBe` 2
            it "sends the current screen to visible screens" $ do
                let windowSet = mockWindowSet
                        (mockScreen 1 (mockWorkspace "1"))  -- current screen
                        [ mockScreen 2 (mockWorkspace "2")  -- visible screens
                        , mockScreen 3 (mockWorkspace "3")
                        ]
                let actual = xviewS' 2 windowSet
                (actual ^. visibleL . to (fmap W.screen)) `shouldBe` [1, 3]
        it "does nothing when i is an invalid screen id" $ do
            let windowSet = mockWindowSet
                    (mockScreen 1 (mockWorkspace "1"))  -- current screen
                    [ mockScreen 2 (mockWorkspace "2")  -- visible screens
                    , mockScreen 3 (mockWorkspace "3")
                    ]
            let actual = xviewS' 4 windowSet
            (actual ^. currentL . screenL) `shouldBe` 1 -- A current screen id doesn't change


newtype EnvMock = EnvMock
    { _workspaces :: [WorkspaceId]
    }

instance HasWorkspaces EnvMock where
    workspacesL = lens _workspaces $ \x y -> x { _workspaces = y }

mockLayout :: Layout Window
mockLayout = Layout Full

newtype MockLayout a = MockLayout ()
    deriving (Eq, Read, Show)

instance LayoutClass MockLayout a

mockWorkspace :: i -> W.Workspace i (Layout Window) a
mockWorkspace tag = W.Workspace
    { W.tag = tag
    , W.layout = mockLayout
    , W.stack = Nothing
    }

mockScreenDetail :: ScreenDetail
mockScreenDetail = SD
    { screenRect = Rectangle
        { rect_x = 0
        , rect_y = 0
        , rect_width = 0
        , rect_height = 0
        }
    }

mockScreen :: ScreenId -> W.Workspace i (Layout Window) a -> W.Screen i (Layout Window) a ScreenId ScreenDetail
mockScreen sid workspace = W.Screen
    { W.workspace = workspace
    , W.screen = sid
    , W.screenDetail = mockScreenDetail
    }

-- type WindowSet   = StackSet  WorkspaceId (Layout Window) Window ScreenId ScreenDetail
mockWindowSet :: W.Screen i (Layout Window) a ScreenId ScreenDetail
              -> [W.Screen i (Layout Window) a ScreenId ScreenDetail]
              -> W.StackSet i (Layout Window) a ScreenId ScreenDetail
mockWindowSet current visible = W.StackSet
    { W.current = current
    , W.visible = visible
    , W.hidden = []
    , W.floating = M.empty
    }

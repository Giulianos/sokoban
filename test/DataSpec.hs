module DataSpec (spec) where

import Test.Hspec
import Data

spec :: Spec
spec = parallel $ do
    describe "cellAt" $ do
        it "returns nothing when given an empty board" $
            cellAt (0,0) [] `shouldBe` Nothing
    describe "cellAt (0,0)" $ do
        it "returns top-left cell when given a non-empty board" $
            let board = [[Floor Nothing, Wall],[Wall, Wall]] in
                cellAt (0,0) board `shouldBe` Just (Floor Nothing)
    describe "cellAt (0,N)" $ do
        it "returns top-right cell when given a non-empty board" $
            let board = [[Wall, Floor Nothing],[Wall, Wall]] in
                cellAt (0,1) board `shouldBe` Just (Floor Nothing)
    describe "cellAt (N,0)" $ do
        it "returns bottom-left cell when given a non-empty board" $
            let board = [[Wall, Wall],[Floor Nothing, Wall]] in
                cellAt (1,0) board `shouldBe` Just (Floor Nothing)
    describe "cellAt (N,N)" $ do
        it "returns bottom-right cell when given a non-empty board" $
            let board = [[Wall, Wall],[Wall, Floor Nothing]] in
                cellAt (1,1) board `shouldBe` Just (Floor Nothing)
    describe "objectAt" $ do
        it "returns player when given a floor with player" $
            let cell = Floor (Just Player) in
                objectAt cell `shouldBe` Just Player
        it "returns player when given a storage with player" $
            let cell = Storage (Just Player) in
                objectAt cell `shouldBe` Just Player
        it "returns box when given a floor with box" $
            let cell = Floor (Just Box) in
                objectAt cell `shouldBe` Just Box
        it "returns box when given a storage with box" $
            let cell = Storage (Just Box) in
                objectAt cell `shouldBe` Just Box
    describe "emptyCell" $ do
        it "returns empty floor when given a floor with player" $
            let cell = Floor (Just Player) in
                objectAt (emptyCell cell) `shouldBe` Nothing
        it "returns empty floor when given a floor with box" $
            let cell = Floor (Just Box) in
                objectAt (emptyCell cell) `shouldBe` Nothing
        it "returns empty floor when given an empty floor" $
            let cell = Floor Nothing in
                objectAt (emptyCell cell) `shouldBe` Nothing
        it "returns empty storage when given a storage with player" $
            let cell = Storage (Just Player) in
                objectAt (emptyCell cell) `shouldBe` Nothing
        it "returns empty storage when given a storage with box" $
            let cell = Storage (Just Box) in
                objectAt (emptyCell cell) `shouldBe` Nothing
        it "returns empty storage when given an empty storage" $
            let cell = Storage Nothing in
                objectAt (emptyCell cell) `shouldBe` Nothing
        it "returns wall when given a wall" $
            let cell = Wall in
                objectAt (emptyCell cell) `shouldBe` Nothing
    describe "putObjectInCell" $ do
        it "returns floor with player when given an empty floor and a player" $
            let cell = Floor Nothing in
                objectAt (putObjectInCell cell Player) `shouldBe` Just Player
        it "returns floor with box when given a floor with box and a player" $
            let cell = Floor (Just Box) in
                objectAt (putObjectInCell cell Player) `shouldBe` Just Box
        it "returns wall when given a wall and a player" $
            let cell = Wall in
                putObjectInCell cell Player `shouldBe` Wall
    describe "canMoveObjectTo" $ do
        it "returns True when given an empty floor" $
            let cell = Floor Nothing in
                canMoveObjectTo cell `shouldBe` True
        it "returns True when given an empty storage" $
            let cell = Storage Nothing in
                canMoveObjectTo cell `shouldBe` True
        it "returns False when given an storage with box" $
            let cell = Storage (Just Box) in
                canMoveObjectTo cell `shouldBe` False
        it "returns False when given an floor with box" $
            let cell = Floor (Just Box) in
                canMoveObjectTo cell `shouldBe` False
        it "returns False when given a wall" $
            let cell = Wall in
                canMoveObjectTo cell `shouldBe` False
    describe "moveObject" $ do
        context "when given (floor with player, empty floor)" $ do
            it "returns (empty floor, floor with player) " $ do
                let src = Floor (Just Player); dst = Floor Nothing in
                    moveObject (src, dst) `shouldBe` (Floor Nothing, Floor (Just Player))
        context "when given (floor with player, floor with box)" $ do
            it "returns (floor with player, floor with box) " $ do
                let src = Floor (Just Player); dst = Floor (Just Box) in
                    moveObject (src, dst) `shouldBe` (src, dst)
        context "when given (floor with box, empty floor)" $ do
            it "returns (empty floor, floor with box) " $ do
                let src = Floor (Just Box); dst = Floor Nothing in
                    moveObject (src, dst) `shouldBe` (Floor Nothing, Floor (Just Box))
        context "when given (empty floor, empty floor)" $ do
            it "returns (empty floor, empty floor) " $ do
                let src = Floor Nothing; dst = Floor Nothing in
                    moveObject (src, dst) `shouldBe` (src, dst)
        context "when given (floor with player, wall)" $ do
            it "returns (floor with player, wall) " $ do
                let src = Floor (Just Player); dst = Wall in
                    moveObject (src, dst) `shouldBe` (src, dst)
    describe "translateObject" $ do
        context "when translating player up with space" $ do
            it "returns board with player one cell up" $ do
                translateObject (2, 1) U boardWithTopSpace `shouldBe` boardWithBottomSpace
        context "when translating player down with space" $ do
            it "returns board with player one cell down" $ do
                translateObject (1, 1) D boardWithBottomSpace `shouldBe` boardWithTopSpace
        context "when translating player left with space" $ do
            it "returns board with player one cell left" $ do
                translateObject (1, 2) L boardWithLeftSpace `shouldBe` boardWithRightSpace
        context "when translating player right with space" $ do
            it "returns board with player one cell right" $ do
                translateObject (1, 1) R boardWithRightSpace `shouldBe` boardWithLeftSpace
    describe "findPlayer" $ do
        context "when given a board with a player" $ do
            it "returns position of player in board" $ do
                findPlayer boardWithTopSpace `shouldBe` Just (2,1)
        context "when given a board without a player" $ do
            it "returns nothing" $ do
                findPlayer boardWithoutPlayer `shouldBe` Nothing
        context "when given an empty board" $ do
            it "returns nothing" $ do
                findPlayer [] `shouldBe` Nothing
    describe "tryMovePlayer" $ do
        context "when given a board with a player and a box next to it with space" $ do
            it "returns the board with the player and boxe moved" $ do
                tryMovePlayer R boardWithPlayerAndBoxLeft `shouldBe` boardWithPlayerAndBoxRight
        context "when given a board with a player and two boxes" $ do
            it "returns the same board" $ do
                tryMovePlayer R boardWithPlayerAndTwoBoxes `shouldBe` boardWithPlayerAndTwoBoxes
    describe "checkFinishedBoard" $ do
        context "when given a finished board" $ do
            it "returns true" $ do
                checkFinishedBoard finishedBoard `shouldBe` True
        context "when given a partially finished board" $ do
            it "returns false" $ do
                checkFinishedBoard partiallyFinishedBoard `shouldBe` False
        context "when given an unfinished board" $ do
            it "returns false" $ do
                checkFinishedBoard unfinishedBoard `shouldBe` False

boardWithTopSpace = [
    [Wall,  Wall,                Wall],
    [Wall,  Floor Nothing,       Wall],
    [Wall,  Floor (Just Player), Wall],
    [Wall,  Wall,                Wall]]

boardWithBottomSpace = [
    [Wall,  Wall,                Wall],
    [Wall,  Floor (Just Player), Wall],
    [Wall,  Floor Nothing,       Wall],
    [Wall,  Wall,                Wall]]

boardWithLeftSpace = [
    [Wall,  Wall,                Floor Nothing, Wall],
    [Wall,  Floor Nothing, Floor (Just Player), Wall],
    [Wall,  Wall,                Floor Nothing, Wall]]

boardWithRightSpace = [
    [Wall,  Wall,                Floor Nothing, Wall],
    [Wall,  Floor (Just Player), Floor Nothing, Wall],
    [Wall,  Wall,                Floor Nothing, Wall]]

boardWithoutPlayer = [
    [Wall,  Wall,                Floor Nothing, Wall],
    [Wall,  Floor Nothing, Floor Nothing, Wall],
    [Wall,  Wall,                Floor Nothing, Wall]]

boardWithPlayerAndBoxRight = [
    [Wall,  Wall,                Floor Nothing, Floor Nothing, Wall],
    [Wall,  Floor Nothing, Floor (Just Player), Floor (Just Box), Wall],
    [Wall,  Wall,                Floor Nothing, Floor Nothing, Wall]]

boardWithPlayerAndBoxLeft = [
    [Wall,  Wall,                Floor Nothing, Floor Nothing, Wall],
    [Wall,  Floor (Just Player), Floor (Just Box), Floor Nothing, Wall],
    [Wall,  Wall,                Floor Nothing, Floor Nothing, Wall]]

boardWithPlayerAndTwoBoxes = [
    [Wall,  Wall,                Floor Nothing, Floor Nothing, Wall],
    [Wall,  Floor (Just Player), Floor (Just Box), Floor (Just Box), Wall],
    [Wall,  Wall,                Floor Nothing, Floor Nothing, Wall]]

finishedBoard = [
    [Wall,  Wall,                Wall, Wall, Wall],
    [Wall,  Floor (Just Player), Floor Nothing, Storage (Just Box), Wall],
    [Wall,  Wall,                Wall, Wall, Wall]]

partiallyFinishedBoard = [
    [Wall,  Wall,                Wall, Wall, Wall],
    [Wall,  Floor (Just Player), Floor Nothing, Storage (Just Box), Wall],
    [Wall,  Floor Nothing, Floor (Just Box), Storage Nothing, Wall],
    [Wall,  Wall,                Wall, Wall, Wall]]

unfinishedBoard = [
    [Wall,  Wall,                Wall, Wall, Wall],
    [Wall,  Floor (Just Player), Floor (Just Box), Storage Nothing, Wall],
    [Wall,  Wall,                Wall, Wall, Wall]]
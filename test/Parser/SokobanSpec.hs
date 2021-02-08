module Parser.SokobanSpec (spec) where

import Test.Hspec
import Parser.Sokoban
import Parser.Monadic
import Sokoban.Data

spec :: Spec
spec = parallel $ do
    describe "cell" $ do
        it "returns wall when parsing #" $
            let [(result, remaining)] = parse cell "#"
            in (result, remaining) `shouldBe` (Wall, "")
        it "returns player on floor when parsing @" $
            let [(result, remaining)] = parse cell "@"
            in (result, remaining) `shouldBe` (Floor (Just Player), "")
        it "returns player on storage when parsing +" $
            let [(result, remaining)] = parse cell "+"
            in (result, remaining) `shouldBe` (Storage (Just Player), "")
        it "returns box on floor when parsing $" $
            let [(result, remaining)] = parse cell "$"
            in (result, remaining) `shouldBe` (Floor (Just Box), "")
        it "returns box on storage when parsing *" $
            let [(result, remaining)] = parse cell "*"
            in (result, remaining) `shouldBe` (Storage (Just Box), "")
        it "returns storage when parsing ." $
            let [(result, remaining)] = parse cell "."
            in (result, remaining) `shouldBe` (Storage Nothing, "")
        it "returns empty floor when parsing space" $
            let [(result, remaining)] = parse cell " "
            in (result, remaining) `shouldBe` (Floor Nothing, "")
    describe "row" $ do
        it "returns row of cells when parsing multiple chars" $
            let [(result, remaining)] = parse row "# #"
            in (result, remaining) `shouldBe` ([Wall, Floor Nothing, Wall], "")
        it "returns empty row when parsing empty string" $
            let [(result, remaining)] = parse row ""
            in (result, remaining) `shouldBe` ([], "")
    describe "board" $ do
        it "returns board of cells when parsing multiple rows of chars" $
            let [(result, remaining)] = parse board "###\n# #\n###\n"
            in (result, remaining) `shouldBe` (simpleBoard, "")
        it "returns empty board when parsing empty string" $
            let [(result, remaining)] = parse board ""
            in (result, remaining) `shouldBe` ([], "")

simpleBoard = [
    [Wall, Wall, Wall],
    [Wall, Floor Nothing, Wall],
    [Wall, Wall, Wall]]
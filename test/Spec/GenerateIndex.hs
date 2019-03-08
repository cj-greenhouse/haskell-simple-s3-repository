module Spec.GenerateIndex (tests) where

import Spec.Prelude

import Data.Text (Text)

tests :: TestTree
tests = testGroup "GenerateIndex" [

    testGenerateIndex

    ]

testGenerateIndex :: TestTree
testGenerateIndex = testCase "generateIndex" $ do
    let name = "alib"
        version = "0.1.0"
        items = [Item name version]

        actual = generateIndex items

    actual === [Entry [name, version, mappend name ".cabal"]]


-------------------------------

data Item = Item Text Text deriving (Show, Eq) -- let's use names and PVP ver
data Entry = Entry [Text] deriving (Show, Eq, Ord)


itemEntry :: Item -> Entry
itemEntry (Item name version) = Entry [name, version, mappend name ".cabal"]

generateIndex :: [Item] -> [Entry]
generateIndex = fmap itemEntry

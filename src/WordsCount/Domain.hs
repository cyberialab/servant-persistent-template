{-# LANGUAGE DeriveGeneric #-}

module WordsCount.Domain where

import Data.Aeson hiding (decode)
import Data.List (sortBy)
import Data.Map (Map)

import qualified Data.Map as Map

import GHC.Generics

-- * data

-- This type is to define the order of the word count result
data SortBy = Asc | Desc deriving (Bounded, Show, Enum)

-- Request type
newtype WordsToCount
  = WordsToCount {
    textToCount :: String
  } deriving (Eq, Show, Generic)

instance FromJSON WordsToCount
instance ToJSON WordsToCount

-- Result type
data WordCount
  = WordCount {
    word :: String,
    count :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON WordCount
instance ToJSON WordCount

-- * core

-- Maps from the map association to the data type that we reutnr

assocToWordCount :: (String, Integer) -> WordCount
assocToWordCount (w, c) = WordCount w c

-- From a splitted string creates a Map with the word and count association
countWordsMap :: [String] -> Map String Integer
countWordsMap l =
  foldl (\ mp x -> Map.insert x ((Map.findWithDefault 0 x mp) + 1) mp) Map.empty l

-- This is the usual way to create orderings in Haskell, using the types LT (Lower than) and GT (Greater than)
compareWordCount :: WordCount -> WordCount -> Ordering
compareWordCount (WordCount _ countA) (WordCount _ countB)
  | countA < countB = GT
  | otherwise       = LT

-- This is just a way to retrieve a sorted list according to the SortBy specific type
sortArray :: [WordCount] -> Maybe SortBy -> [WordCount]
sortArray wordsCount (Just Desc)  = sortBy compareWordCount wordsCount
sortArray wordsCount (Just Asc) = sortBy (flip compareWordCount) wordsCount
sortArray wordsCount Nothing    = wordsCount

-- Using the helper functions this creates and sorts the WordCount list
countWords :: String -> [WordCount]
countWords text =
  let
    splitted = map (filter (/='"')) $ words text
  in
    map assocToWordCount $ Map.assocs (countWordsMap splitted)

module AppSpec where

import Test.Hspec
import Test.QuickCheck.Arbitrary.ADT

import App hiding (getItems)

import Data.List (intercalate, find)

import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck

import Control.Monad.IO.Class (liftIO)

import WordsCount.Domain

randomList :: Arbitrary a => Gen [a]
randomList = frequency
  [ (1, (:) <$> arbitrary <*> return [])
  , (4, (:) <$> arbitrary <*> randomList)
  ]

-- more verbose alternative to make things clear
nonEmptyString :: Gen String
nonEmptyString = fmap show (choose (1, 1000) :: Gen Integer)

instance Arbitrary WordCount where
  arbitrary = WordCount <$> nonEmptyString <*> choose (0, 10000)

instance ToADTArbitrary WordCount

isSorted :: [WordCount] -> (Integer -> Integer -> Bool) -> Integer -> Bool
isSorted l op initialValue = snd $ foldl (\ (prev, b) (WordCount _ curr) -> (curr, b && (curr `op` prev))) (initialValue, True) l

isSortedDesc :: [WordCount] -> Bool
isSortedDesc wordsCount = isSorted wordsCount (<=) $ toInteger 10000

isSortedAsc :: [WordCount] -> Bool
isSortedAsc wordsCount = isSorted wordsCount (>=) $ toInteger 0

spec :: Spec
spec = do
  describe "WordsCount.Domain.assocToWordCount" $ do
    it "turns an (String, Integer) tuple into a WordCount" $ \ env -> do
      property $ \(word, count) -> assocToWordCount (word, count) == WordCount word count

  describe "WordsCount.Domain.countWordsMap" $ do
    it "turns a non empty list of Strings (e.g [String]) into a Map String Integer" $ \ env -> do
      forAll randomList $ \list ->
        forAll (choose (0 :: Int, length list - 1)) $ \n ->
          let numberOfFilter = length $ filter (\w -> w == list!!n) $ list
              numberOfMap = Map.findWithDefault 0 (list!!n) (countWordsMap list Map.empty)
          in
            numberOfMap == toInteger numberOfFilter
          
    it "turns an empty list of Strings into an empty Map" $ \ env ->
      countWordsMap [] Map.empty `shouldBe` Map.empty

    it "checks ordering of two WordCount elements" $ \ env ->
      property $ \w1@(WordCount _ countA) w2@(WordCount _ countB) ->
        compareWordCount w1 w2 == if countA < countB then GT else LT

  describe "WordsCount.Domain.sortArray" $ do
    it "sorts a [WordCount] in increasing order" $ \env -> do
      forAll randomList $ \list ->
        isSortedAsc $ sortArray list (Just Asc)

    it "sorts a [WordCount] in decreasing order" $ \env -> do
      forAll randomList $ \list ->
        isSortedDesc $ sortArray list (Just Desc)

    it "keeps the same a [WordCount] if no sorting order provided" $ \env -> do
      forAll randomList $ \list ->
        sortArray list Nothing == list
        
  describe "WordsCount.Domain.countWords" $ do
    it "counts the words in a String and returns a [WordCount] with the respective count" $ \ env ->
      forAll (listOf1 nonEmptyString) $ \list ->
        forAll (choose (0 :: Int, length list - 1)) $ \n ->
          let numberOfFilter = length $ filter (\w -> w == list!!n) list
              intercalated = intercalate " " list
              maybeWord = find (\w -> word w == list!!n) (countWords intercalated Nothing)
          in
            case maybeWord of
              Nothing ->
                False
              Just v ->
                count v == toInteger numberOfFilter

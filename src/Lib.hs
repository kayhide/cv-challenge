{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import Data.Function (on)
import Data.Monoid ((<>))
import Data.Ord
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Data.Aeson.Lens
import Control.Monad (guard)
import Control.Lens
import System.FilePath

type Subject = Text

data Tutor = Tutor
  { tutorName :: Text
  , tutorSubjects :: [Subject]
  } deriving (Eq, Show)

data Student = Student
  { studentName :: Text
  , studentSubjects :: [Subject]
  } deriving (Eq, Show)

type Matching = (Tutor, Student)
type Plan = [Matching]

showMatching :: Matching -> Text
showMatching (Tutor t _, Student s _) = "(" <> t <> ", " <> s <> ")"

showPlan :: Plan -> Text
showPlan plan = Text.intercalate ", " $ showMatching <$> plan

run :: IO ()
run = do
  (tutors, students) <- readDataFile
  mapM_ (Text.putStrLn . showPlan) $ snd $ bests' tutors students
  where
    bests' tutors students = bests $ plans $ matchings tutors students

plans' :: IO [[Matching]]
plans' = do
  (tutors, students) <- readDataFile
  return $ plans $ matchings tutors students

dataFile :: FilePath
dataFile = "data" </> "members.yaml"

readDataFile :: IO ([Tutor], [Student])
readDataFile = do
  Just vals <- readYaml dataFile
  return (tutors vals, students vals)
  where
    tutor (name, xs) = Tutor name $ xs ^.. values . _String
    tutors vals = tutor <$> (vals ^?! key "tutors") ^@.. members

    student (name, xs) = Student name $ xs ^.. values . _String
    students vals = student <$> (vals ^?! key "students") ^@.. members

readYaml :: FilePath -> IO (Maybe Yaml.Value)
readYaml = Yaml.decodeFile


isMatching :: Tutor -> Student -> Bool
isMatching (Tutor _ xs) (Student _ ys) = (not . null) $ List.intersect xs ys

matchings :: [Tutor] -> [Student] -> [Matching]
matchings tutors students = do
  t <- tutors
  s <- students
  guard $ isMatching t s
  return (t, s)

plans :: [Matching] -> [Plan]
plans [] = [[]]
plans (x : xs) = taking ++ skipping
  where
    taking = (x :) <$> plans (except x xs)
    skipping = plans xs

except :: Matching -> [Matching] -> [Matching]
except (t, s) = filter pred
  where
    pred (t', s') = t /= t' && s /= s'

bests :: [Plan] -> (Int, [Plan])
bests = foldr pick (0, [])
  where
    pick x (n, xs)
      | length x > n = (length x, [x])
      | length x == n = (n, x : xs)
      | otherwise = (n, xs)

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : A program that splits a video into sentences and prints them in SRT format.
-- Copyright   : (c) 2025
-- License     : MIT
-- Maintainer  : vivaswt@gmail.com
-- Stability   : experimental
--
-- This program reads a tab-separated text file with three columns.
-- The first column is the start time of a segment in seconds.
-- The second column is the end time of the segment in seconds.
-- The third column is the text of the segment.
-- The program splits the segments into sentences and prints as SRT format.
-- To run the program, you need to specify the input file name as a command line argument.
-- for example:
--   edsrt input.txt
module Main (main) where

import Control.Exception.Safe
  ( SomeException (SomeException),
    catch,
  )
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Char (isSpace)
import Data.List (intercalate, intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Environment (getArgs)

-- | A data structure representing a segment of a video.
data Segment = Segment
  { -- | The start time of the segment
    segmentStart :: NominalDiffTime,
    -- | The end time of the segment
    segmentEnd :: NominalDiffTime,
    -- | The text of the segment
    segmentText :: T.Text
  }
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  result <- runExceptT f
  case result of
    Left err -> TIO.putStrLn $ "error:" <> err
    Right _ -> return ()

-- | Split the segments as a sentence and print them
-- Assuming a sentece ends with a period, question mark, or exclamation mark.
-- The input data is read from a file specified by the command line argument.
-- If the file does not exist, an error message is displayed.
f :: ExceptT T.Text IO ()
f = do
  fileName <- getInputFileName
  dataLines <- fmap parseInputData . safeReadFile $ fileName
  lift
    . mapM_ TIO.putStrLn
    . intercalate [""] -- Add an empty line between SRT rows
    . zipWith segmentToSrtRow [1 ..]
    . map mergeSegments
    . groupByPredicate isEndOfSentence
    $ dataLines

-- | Merge a list of Segments into a single Segment
-- The start time of the merged segment is the start time of the first segment.
-- The end time of the merged segment is the end time of the last segment.
-- The text of the merged segment is the concatenation of the text of all segments.
mergeSegments :: [Segment] -> Segment
mergeSegments [] = error "Empty list of Segments cann't be merged"
mergeSegments segments =
  Segment
    { segmentStart = segmentStart . head $ segments,
      segmentEnd = segmentEnd . last $ segments,
      segmentText = T.unwords . map segmentText $ segments
    }

-- | Check if the segment is the end of a sentence
-- A segment is considered the end of a sentence
-- if the last character of the text is a period, question mark, or exclamation mark.
isEndOfSentence :: Segment -> Bool
isEndOfSentence Segment {segmentText = t} = T.last t `T.elem` ".!?"

-- | Get input file name from command line arguments
-- If the number of arguments is not 1, an error message is returned.
getInputFileName :: ExceptT T.Text IO String
getInputFileName = do
  args <- lift getArgs
  case args of
    [name] -> return name
    _ -> throwE "The input file name is required."

-- | Read file safely
-- If the file does not exist, an error message is returned.
-- If the file exists, the contents of the file are returned.
safeReadFile :: FilePath -> ExceptT T.Text IO T.Text
safeReadFile fileName = do
  catch (lift $ TIO.readFile fileName) $
    \(SomeException e) -> throwE $ "Fail to open the file " <> T.pack (show e)

-- | Parse input data to a list of Segment
-- The input data is a tab-separated text file with three columns.
-- If the input data is not in the correct format, an exception will be thrown.
-- The third column may contain leading spaces, so they are removed.
parseInputData :: T.Text -> [Segment]
parseInputData inputData = do
  linetext <- T.lines inputData
  let (w1 : w2 : w3 : _) = take 3 . splitByTab $ linetext
  return $
    Segment
      (readAsDiffTime w1)
      (readAsDiffTime w2)
      (ltrim w3)
  where
    splitByTab = T.splitOn "\t"
    ltrim = T.dropWhile isSpace

-- | Read a text as a DiffTime
readAsDiffTime :: T.Text -> NominalDiffTime
readAsDiffTime = realToFrac . (read :: String -> Double) . T.unpack

-- | Split a list into sublists based on a predicate.
-- ELements satisfying the predicate end a sublist, while others are grouped together.
groupByPredicate :: (a -> Bool) -> [a] -> [[a]]
groupByPredicate _ [] = []
groupByPredicate p (x : xs)
  | p x = [x] : groupByPredicate p xs
  | otherwise = case groupByPredicate p xs of
      [] -> [[x]]
      (y : ys) -> (x : y) : ys

-- | Convert a Segment to a list of SRT rows
-- The SRT row consists of four lines:
--   ie, the segment number, the start and end times, and the text of the segment.
-- Warning:
--  An empty line is not added at the end of the SRT rows.
--  Because if it is added, it will be repeated at the end of the file.
segmentToSrtRow :: Int -> Segment -> [T.Text]
segmentToSrtRow
  i
  Segment {segmentStart = start, segmentEnd = end, segmentText = text} =
    [ T.pack . show $ i,
      ftime start <> " --> " <> ftime end,
      text
    ]
    where
      -- \| Format a NominalDiffTime as a string in the SRT format
      ftime =
        T.pack
          . map (\c -> if c == '.' then ',' else c)
          . formatTime defaultTimeLocale "%0H:%0M:%03ES"

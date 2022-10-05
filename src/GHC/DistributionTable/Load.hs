{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}


module GHC.DistributionTable.Load
  ( -- * Important
    loadHeader
  ) where

import Control.Arrow (first)
import Control.Monad ((<=<))
import Data.Array
import Data.Char (isSpace, toLower)
import Data.List (uncons)
import Data.Foldable
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import GHC.DistributionTable.Type
import Language.Haskell.TH.Syntax


loadHeader :: Header
loadHeader =
    let loadedVersioningDataText :: String
        loadedVersioningDataText = $(
                let dataFile = "data/distribution-versioning.csv"
                    embedStr = fmap (LitE . StringL)  . runIO
                in  qAddDependentFile dataFile *> embedStr (readFile dataFile)
            )

        precompiledVersioningData :: Header
        precompiledVersioningData = fst $ readCSV loadedVersioningDataText

    in $$( [|| precompiledVersioningData ||] )


moduleName :: String
moduleName = $(fmap loc_module qLocation >>= \modStr ->  return (LitE (StringL modStr) ))


errorMessage :: String -> a
errorMessage str = error $ fold [ "ERROR: ", moduleName, "\n\t", str ]


parseErrorMessage :: String -> a
parseErrorMessage str =  errorMessage $ "Parsing tabular data from CSV file\n\t\t" <> str


readCSV :: String -> (Header, Digest)
readCSV inputString =
    let (header, _dataRows) = first parseHeader $ parseTabularStructure inputString
    in  header `seq` undefined


makeHeader :: [String] -> Header
makeHeader packages =
    let lower = minBound
        upper = toEnum $ length packages - 1
    in  Header $ listArray (lower, upper) packages


{- -- --- ---- ----- ------
   CSV Parsing functions
------ ----- ---- --- -- -}


parseTabularStructure :: String -> (String, [String])
parseTabularStructure =
    let compileTimeError = errorMessage "Empty file...!?"
    in  fromMaybe compileTimeError . uncons . lines


parseRow :: String -> Either Word [String]
parseRow = parseRowCSV '"' ','


parseHeader :: String -> Header
parseHeader =
    let groupByFour i = \case
            []           -> []
            [_]          -> groupErrorMessage i 1
            [_,_]        -> groupErrorMessage i 2
            [_,_,_]      -> groupErrorMessage i 3
            w:x:y:z:next -> case validateSuffixes w x y z of
                Nothing -> ranksErrorMessage i
                Just names -> case validateColumnID names of
                    Nothing -> namesMessage i
                    Just name -> name : groupByFour (i + 1) next

        validateColumnID (w, x, y, z)
            | all (== w) [x,y,z] = Just w
            | otherwise = Nothing

        validateSuffixes w x y z = (,,,)
            <$> (Major `suffixing` w)
            <*> (Minor `suffixing` x)
            <*> (Patch `suffixing` y)
            <*> (Point `suffixing` z)

        suffixing :: VersionRank -> String -> Maybe String
        suffixing rank entity =
            let expect = ('_' :) $ toLower <$> show rank
                expectLen = length expect
                entityLen = length entity
                (prefix, actual) = splitAt (entityLen - expectLen) entity
            in  if expect == actual
                then Just prefix
                else Nothing

        errorPrefix i = "Grouping Header of package number " <> show i
        cellsErrorMessage i   = parseErrorMessage $ "Header Row, Cell number " <> show i
        groupErrorMessage i j = parseErrorMessage $
            fold [ errorPrefix i, ", found only (", show j, "/4)"]
        ranksErrorMessage i   = parseErrorMessage $
            fold [ errorPrefix i, ", suffix(es) do not match version rank(s)"]
        namesMessage i   = parseErrorMessage $
            fold [ errorPrefix i, ", all four names are not the same"]

    in  either cellsErrorMessage (makeHeader . groupByFour 1) . parseRow


parseRowCSV
  :: Char  -- ^ Fields enclosed by
  -> Char  -- ^ Fields seperated by
  -> String
  -> Either Word [String]
parseRowCSV enc sep =
    let go :: Word -> String -> Either Word [String]
        go i input =
          case cell $ trim input of
              Nothing -> Left i
              Just (extra, value) ->
                  case trim extra of
                      c:next | c == sep -> (value :) <$> go (i + 1) next
                      [] -> Right [value]
                      _  -> Left i

        trim :: String -> String
        trim = dropWhile isSpace

        cell :: String -> Maybe (String, String)
        cell =
            let start str = (str, mempty)
            in  quoted <=< within <=< quoted . start

        quoted :: (String, String) -> Maybe (String, String)
        quoted (str, v) =
            case str of
                c:d | c == enc -> Just (d, v)
                _  -> Nothing

        within :: (String, String) -> Maybe (String, String)
        within (str, _) =  pure . swap $ span (not . (== enc)) str

    in  go 1

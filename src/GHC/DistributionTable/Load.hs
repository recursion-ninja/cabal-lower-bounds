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
  ( -- * Core Template Haskell
    loadDistributionTable
    -- * Useful Accessors
  , packageIndex
  , versionTable
  ) where

import Control.Arrow ((***), second)
import Control.Monad ((<=<))
import Data.Array.Unboxed
import Data.Char (isSpace, toLower)
import Data.List (uncons)
import Data.Foldable
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Distribution.Solver.Modular.Package (mkPackageName)
import GHC.DistributionTable.Type
import Language.Haskell.TH.Syntax
import Text.Read (readMaybe)

--import Debug.Trace


type PVP = ( Word16, Word16, Word16, Word16 )


loadDistributionTable :: (Header, Digest)
loadDistributionTable =
    let loadedVersioningDataText :: String
        loadedVersioningDataText = $(
                let dataFile = "data/distribution-versioning.csv"
                    embedStr = fmap (LitE . StringL)  . runIO
                in  qAddDependentFile dataFile *> embedStr (readFile dataFile)
            )

        precompiledVersioningData :: (Header, Digest)
        precompiledVersioningData = readCSV loadedVersioningDataText

    in $$( [|| precompiledVersioningData ||] )


packageIndex :: Header
packageIndex = fst loadDistributionTable


versionTable :: Digest
versionTable = snd loadDistributionTable


moduleName :: String
moduleName = $(fmap loc_module qLocation >>= \modStr ->  return (LitE (StringL modStr) ))


errorMessage :: String -> a
errorMessage str = error $ fold [ "ERROR: ", moduleName, "\n\t", str ]


parseErrorMessage :: String -> a
parseErrorMessage str =  errorMessage $ "Parsing tabular data from CSV file\n\t\t" <> str


readCSV :: String -> (Header, Digest)
readCSV = (parseHeader *** parseDigest) . parseTabularStructure


makeDigest :: [[PVP]] -> Digest
makeDigest versioningRows =
    let fromPVP :: PVP -> [Word16]
        fromPVP (w, x, y, z) = [ w, x, y, z ]
        
        measure :: (Enum e, Foldable f) => f a -> e
        measure  = toEnum . pred . length

        collapse :: [[PVP]] -> [Word16]
        collapse = foldMap (foldMap fromPVP)

        ghcLower = minBound
        pkgLower = minBound
        verLower = minBound
        ghcUpper = measure versioningRows
        pkgUpper = measure $ head versioningRows
        verUpper = maxBound
        lower    = (ghcLower, pkgLower, verLower)
        upper    = (ghcUpper, pkgUpper, verUpper)
    in  listArray (lower, upper) $ collapse versioningRows


makeHeader :: [String] -> Header
makeHeader packages =
    let lower = minBound
        upper = toEnum $ length packages - 1
    in  listArray (lower, upper) $ mkPackageName <$> packages


{- -- --- ---- ----- ------
   CSV Parsing functions
------ ----- ---- --- -- -}

parseTabularStructure :: String -> (String, [String])
parseTabularStructure =
    let compileTimeError = errorMessage "Empty file...!?"
    in  fromMaybe compileTimeError . uncons . lines


parseDigest :: [String] -> Digest
parseDigest =
    let groupByFour :: Word -> Word -> [String] -> [PVP]
        groupByFour i j = \case
            []          -> []
            [_]         -> groupErrorMessage i j 1
            [_,_]       -> groupErrorMessage i j 2
            [_,_,_]     -> groupErrorMessage i j 3
            w:x:y:z:aft -> case validateVersion w x y z of
                Nothing -> readsErrorMessage i j
                Just parts -> parts : groupByFour i (j + 1) aft

        validateVersion
          :: String
          -> String
          -> String
          -> String
          -> Maybe PVP
        validateVersion w x y z = (,,,)
            <$> readMaybe w
            <*> readMaybe x
            <*> readMaybe y
            <*> readMaybe z

        errorPrefix i j = fold [ "Data row", show i, ", package number ", show j ]
        cellsErrorMessage i j   = parseErrorMessage $ fold [ "CSV error: row ", show i, ", cell ", show j ]
        groupErrorMessage i j k = parseErrorMessage $ fold [ errorPrefix i j, ", found only (", show k, "/4)"]
        readsErrorMessage i j   = parseErrorMessage $ fold [ errorPrefix i j, "Read error of version parts" ]

        assembleRow :: Word -> String -> [PVP]
        assembleRow i = either (cellsErrorMessage i) (groupByFour i 1) . parseRow

    in  makeDigest . zipWith assembleRow [ 1 .. ]


parseHeader :: String -> Header
parseHeader =
    let groupByFour i = \case
            []          -> []
            [_]         -> groupErrorMessage i 1
            [_,_]       -> groupErrorMessage i 2
            [_,_,_]     -> groupErrorMessage i 3
            w:x:y:z:aft -> case validateSuffixes w x y z of
                Nothing -> ranksErrorMessage i
                Just names -> case validateColumnID names of
                    Nothing -> namesMessage i
                    Just name -> name : groupByFour (i + 1) aft

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
            let normal = fmap toLower
                expect = ('_' :) . normal $ show rank
                expectLen = length expect
                entityLen = length entity
                (prefix, actual) = second normal $ splitAt (entityLen - expectLen) entity
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


parseRow :: String -> Either Word [String]
parseRow = parseRowCSV '"' ','


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

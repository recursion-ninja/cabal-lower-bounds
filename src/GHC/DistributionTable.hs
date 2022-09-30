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


module GHC.DistributionTable
  ( -- * Data-types
    -- ** Tabular Structures
    Header
  , Digest
    -- ** Indices
  , Ordinal
  , PackageRank
  , VersionRank
    -- ** Datum
  , VersionPart
  , partNumber
    -- * Important
  , readCSV
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (guard)
import Data.Array.Unboxed
import Data.Coerce (coerce)
import Data.Char (digitToInt, isDigit)
import Data.Data (Data) 
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.Word (Word8, Word16)
import Distribution.Pretty (Pretty(..))
import Distribution.Solver.Compat.Prelude (Structured)
import GHC.Generics (Generic)
import Text.PrettyPrint (int, text)
import Text.Read
import Text.ParserCombinators.ReadP (ReadP, satisfy)


newtype Digest = Digest (Array (Ordinal, PackageRank, VersionRank) VersionPart)
    deriving stock   (Data, Generic)
    deriving newtype (Eq, NFData, Ord)


newtype Header = Header (Array Ordinal String)
    deriving stock   (Data, Generic)
    deriving newtype (Eq, NFData, Ord)


{- |
Encodes a compact subspace of the natural numbers \( \mathbb{N} \), acting as a linear index for the \(1^{st}\) dimension of all pre-computed structures.
Ordinal indices begin at \( 0 \) and end at \( n - 1 \), where \( n \) is the cardinality of the pre-computed structure's \(1^{st}\) dimension.
-}
newtype Ordinal = Ordinal Word8
    deriving stock   (Data, Generic)
    deriving newtype (Bounded, Enum, Eq, Ix, NFData, Num, Ord, Read, Show)


{- |
A 'PackageRank' of \(i\) represents the \(i^{th}\) core library package distributed with GHC, for some total ordering of the core libraries.

Serves as a linear index for the \(2^{nd}\) dimension of the 'Digest' pre-computed structure.
-}
newtype PackageRank = PackageRank Word8
    deriving stock   (Data, Generic)
    deriving newtype (Bounded, Enum, Eq, Ix, NFData, Num, Ord, Read, Show)


newtype VersionPart = VersionPart Word16
    deriving stock   (Data, Generic)
    deriving newtype (Bounded, Enum, Eq, NFData, Ord)


{- |
A 'VersionRank' indicates the precedence of a versioning sequence, comprised of 'VersionPart's.

  * @PVP@ specifies a sequence of at least one and up to four 'VersionPart's.

  * @SemVer@ specifies a sequence of exactly /three/ 'VersionPart's.

Serves as a linear index for the \(3^{rd}\) dimension of the 'Digest' pre-computed structure.
-}
data  VersionRank
    = Major
    | Minor
    | Patch
    | Point
    deriving anyclass (NFData)
    deriving stock (Bounded, Data, Enum, Eq, Generic, Ix, Ord, Read, Show)


instance Pretty Ordinal where

    pretty = int . fromEnum . (coerce :: Ordinal -> Word8)

    prettyVersioned = const pretty


instance Pretty PackageRank where

    pretty = int . fromEnum . (coerce :: PackageRank -> Word8)

    prettyVersioned = const pretty


instance Pretty VersionPart where

    pretty = maybe mempty (int . fromEnum) . partNumber

    prettyVersioned = const pretty


instance Pretty VersionRank where

    pretty = text . show

    prettyVersioned = const pretty


instance Read VersionPart where

    readPrec = readPrecWord16 >>= \v ->
        guard (v /= maxBound) $> coerce v


instance Show VersionPart where

    show = (`versionPartShowS` "")

    showsPrec = const versionPartShowS

    showList = showString . intercalate "." . fmap show . catMaybes . fmap partNumber  


instance Structured Ordinal


instance Structured PackageRank


instance Structured VersionPart


instance Structured VersionRank


readCSV :: String -> (Header, Digest)
readCSV = undefined


partNumber :: VersionPart -> Maybe Int
partNumber = \case
        VersionPart 65535 -> Nothing
        VersionPart value -> Just $ fromEnum value


readPrecWord16 :: ReadPrec Word16
readPrecWord16 =
    let check :: Int -> ReadP Word16
        check n = guard (n <= limit) $> toEnum n

        compute :: String -> ReadP Word16
        compute xs = check . snd $ foldl' f (length xs - 1, 0) xs

        f :: (Int, Int) -> Char -> (Int, Int)
        f (e,s) c = ( e-1, s + (10 ^ e * digitToInt c) )

        limit = fromEnum (maxBound :: Word16)

    in  lift $ some (satisfy isDigit) >>= compute


versionPartShowS :: VersionPart -> ShowS
versionPartShowS =
    let f x y = x <> y
    in  f . maybe "empty" show . partNumber


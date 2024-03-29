{-# Language BangPatterns #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedTuples #-}

module GHC.CompilerVersion.Type
    ( -- * Data-types
      CompilerVersion (..)
      -- * Enumeration
    , versionsOfGHC
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (guard)
import Data.Array.Unboxed
import Data.Char (toUpper)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable
import Data.Functor (void)
import Data.List (intercalate)
import Data.Word (Word8)
import Distribution.Pretty (Pretty(..))
import Distribution.Utils.Structured (Structured)
import GHC.Distribution.Table (Ordinal, VersionRank(..), versionTable)
import GHC.Distribution.Table.Type (readPrecBounded)
import GHC.Exts (IsList(Item, fromListN))
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP (satisfy)
import Text.PrettyPrint (char, int, text)
import Text.Read


{- |
Represents a major release version of GHC.
All 'CompilerVersion' values are /valid/, meaning it is impossible to create a 'CompilerVersion' which does not correspond to an officially published GHC version.


Notably, the following of versions of GHC are excluded from the domain of 'CompilerVersion' values:

  * GHC versions prior to @6.10.1@

  * GHC alpha (@alpha@) versions

  * GHC release candidates (@rc@) versions


An enumeration of GHC versions in ascending order can be produced from 'versionsOfGHC'.
-}
newtype CompilerVersion
    = CompilerVersion Ordinal
    deriving stock (Data, Generic)
    deriving newtype (Eq, Ix, NFData, Ord)


instance Bounded CompilerVersion where

    minBound = CompilerVersion 0

    maxBound = CompilerVersion $ versionRank - 1


instance Enum CompilerVersion where

    toEnum   = CompilerVersion . toEnum . (`mod` versionRank)

    fromEnum = fromEnum . ordinal

    succ v@(CompilerVersion w)
        | v == maxBound = minBound
        | otherwise     = CompilerVersion $ w + 1

    pred v@(CompilerVersion w)
        | v == minBound = maxBound
        | otherwise     = CompilerVersion $ w - 1

    enumFrom = flip selectRange maxBound

    enumFromThen v w = enumFromThenTo v w maxBound

    enumFromTo = selectRange

    enumFromThenTo v w x
        | w <= v || x < v || x < w
        = []
        | otherwise
        = let
              i = ordinal v
              j = ordinal w - i
              k = ordinal x - i

              go :: Ordinal -> Ordinal -> [CompilerVersion]
              go 0 e = [CompilerVersion e]
              go n e = CompilerVersion e : go (n - 1) (e + j)
          in  go (k `div` j) i


instance Pretty CompilerVersion where

    pretty v =
        let (# x, y, z #) = selectVersion $ coerce v
            ppi           = int . fromEnum
            dot           = char '.'
        in  fold [text "GHC-", ppi x, dot, ppi y, dot, ppi z]

    prettyVersioned = const pretty


instance Read CompilerVersion where

    readPrec =
        let ghc = do
                Ident str <- lexP
                guard ("GHC" == fmap toUpper str)
            num = readPrecBounded :: ReadPrec Word8
            sep = lift . void $ satisfy (== '-')
            dot = lift . void $ satisfy (== '.')
            ver = liftA3 knownVersion (ghc *> sep *> num) (dot *> num) (dot *> num)
        in  ver >>= maybe empty pure

    readListPrec = readListPrecDefault


instance Show CompilerVersion where

    show      = (`versionShowS` "")

    showsPrec = const versionShowS

    showList vs = let str = fold ["[ ", intercalate ", " (show <$> vs), " ]"] in showString str


instance Structured CompilerVersion


{- |
An enumeration of GHC versions in ascending order, starting from the 'minBound' of version  @6.10.1@ and ending at 'maxBound'. The polymorphic return type facilitates the construction of any appropriate 'IsList' structure.


Rewrite rules exist to improve the construction of ordered containers such as 'Set'.
-}
versionsOfGHC
    :: (IsList (f CompilerVersion), Item   (f CompilerVersion) ~ CompilerVersion) => f CompilerVersion
versionsOfGHC = fromListN versionRank $ CompilerVersion <$> [0 .. versionRank - 1]


knownVersion :: Word8 -> Word8 -> Word8 -> Maybe CompilerVersion
knownVersion x y z =
    let -- Perform a binary search on the unboxed vector
        -- to determine if a character is valid.
        --
        -- Equally fast, and uses less memory than a Set.
        {-# INLINE go #-}
        go !lo !hi
            | lo > hi
            = Nothing
            | otherwise
            = let
                  !md           = (hi + lo) `div` 2
                  (# i, j, k #) = selectVersion md
                  goRight       = i < x || (i == x && (j < y || (j == y && k < z)))
                  goLeft        = i > x || (i == x && (j > y || (j == y && k > z)))
              in  if goRight
                  then go (md + 1) hi
                  else if goLeft then go lo (md - 1) else Just $ CompilerVersion md
    in  go 0 (versionRank - 1)


ordinal :: CompilerVersion -> Ordinal
ordinal = coerce


selectRange
    :: (IsList (f CompilerVersion), Item   (f CompilerVersion) ~ CompilerVersion)
    => CompilerVersion
    -> CompilerVersion
    -> f CompilerVersion
selectRange i j
    | j < i
    = fromListN 0 []
    | otherwise
    = let
          first = ordinal i
          final = ordinal j
          count = fromEnum $ final - first
      in  fromListN count $ CompilerVersion <$> [first .. final]


selectVersion :: Ordinal -> (# Word8, Word8, Word8 #)
selectVersion i =
    let ver x = fromIntegral $ versionTable ! (i, 0, x) in (# ver Major, ver Minor, ver Patch #)


{- |
The number of /known/ GHC versions.
-}
versionRank :: Integral i => i
versionRank =
    let ~(~(!rowLower, _, _), ~(!rowUpper, _, _)) = bounds versionTable
    in  fromIntegral $ fromEnum rowUpper - fromEnum rowLower


versionShowS :: CompilerVersion -> ShowS
versionShowS v =
    let (# x, y, z #) = selectVersion $ coerce v
        str           = fold ["GHC-", show x, ".", show y, ".", show z]
    in  showString str

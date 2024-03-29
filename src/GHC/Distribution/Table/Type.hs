{-# Language CPP #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveLift #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language UnboxedTuples #-}

module GHC.Distribution.Table.Type
    ( -- * Data-types
      -- ** Tabular Structures
      Digest ()
    , Header ()
      -- ** Indices
    , Ordinal ()
    , PackageRank ()
    , VersionRank (..)
      -- ** Datum
    , VersionPart ()
    , partNumber
      -- ** Utility
    , readPrecBounded
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (guard)
import Data.Array.Unboxed
import Data.Char (digitToInt, isDigit)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Word (Word16, Word8)
import Distribution.Compat.Prelude (Binary)
#if MIN_VERSION_base(4,13,0)
#else
import Distribution.Compat.Semigroup ((<>))
#endif
import Distribution.Types.PackageName (PackageName)
import Distribution.Pretty (Pretty(..))
import Distribution.Utils.Structured (Structured)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Text.ParserCombinators.ReadP (ReadP, satisfy)
import Text.PrettyPrint (int, text)
import Text.Read


type Digest = UArray (Ordinal, PackageRank, VersionRank) Word16


type Header = Array PackageRank PackageName


{- |
Encodes a compact subspace of the natural numbers \( \mathbb{N} \), acting as a linear index for the \(1^{st}\) dimension of all pre-computed structures.
Ordinal indices begin at \( 0 \) and end at \( n - 1 \), where \( n \) is the cardinality of the pre-computed structure's \(1^{st}\) dimension.
-}
newtype Ordinal
    = Ordinal Word8
    deriving stock (Data, Generic, Lift)
    deriving newtype (Binary, Bounded, Enum, Eq, Integral, Ix, NFData, Num, Ord, Read, Real, Show)


{- |
A 'PackageRank' of \(i\) represents the \(i^{th}\) core library package distributed with GHC, for some total ordering of the core libraries.


Serves as a linear index for the \(2^{nd}\) dimension of the 'Digest' pre-computed structure.
-}
newtype PackageRank
    = PackageRank Word8
    deriving stock (Data, Generic, Lift)
    deriving newtype (Binary, Bounded, Enum, Eq, Integral, Ix, NFData, Num, Ord, Read, Real, Show)


newtype VersionPart
    = VersionPart Word16
    deriving stock (Data, Generic, Lift)
    deriving newtype (Binary, Bounded, Enum, Eq, NFData, Ord)


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

    pretty          = int . fromEnum . (coerce :: Ordinal -> Word8)

    prettyVersioned = const pretty


instance Pretty PackageRank where

    pretty          = int . fromEnum . (coerce :: PackageRank -> Word8)

    prettyVersioned = const pretty


instance Pretty VersionPart where

    pretty          = maybe mempty (int . fromEnum) . partNumber

    prettyVersioned = const pretty


instance Pretty VersionRank where

    pretty          = text . show

    prettyVersioned = const pretty


instance Read VersionPart where

    readPrec = (readPrecBounded :: ReadPrec Word16) >>= \v -> guard (v /= maxBound) $> coerce v


instance Show VersionPart where

    show      = (`versionPartShowS` "")

    showsPrec = const versionPartShowS

    showList  = showString . intercalate "." . fmap show . mapMaybe partNumber


instance Structured Ordinal


instance Structured PackageRank


instance Structured VersionPart


instance Structured VersionRank


partNumber :: VersionPart -> Maybe Int
partNumber = \case
    VersionPart 65535 -> Nothing
    VersionPart value -> Just $ fromEnum value


versionPartShowS :: VersionPart -> ShowS
versionPartShowS = let f x y = x <> y in f . maybe "empty" show . partNumber


readPrecBounded :: forall a . (Bounded a, Enum a) => ReadPrec a
readPrecBounded =
    let check :: Int -> ReadP a
        check n = guard (n <= limit) $> toEnum n

        compute :: String -> ReadP a
        compute xs = check . snd $ foldl' f (length xs - 1, 0) xs

        f :: (Int, Int) -> Char -> (Int, Int)
        f (e, s) c = (e - 1, s + (10 ^ e * digitToInt c))

        limit = fromEnum (maxBound :: a)
    in  lift $ some (satisfy isDigit) >>= compute

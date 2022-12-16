{-# Language CPP #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedTuples #-}

module GHC.Distribution
    ( CompilerVersion ()
    , CoreLibrary ()
    , CoreLibraryDistribution ()
      -- * Accessors
    , distributedLibrary
    , distributedVersion
      -- * Queries
    , hasCompilerVersions
    , hasCoreLibraryVersionOf
    , lookupCorrespondingVersionsOf
    ) where

import Control.DeepSeq
import Data.Array.Unboxed
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable
import Data.List (intercalate)
import Data.Word (Word16)
import Distribution.Compat.Prelude (Binary)
#if MIN_VERSION_base(4,13,0)
#else
import Distribution.Compat.Semigroup ((<>))
#endif
import Distribution.Pretty (Pretty(..))
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import Distribution.Utils.Structured (Structured)
import GHC.CompilerVersion.Type
import GHC.CoreLibrary.Type
import GHC.Distribution.Table (Ordinal, PackageRank, VersionRank(..), versionTable)
import GHC.Exts (IsList(Item, fromList, fromListN))
import GHC.Generics (Generic)
import Text.PrettyPrint (char)
import Text.Show (showListWith)


-- | The name and version of a package.
data  CoreLibraryDistribution
    = CoreLibraryDistribution
    { _distributedLibrary  :: {-# UNPACK #-}CoreLibrary
      -- ^ The core library distribution, eg. base
    , _distributedVersion  :: CoreLibraryVersion
      -- ^ the version of this package, eg. 4.8.2.0
    , _distributedCompiler :: {-# UNPACK #-}CompilerVersion
      -- ^ the version of this package, eg. ghc-7.10.3
    }
    deriving stock (Data, Eq, Generic, Ord)


newtype CoreLibraryVersion
    = CoreLibraryVersion Version
    deriving stock (Data, Generic)
    deriving newtype (Binary, Eq, NFData, Ord, Pretty, Structured)


instance Pretty CoreLibraryDistribution where

    pretty (CoreLibraryDistribution pkg ver _) = pretty pkg <> char '-' <> pretty ver

    prettyVersioned = const pretty


instance Show CoreLibraryDistribution where

    show      = (`distributionShowS` "")

    showsPrec = const distributionShowS

    showList  = showListWith distributionShowS


instance Show CoreLibraryVersion where

    show      = (`versionShowS` "")

    showsPrec = const versionShowS

    showList  = showListWith versionShowS


{-|
Extract the 'CoreLibrary' from an identified 'CoreLibraryDistribution'.

/Time:/ \(\, \mathcal{O} \left( 1 \right) \)

/Since:/ 1.0.0
-}
{-# INLINE distributedLibrary #-}
distributedLibrary :: CoreLibraryDistribution -> CoreLibrary
distributedLibrary = _distributedLibrary


{-|
Extract the 'Version' from an identified 'CoreLibraryDistribution'.

/Time:/ \(\, \mathcal{O} \left( 1 \right) \)

/Since:/ 1.0.0
-}
{-# INLINE distributedVersion #-}
distributedVersion :: CoreLibraryDistribution -> Version
distributedVersion = coerce . _distributedVersion


{-|
Lookup the 'CompilerVersion' associated with the 'CoreLibraryDistribution'.

/Time:/ \(\, \mathcal{O} \left( 1 \right) \)

/Since:/ 1.0.0
-}
{-# INLINE hasCompilerVersions #-}
hasCompilerVersions
    :: (IsList (t CompilerVersion), Item (t CompilerVersion) ~ CompilerVersion)
    => CoreLibraryDistribution
    -> t CompilerVersion
hasCompilerVersions (CoreLibraryDistribution lib ver ghc) =
    let (# w, x, y, z #) = case getVersionNumbers ver of
            []                -> (# maxBound, maxBound, maxBound, maxBound #)
            [a]               -> (# a, maxBound, maxBound, maxBound #)
            [a, b]            -> (# a, b, maxBound, maxBound #)
            [a, b, c]         -> (# a, b, c, maxBound #)
            a : b : c : d : _ -> (# a, b, c, d #)

        above :: [CompilerVersion]
        above = equal [ghc .. maxBound]

        below :: [CompilerVersion]
        below
            | ghc == minBound = []
            | otherwise       = equal [ghc, succ ghc .. minBound]

        check :: CompilerVersion -> Bool
        check i = let (# a, b, c, d #) = i `indexVersionParts` lib in a == w && b == x && c == y && d == z

        equal :: [CompilerVersion] -> [CompilerVersion]
        equal = takeWhile check . tail

        found :: [CompilerVersion]
        found = above <> [ghc] <> below

        total :: Int
        total = length found
    in  fromListN total found


{-|
/Time:/ \(\, \mathcal{O} \left( n \right) \)

/Since:/ 1.0.0
-}
{-# INLINE hasCoreLibraryVersionOf #-}
hasCoreLibraryVersionOf
    :: ( Foldable f
       , IsList (t CoreLibraryDistribution)
       , Item (t CoreLibraryDistribution) ~ CoreLibraryDistribution
       )
    => CompilerVersion
    -> f CoreLibrary
    -> t CoreLibraryDistribution
hasCoreLibraryVersionOf ghc =
    let identifyLibrary :: CoreLibrary -> CoreLibraryDistribution
        identifyLibrary lib = CoreLibraryDistribution lib (indexVersion ghc lib) ghc
    in  fromList . fmap identifyLibrary . toList


lookupCorrespondingVersionsOf
    :: (Foldable f, IsList (t CoreLibraryDistribution))
    => CompilerVersion
    -> f CoreLibrary
    -> t CoreLibraryDistribution
lookupCorrespondingVersionsOf = const $ fromList . undefined . toList


distributionShowS :: CoreLibraryDistribution -> ShowS
distributionShowS (CoreLibraryDistribution pkg ver _) = shows pkg . showChar '-' . shows ver


getVersionNumbers :: CoreLibraryVersion -> [Word16]
getVersionNumbers = fmap toEnum . versionNumbers . coerce


indexVersion :: CompilerVersion -> CoreLibrary -> CoreLibraryVersion
indexVersion ghc lib =
    let (# a, b, c, d #) = indexVersionParts ghc lib

        assemble :: [Word16] -> CoreLibraryVersion
        assemble = coerce . mkVersion . fmap fromIntegral . takeWhile (< maxBound)
    in  assemble [a, b, c, d]


indexVersionParts :: CompilerVersion -> CoreLibrary -> (# Word16, Word16, Word16, Word16 #)
indexVersionParts ghc lib =
    let row = coerce ghc :: Ordinal
        col = coerce lib :: PackageRank
        ver part = versionTable ! (row, col, part)
    in  (# ver Major, ver Minor, ver Patch, ver Point #)


versionShowS :: CoreLibraryVersion -> ShowS
versionShowS = showString . intercalate "." . fmap show . getVersionNumbers

{-# Language BangPatterns #-}
{-# Language CPP #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module GHC.CoreLibrary.Type
    ( CoreLibrary (..)
    , coreLibraries
    , coreLibraryName
    , isCoreLibrary
    ) where

import Control.Applicative
import Control.DeepSeq
import Data.Array.IArray
import Data.Coerce
import Data.Data (Data)
import Data.Maybe (isJust)
import Data.String (IsString(..))
import Distribution.Compat.Prelude (Binary)
#if MIN_VERSION_base(4,13,0)
#else
import Distribution.Compat.Semigroup ((<>))
#endif
import Distribution.Pretty (Pretty(..))
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Utils.Structured (Structured)
import GHC.Distribution.Table (PackageRank, packageIndex)
import GHC.Exts (IsList(Item, fromListN))
import GHC.Generics (Generic)
import Text.PrettyPrint (text)
import Text.Read
import Text.Show (showListWith)


newtype CoreLibrary
    = CoreLib PackageRank
    deriving stock (Data, Generic)
    deriving newtype (Binary, Eq, NFData, Ord, Structured)


instance IsString CoreLibrary where

    fromString query =
        let errorMsg =
                error $ "GHC.CoreLibrary.fromString: The package '" <> query <> "' is not a GHC core library!"
        in  maybe errorMsg CoreLib . findPackageRank $ mkPackageName query


instance Pretty CoreLibrary where

    pretty          = text . unPackageName . coreLibraryName

    prettyVersioned = const pretty


instance Read CoreLibrary where

    readPrec = do
        Ident pkg <- lexP
        maybe empty (pure . CoreLib) . findPackageRank $ mkPackageName pkg

    readListPrec = readListPrecDefault


instance Show CoreLibrary where

    show      = (`libraryShowS` "")

    showsPrec = const libraryShowS

    showList  = showListWith libraryShowS


{- |
An enumeration of all the core libraries which has been distributed with /any/ version of GHC.
Enumerates in /ascending/ order, starting from the 'minBound' library of @base@ and ending at 'maxBound'.
The polymorphic return type facilitates the construction of any appropriate 'IsList' structure.


Rewrite rules exist to improve the construction of ordered containers such as 'Set'.


/Time:/ \(\, \mathcal{O} \left( 1 \right) \)


/Since:/ 1.0.0
-}
coreLibraries :: (IsList (f CoreLibrary), Item   (f CoreLibrary) ~ CoreLibrary) => f CoreLibrary
coreLibraries = fromListN coreLibraryRank $ CoreLib <$> [0 .. coreLibraryRank]


{- |
Access the core name of a library via the Cabal-style 'PackageName'.


/Time:/ \(\, \mathcal{O} \left( 1 \right) \)


/Since:/ 1.0.0
-}
coreLibraryName :: CoreLibrary -> PackageName
coreLibraryName = (packageIndex !) . coerce


{- |
Query whether a package name is a core library.


/Time:/ \(\, \mathcal{O} \left( 1 \right) \)


/Since:/ 1.0.0
-}
isCoreLibrary :: PackageName -> Bool
isCoreLibrary = isJust . findPackageRank


{- |
The number of /known/ GHC core library packages.
-}
coreLibraryRank :: Integral i => i
coreLibraryRank = let ~(_, !rowUpper) = bounds packageIndex in fromIntegral $ fromEnum rowUpper


findPackageRank :: PackageName -> Maybe PackageRank
findPackageRank pkgName | pkgName == packageIndex ! 0 = Just 0
findPackageRank pkgName | pkgName == packageIndex ! 1 = Just 1
findPackageRank pkgName | pkgName == packageIndex ! 2 = Just 2
findPackageRank pkgName =
    let -- Perform a binary search on the pre-computed compile-time constant
        -- array to determine if a package name is valid.
        --
        -- Equally fast, and uses less memory than a Set.
        {-# INLINE go #-}
        go :: PackageRank -> PackageRank -> Maybe PackageRank
        go !lo !hi
            | lo > hi
            = Nothing
            | otherwise
            = let
                  !md     = (hi + lo) `div` 2
                  curName = packageIndex ! md
              in  case pkgName `compare` curName of
                  EQ -> Just md
                  GT -> go (md + 1) hi
                  LT -> go lo (md - 1)
        lower = 3
        upper = coreLibraryRank
    in  go lower upper


libraryShowS :: CoreLibrary -> ShowS
libraryShowS = showString . unPackageName . coreLibraryName

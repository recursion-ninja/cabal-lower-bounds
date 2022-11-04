{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

module GHC.CoreLibraries
  ( CoreLibrary()
  , CoreLibraryVersion(..)
    -- * Core functionality
  , coreLibraries
  , coreLibraryName
  , isCoreLibrary
  ) where

import           Control.DeepSeq
import           Data.Coerce
import           Data.Data (Data)
--import           Data.Foldable
import Data.Array.IArray
import Data.Maybe (isJust)
--import           Data.String(IsString)
--import           Data.Map.Strict                                     (Map, (!))
--import qualified Data.Map.Strict                              as Map
--import           Data.Vector.Unboxed (Vector)
--import           Distribution.Compiler
--import           Distribution.Parsec (Parsec)
--import           GHC.CompilerVersion
--import           GHC.DistributionTable (Ordinal, PackageRank, VersionRank(..), packageIndex)
import           GHC.DistributionTable ( PackageRank, packageIndex)
--import           GHC.Generics (Generic)
import           GHC.IsList
import           Distribution.Pretty (Pretty)
import           Distribution.Solver.Modular.Package (PackageName)
import           Distribution.Solver.Compat.Prelude (Binary, Structured)
--import           Distribution.Types.CondTree
--import           Distribution.Types.Dependency
--import           Distribution.Types.GenericPackageDescription        (GenericPackageDescription(..))
--import           Distribution.Types.PackageDescription               (testedWith)
import           Distribution.Types.Version
--import           Distribution.Types.VersionRange
import           GHC.Generics (Generic)


{-
baseVersionToGHC :: Map Version Version
baseVersionToGHC = Map.fromList $ (mkVersion *** mkVersion) <$> mapping
  where
    mapping =
        [ ([4, 0,0,0], [6,10,1])
        , ([4, 1,0,0], [6,10,4])
        , ([4, 2,0,0], [6,12,1])
        , ([4, 2,0,1], [6,12,2])
        , ([4, 2,0,2], [6,12,3])
        , ([4, 3,0,0], [7, 0,1])
        , ([4, 3,1,0], [7, 0,4])
        , ([4, 4,0,0], [7, 2,1])
        , ([4, 4,1,0], [7, 2,2])
        , ([4, 5,0,0], [7, 4,1])
        , ([4, 5,1,0], [7, 4,2])
        , ([4, 6,0,0], [7, 6,1])
        , ([4, 6,0,1], [7, 6,2])
        , ([4, 7,0,0], [7, 8,2])
        , ([4, 7,0,1], [7, 8,3])
        , ([4, 7,0,2], [7, 8,4])
        , ([4, 8,0,0], [7,10,1])
        , ([4, 8,1,0], [7,10,2])
        , ([4, 8,2,0], [7,10,3])
        , ([4, 9,0,0], [8, 0,1])
        , ([4, 9,1,0], [8, 0,2])
        , ([4,10,0,0], [8, 2,1])
        , ([4,10,1,0], [8, 2,2])
        , ([4,11,0,0], [8, 4,1])
        , ([4,11,1,0], [8, 4,4])
        , ([4,12,0,0], [8, 6,5])
        , ([4,13,0,0], [8, 8,4])
        , ([4,14,0,0], [8,10,3])
        , ([4,15,0,0], [9, 0,1])
        , ([4,15,1,0], [9, 0,2])
        , ([4,16,0,0], [9, 2,1])
        , ([4,16,1,0], [9, 2,2])
        , ([4,16,2,0], [9, 2,3])
        , ([4,16,3,0], [9, 2,4])
        , ([4,17,0,0], [9, 4,1])
        , ([4,17,0,0], [9, 4,2])
        ]
-}

newtype CoreLibrary = CoreLib PackageRank
    deriving stock (Data, Generic)
    deriving newtype (Binary, Eq, Ord, NFData, Pretty, Structured)
    -- TODO:
    --   - IsString (throw exception)
    --   - Pretty   (ignore PackageRank)
    --   - Read     (throw exception)
    --   - Show     (ignore PackageRank)
    --   - 


newtype CoreLibraryVersion = CoreLibraryVersion Version


{- |
An enumeration of GHC versions in ascending order, starting from the 'minBound' of version  @6.10.1@ and ending at 'm\
axBound'. The polymorphic return type facilitates the construction of any appropriate 'IsList' structure.

Rewrite rules exist to improve the construction of ordered containers such as 'Set'.
-}
coreLibraries
  :: ( IsList (f CoreLibrary)
     , Item   (f CoreLibrary) ~ CoreLibrary
     )
  => f CoreLibrary
coreLibraries = fromListN coreLibraryRank $ CoreLib <$> [ 0 .. coreLibraryRank - 1 ]


coreLibraryName :: CoreLibrary -> PackageName
coreLibraryName = (packageIndex !) . coerce  


isCoreLibrary :: PackageName -> Bool
isCoreLibrary = isJust . findPackageRank


{-
coreLibraries :: Set CoreLibrary
coreLibraries = Set.fromList $ CoreLib <$>
    [ "ghc"
    , "Cabal"
    , "Win32"
    , "array"
    , "base"
    , "binary"
    , "bytestring"
    , "containers"
    , "deepseq"
    , "directory"
    , "exceptions"
    , "filepath"
    , "ghc-boot-th"
    , "ghc-boot"
    , "ghc-compact"
    , "ghc-heap"
    , "ghc-prim"
    , "ghci"
    , "haskeline"
    , "hpc"
    , "integer-gmp"
    , "libiserv"
    , "mtl"
    , "parsec"
    , "pretty"
    , "process"
    , "stm"
    , "template-haskell"
    , "terminfo"
    , "text"
    , "time"
    , "transformers"
    , "unix"
    , "xhtml"
    ]
-}


{- |
The number of /known/ GHC versions.
-}
coreLibraryRank :: Integral i => i
coreLibraryRank =
  let ~(!rowLower, !rowUpper) = bounds packageIndex
  in  fromIntegral $ fromEnum rowUpper - fromEnum rowLower


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
        go !lo !hi
            | lo > hi   = Nothing
            | otherwise =
                let !md = (hi + lo) `div` 2
                    curName = packageIndex ! md
                in  case pkgName `compare` curName of
                        EQ -> Just $ fromIntegral md
                        GT -> go  (md + 1)  hi
                        LT -> go   lo      (md - 1)
        lower = 3
        upper = pred . snd $ bounds packageIndex
    in  go lower upper


--librariesIncludedWithGHC :: Map CompilerVersion (Map CoreLibrary CoreLibraryVersion)


{-
ghc 	9
Cabal 	3.4.0.0 	Dependency of ghc-pkg utility
Win32 	2.10.0.0 	Dependency of ghc library
array 	0.5.4.0 	Dependency of ghc library
base 	4.15.0.0 	Core library
binary 	0.8.8.0 	Dependency of ghc library
bytestring 	0.10.12.1 	Dependency of ghc library
containers 	0.6.4.1 	Dependency of ghc library
deepseq 	1.4.5.0 	Dependency of ghc library
directory 	1.3.6.1 	Dependency of ghc library
exceptions 	0.10.4 	Dependency of ghc and haskeline library
filepath 	1.4.2.1 	Dependency of ghc library
ghc-boot-th 	9.0.1 	Internal compiler library
ghc-boot 	9.0.1 	Internal compiler library
ghc-compact 	0.1.0.0 	Core library
ghc-heap 	9.0.1 	GHC heap-walking library
ghc-prim 	0.7.0 	Core library
ghci 	9.0.1 	The REPL interface
haskeline 	0.8.1.0 	Dependency of ghci executable
hpc 	0.6.1.0 	Dependency of hpc executable
integer-gmp 	1.1 	Core library
libiserv 	9.0.1 	Internal compiler library
mtl 	2.2.2 	Dependency of Cabal library
parsec 	3.1.14.0 	Dependency of Cabal library
pretty 	1.1.3.6 	Dependency of ghc library
process 	1.6.11.0 	Dependency of ghc library
stm 	2.5.0.0 	Dependency of haskeline library
template-haskell 	2.17.0.0 	Core library
terminfo 	0.4.1.4 	Dependency of haskeline library
text 	1.2.4.1 	Dependency of Cabal library
time 	1.9.3 	Dependency of ghc library
transformers 	0.5.6.2 	Dependency of ghc library
unix 	2.7.2.2 	Dependency of ghc library
xhtml
-}

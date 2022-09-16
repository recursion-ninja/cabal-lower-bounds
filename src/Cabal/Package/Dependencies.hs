{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal.Package.Dependencies
  ( dependencyMinimalConstraint
  , extractLowerBounds
  ) where

import           Control.Arrow                                       ((***), second)
import           Distribution.Compiler
import           Distribution.Types.CondTree
import           Distribution.Types.Dependency
import           Distribution.Types.GenericPackageDescription        (GenericPackageDescription(..))
import           Distribution.Types.PackageDescription               (testedWith)
import           Distribution.Types.Version
import           Distribution.Types.VersionRange
import           Data.Foldable
import           Data.Map.Strict                                     (Map, (!))
import qualified Data.Map.Strict                              as Map
import           Data.Ord


withPinnedLowerBound :: Dependency -> Dependency
withPinnedLowerBound (Dependency p v s) = Dependency p v' s
  where
    v' = withinVersion $ versionLowerBound v


dependencyMinimalConstraint :: Dependency -> VersionRange
dependencyMinimalConstraint = withinVersion . versionLowerBound . depVerRange


extractLowerBounds :: GenericPackageDescription -> (CompilerFlavor, Version, [Dependency])
extractLowerBounds description = (compiler, version, dependencies)
  where
    -- Get dependencies from all build targets
    dependencies = fmap withPinnedLowerBound . fold $
        [ maybe [] condTreeConstraints . condLibrary
        , getDependenciesFromTrees . condSubLibraries
        , getDependenciesFromTrees . condForeignLibs
        , getDependenciesFromTrees . condExecutables
        , getDependenciesFromTrees . condTestSuites
        , getDependenciesFromTrees . condBenchmarks
        ] <*> [description]

    -- Also get the minimal compiler version
    (compiler, version) = getLowestCompiler dependencies . testedWith $ packageDescription description

    getDependenciesFromTrees :: [(a1, CondTree v [Dependency] a2)] -> [Dependency]
    getDependenciesFromTrees = foldMap (condTreeConstraints . snd)


getLowestCompiler :: [Dependency] -> [(CompilerFlavor, VersionRange)] -> (CompilerFlavor, Version)
getLowestCompiler dependencies = \case
    [] -> let bases   = depVerRange <$> filter (("base" ==) . depPkgName) dependencies
              minBase = minimum $ versionLowerBound <$> bases
          in  (GHC, baseVersionToGHC ! minBase)
    xs -> minimumBy (comparing snd) $ second versionLowerBound <$> xs


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
        ]


versionLowerBound :: VersionRange -> Version
versionLowerBound vRange =
    let computeRange :: VersionRangeF Version -> Version
        computeRange = \case
            ThisVersionF            v -> v
            LaterVersionF           v -> v
            OrLaterVersionF         v -> v
            EarlierVersionF         _ -> defaultRange
            OrEarlierVersionF       _ -> defaultRange
            MajorBoundVersionF      v -> v
            UnionVersionRangesF     x y -> min x y
            IntersectVersionRangesF x y -> min x y

        defaultRange :: Version
        defaultRange = mkVersion [0,0,0]

        extractRange :: VersionRange -> Version
        extractRange = cataVersionRange computeRange

    in  if isAnyVersion vRange
        then defaultRange
        else extractRange vRange

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


module GHC.CompilerVersion
  ( CompilerVersion()
  , versionsOfGHC
  ) where

import Control.Arrow ((***))
import Control.Applicative
import Control.DeepSeq
import Control.Monad (guard)
import Data.Array.Unboxed
import Data.Coerce (coerce)
import Data.Char (digitToInt, isDigit, toUpper)
import Data.Data (Data) 
import Data.Function ((&))
import Data.Functor (($>), void)
import Data.List (intercalate)
import Data.Foldable
import Data.Set (Set, fromDistinctAscList)
import Data.Word (Word8)
import Distribution.Pretty (Pretty(..))
import Distribution.Solver.Compat.Prelude (Structured)
import GHC.Generics (Generic)
import GHC.IsList
import Text.PrettyPrint (char, int, text)
import Text.Read
import Text.ParserCombinators.ReadP (ReadP, satisfy)


newtype CompilerVersion = CompilerVersion Word8
    deriving stock   (Data, Generic)
    deriving newtype (Eq, Ix, NFData, Ord)


instance Bounded CompilerVersion where

    minBound = CompilerVersion 0

    maxBound = CompilerVersion $ versionRank - 1


instance Enum CompilerVersion where

    toEnum   = CompilerVersion . toEnum . (`mod` versionRank)

    fromEnum = fromEnum . ordinal

    succ v@(CompilerVersion w)
        | v == maxBound = minBound
        | otherwise = CompilerVersion $ w + 1

    pred v@(CompilerVersion w)
        | v == minBound = maxBound
        | otherwise = CompilerVersion $ w - 1

    enumFrom = flip selectRange maxBound

    enumFromThen v w = enumFromThenTo v w maxBound

    enumFromTo = selectRange

    enumFromThenTo v w x
      | w <= v || x < v || x < w = []
      | otherwise =
        let i = ordinal v
            j = ordinal w - i
            k = ordinal x - i

            go :: Word8 -> Word8 -> [CompilerVersion]
            go 0 e = [ CompilerVersion e ]
            go n e =   CompilerVersion e : go (n-1) (e + j)
        in  go (k `div` j) i


instance Pretty CompilerVersion where

    pretty v =
        let (# x, y, z #) = selectVersion v
            ppi = int . fromEnum
            dot = char '.'
        in  fold [ text "GHC-", ppi x, dot, ppi y, dot, ppi z ]

    prettyVersioned = const pretty


instance Read CompilerVersion where

    readPrec =
        let ghc = do { Ident str <- lexP; guard ("GHC" == fmap toUpper str) }
            num = readPrecWord8
            sep = lift . void $ satisfy (== '-')
            dot = lift . void $ satisfy (== '.')
            ver = liftA3 knownVersion (ghc *> sep *> num) (dot *> num) (dot *> num)
        in  ver >>= maybe empty pure 

    readListPrec = readListPrecDefault


instance Show CompilerVersion where

    show = (`versionShowS` "")

    showsPrec = const versionShowS

    showList vs =
        let str = fold [ "[ ", intercalate ", " (show <$> vs), " ]" ]
        in  showString str


instance Structured CompilerVersion


versionsOfGHC 
  :: ( IsList (f CompilerVersion)
     , Item   (f CompilerVersion) ~ CompilerVersion
     )
  => f CompilerVersion
versionsOfGHC = fromListN versionRank $ CompilerVersion <$> [ 0 .. versionRank - 1 ]

  
{- |
A compact encoding of the "modern" GHC versions.

An unboxed array of bytes with dimension equal to three times the number of GHC
versions. Each interval of three bytes store the major, minor, and patch version
numbers for a single GHC version.

Pre-computed as a compile-time constant.
-} 
compactEncoding :: Array (Word8, Word8) Word8
compactEncoding =
    let versionSet :: Set (Word8, Word8, Word8)
        versionSet = fromDistinctAscList
            [ ( 6, 10, 1 )
            , ( 6, 10, 4 )
            , ( 6, 12, 1 )
            , ( 6, 12, 2 )
            , ( 6, 12, 3 )
            , ( 7,  0, 1 )
            , ( 7,  0, 4 )
            , ( 7,  2, 1 )
            , ( 7,  2, 2 )
            , ( 7,  4, 1 )
            , ( 7,  4, 2 )
            , ( 7,  6, 1 )
            , ( 7,  6, 2 )
            , ( 7,  8, 2 )
            , ( 7,  8, 3 )
            , ( 7,  8, 4 )
            , ( 7, 10, 1 )
            , ( 7, 10, 2 )
            , ( 7, 10, 3 )
            , ( 8,  0, 1 )
            , ( 8,  0, 2 )
            , ( 8,  2, 1 )
            , ( 8,  2, 2 )
            , ( 8,  4, 1 )
            , ( 8,  4, 4 )
            , ( 8,  6, 5 )
            , ( 8,  8, 4 )
            , ( 8, 10, 3 )
            , ( 9,  0, 1 )
            , ( 9,  0, 2 )
            , ( 9,  2, 1 )
            , ( 9,  2, 2 )
            , ( 9,  2, 3 )
            , ( 9,  2, 4 )
            , ( 9,  4, 1 )
            , ( 9,  4, 2 )
            ]

        versionCount :: Word8
        versionCount = toEnum $ length versionSet
        
        versionFlat :: [Word8]
        versionFlat = foldr flat [] versionSet

        flat :: (Word8, Word8, Word8) -> [Word8] -> [Word8]
        flat (x,y,z) = (x :) . (y :) . (z :)
        
        dimensions   = ( versionCount, 3 )
        lowerBound   = ( 0, 0 ) :: (Word8, Word8)
        upperBound   = dimensions & ( pred *** pred )
    in  $$( [|| listArray (lowerBound, upperBound) versionFlat ||] )


knownVersion :: Word8 -> Word8 -> Word8 -> Maybe CompilerVersion
knownVersion x y z =
    let -- Perform a binary search on the unboxed vector
        -- to determine if a character is valid.
        --
        -- Equally fast, and uses less memory than a Set.
        {-# INLINE go #-}
        go !lo !hi
            | lo > hi   = Nothing
            | otherwise =
                let !md = (hi + lo) `div` 2
                    (# i, j, k #) = selectIndex md
                    goRight = i < x || (i == x && (j < y || (j == y && k < z)))
                    goLeft  = i > x || (i == x && (j > y || (j == y && k > z)))
                in  if   goRight
                    then go (md + 1) hi
                    else
                        if   goLeft
                        then go lo (md - 1)
                        else Just $ CompilerVersion md
    in  go 0 (versionRank - 1)


ordinal :: CompilerVersion -> Word8
ordinal = coerce

readPrecWord8 :: ReadPrec Word8
readPrecWord8 =
    let check :: Int -> ReadP Word8
        check n = guard (n <= limit) $> toEnum n

        compute :: String -> ReadP Word8
        compute xs = check . snd $ foldl' f (length xs - 1, 0) xs

        f :: (Int, Int) -> Char -> (Int, Int)
        f (e,s) c = ( e-1, s + (10 ^ e * digitToInt c) )

        limit = fromEnum (maxBound :: Word8)

    in  lift $ some (satisfy isDigit) >>= compute


selectIndex :: Word8 -> (# Word8, Word8, Word8 #)
selectIndex i =
    let a x = compactEncoding ! (i, x)
    in  (# a 0, a 1, a 2 #)


selectRange
  :: ( IsList (f CompilerVersion)
     , Item   (f CompilerVersion) ~ CompilerVersion
     )
  => CompilerVersion -> CompilerVersion -> f CompilerVersion
selectRange i j
    | j < i = fromListN 0 []
    | otherwise =
        let first = ordinal i
            final = ordinal j
            count = fromEnum $ final - first
        in  fromListN count $ CompilerVersion <$> [ first .. final ]
    

selectVersion :: CompilerVersion -> (# Word8, Word8, Word8 #)
selectVersion v = 
    let i = ordinal v
        a x = compactEncoding ! (i, x)
    in  (# a 0, a 1, a 2 #)


{- |
The number of /known/ GHC versions.
-}
versionRank :: Enum e => e
versionRank = toEnum . force $ length compactEncoding `div` 3


versionShowS :: CompilerVersion -> ShowS
versionShowS v = 
    let (# x, y, z #) = selectVersion v
        str = fold [ "GHC-", show x, ".", show y, ".", show z ]
    in  showString str 

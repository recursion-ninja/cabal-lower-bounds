{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}

module Cabal.Package.Parse
    ( ParseError (..)
    , parsePackage
    , readPackage
    , renderParseError
    ) where

import Control.DeepSeq (NFData(..))
import Control.Exception (Exception(..), throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable
import Data.Typeable (Typeable)
import qualified Distribution.Fields as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Parsec as C
import Distribution.Utils.Generic (fromUTF8BS)
import GHC.Generics (Generic)
import System.FilePath (normalise)


-- |
-- Parse error.
data  ParseError f
    = ParseError
    { peFilename :: FilePath
    , peContents :: ByteString
    , peErrors   :: f C.PError
    , peWarnings :: [C.PWarning]
    }
    deriving stock Generic


deriving stock instance (Show (f C.PError)) => Show (ParseError f)


instance (Foldable f, Show (f C.PError), Typeable f) => Exception (ParseError f) where

    displayException = renderParseError


instance (NFData (f C.PError)) => NFData (ParseError f)


-- | High level convenience function to read package definitions, @.cabal@ files.
--
-- May throw 'IOException' when file doesn't exist, and 'ParseError'
-- on parse error.
readPackage :: FilePath -> IO C.GenericPackageDescription
readPackage fp = do
    contents <- BS.readFile fp
    either throwIO return (parsePackage fp contents)


-- | Parse @.cabal@ file.
parsePackage :: FilePath -> ByteString -> Either (ParseError []) C.GenericPackageDescription
parsePackage fp contents = case C.runParseResult $ C.parseGenericPackageDescription contents of
    (ws, Left (_mv, errs)) -> Left $ ParseError fp contents (toList errs) ws
    (_ , Right gpd       ) -> Right gpd


-- | Render parse error highlighting the part of the input file.
renderParseError :: Foldable f => ParseError f -> String
renderParseError (ParseError filepath contents errors warnings)
    | null errors && null warnings
    = ""
    | null errors
    = unlines $ ("Warnings encountered when parsing  file " ++ filepath ++ ":") : renderedWarnings
    | otherwise
    = unlines
        $  ["Errors encountered when parsing file " ++ filepath ++ ":"]
        ++ renderedErrors
        ++ renderedWarnings
    where
        filepath' = normalise filepath

        -- lines of the input file. 'lines' is taken, so they are called rows
        -- contents, line number, whether it's empty line
        rows :: [(String, Int, Bool)]
        rows = zipWith f (BS8.lines contents) [1 ..]            where
                f :: ByteString -> b -> (String, b, Bool)
                f bs i = let s = fromUTF8BS bs in (s, i, isEmptyOrComment s)

        rowsZipper = listToZipper rows

        isEmptyOrComment :: String -> Bool
        isEmptyOrComment s = case dropWhile (== ' ') s of
            ""              -> True   -- empty
            ('-' : '-' : _) -> True   -- comment
            _               -> False

        renderedErrors   = concatMap renderError errors
        renderedWarnings = concatMap renderWarning warnings

        renderError :: C.PError -> [String]
        renderError (C.PError pos@(C.Position row col) msg)
            |
            -- if position is 0:0, then it doesn't make sense to show input
            -- looks like, Parsec errors have line-feed in them
              pos == C.zeroPos = msgs
            | otherwise        = msgs ++ formatInput row col
            where msgs = ["", filepath' ++ ":" ++ C.showPos pos ++ ": error:", trimLF msg, ""]

        renderWarning :: C.PWarning -> [String]
        renderWarning (C.PWarning _ pos@(C.Position row col) msg)
            | pos == C.zeroPos = msgs
            | otherwise        = msgs ++ formatInput row col
            where msgs = ["", filepath' ++ ":" ++ C.showPos pos ++ ": warning:", trimLF msg, ""]

        -- sometimes there are (especially trailing) newlines.
        trimLF :: String -> String
        trimLF = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

        -- format line: prepend the given line number
        formatInput :: Int -> Int -> [String]
        formatInput row col = case advance (row - 1) rowsZipper of
            Zipper xs ys -> before ++ after                where
                    before = case span (\(_, _, b) -> b) xs of
                        (_ , []   ) -> []
                        (zs, z : _) -> map formatInputLine $ z : reverse zs

                    after = case ys of
                        [] -> []
                        (z : _zs) ->
                            [ formatInputLine z                             -- error line
                            , "      | " ++ replicate (col - 1) ' ' ++ "^"  -- pointer: ^
                            ]
                        -- do we need rows after?
                        -- ++ map formatInputLine (take 1 zs)           -- one row after

        formatInputLine :: (String, Int, Bool) -> String
        formatInputLine (str, row, _) = leftPadShow row ++ " | " ++ str

        -- hopefully we don't need to work with over 99999 lines .cabal files
        -- at that point small glitches in error messages are hopefully fine.
        leftPadShow :: Int -> String
        leftPadShow n = let s = show n in replicate (5 - length s) ' ' ++ s


data  Zipper a
    = Zipper [a] [a]


listToZipper :: [a] -> Zipper a
listToZipper = Zipper []


advance :: Int -> Zipper a -> Zipper a
advance n z@(Zipper xs ys)
    | n <= 0 = z
    | otherwise = case ys of
        []        -> z
        (y : ys') -> advance (n - 1) $ Zipper (y : xs) ys'

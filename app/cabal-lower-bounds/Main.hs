module Main
    ( main
    ) where

import Cabal.Package.Dependencies
import Cabal.Package.Parse
import Data.Foldable (traverse_)
import Distribution.Pretty (pretty)
import System.Environment
import Text.PrettyPrint (render, text)
import qualified Text.PrettyPrint as PP


main :: IO ()
main = do
    specifiedFile <- parseArgs <$> getArgs
    case specifiedFile of
        Nothing       -> putStrLn "Specify a file"
        Just filePath -> do
            packageDescription <- readPackage filePath
            let (compiler, version, dependencies) = extractLowerBounds packageDescription
            putStrLn . render $ pretty compiler PP.<> text "-" PP.<> pretty version
      --      traverse_ (putStrLn . render . pretty . dependencyMinimalConstraint) dependencies
            traverse_ (putStrLn . render . pretty) dependencies


parseArgs :: [String] -> Maybe String
parseArgs []      = Nothing
parseArgs (x : _) = Just x

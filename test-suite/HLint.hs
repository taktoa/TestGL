module Main (main) where

import Language.Haskell.HLint (hlint, suggestionSeverity, Severity (..))
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "executable"
    , "library"
    , "test-suite"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    let errorHints = filter ((== Error) . suggestionSeverity) hints
    if null errorHints then exitSuccess else exitFailure

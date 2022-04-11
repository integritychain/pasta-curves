module Main (main) where

import PastaCurves (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)

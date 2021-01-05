module Main where

import qualified TestUtils as TestUtils
import qualified Test.QuickCheck as QuickCheck
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    result <- runAllTests
    if result then exitSuccess else exitFailure

runAllTests :: IO Bool
runAllTests = do
    putStrLn "Tests are:"
    mapM putStrLn $ fst <$> allTests
    results <- sequence $ (QuickCheck.quickCheckWithResult testArgs) <$> (snd <$> allTests)
    return $ all QuickCheck.isSuccess results


allTests :: [(String, QuickCheck.Property)]
allTests = TestUtils.allTests

testArgs :: QuickCheck.Args
testArgs = QuickCheck.stdArgs { QuickCheck.maxSuccess = 1000 }

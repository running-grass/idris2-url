module Main

import Test.General

import Tester.Runner

public export
main : IO ()
main = do
    success <- runTests $ tests
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"
        
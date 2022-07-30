module Main

import Test.General as G
import Test.HTTP as H

import Tester
import Tester.Runner

public export
main : IO ()
main = do
    success <- runTests $ G.tests ++ H.tests
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"
        
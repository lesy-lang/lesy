module Test.Generated.Main1138395189 exposing (main)

import TestUse

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "TestUse" [TestUse.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 159971069436001, processes = 8, globs = [], paths = ["/home/pascal/Dokumente/home/lesy/uvi/browser/tests/TestUse.elm"]}
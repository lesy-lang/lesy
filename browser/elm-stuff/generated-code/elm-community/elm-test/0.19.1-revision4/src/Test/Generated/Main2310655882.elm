module Test.Generated.Main2310655882 exposing (main)

import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 299497968341559, processes = 8, globs = [], paths = ["/home/pascal/Dokumente/home/lesy/uvi/browser/tests/Example.elm"]}
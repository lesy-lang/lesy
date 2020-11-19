module Test.Generated.Main3868372205 exposing (main)

import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 392908834660046, processes = 8, globs = [], paths = ["/home/pascal/Dokumente/home/lesy/uvi/browser/tests/Example.elm"]}
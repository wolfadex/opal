module Cli exposing (..)

import Opal
import Opal.Js
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Process


program : Process -> IO ()
program process =
    case process.argv of
        _ :: filename :: maybeFilename ->
            File.contentsOf filename
                |> IO.exitOnError identity
                |> IO.map Opal.parse
                |> IO.exitOnError identity
                |> IO.map Opal.typeCheck
                |> IO.exitOnError identity
                |> IO.map Opal.Js.compile
                |> IO.exitOnError identity
                |> IO.andThen
                    (\compiledJs ->
                        let
                            outputFilename =
                                case maybeFilename of
                                    [] ->
                                        "opal.js"

                                    name :: _ ->
                                        name
                        in
                        File.writeContentsTo outputFilename compiledJs
                            |> IO.andThen (\() -> Process.print ("Compiled to " ++ outputFilename ++ "\n"))
                    )

        _ ->
            Process.logErr "Usage: npm run dev <start file>\n"

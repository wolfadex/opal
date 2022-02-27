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
                |> IO.andThen
                    (\result ->
                        case result of
                            Err err ->
                                Process.logErr ("Failed to read file: " ++ err)
                                    |> IO.andThen (\() -> Process.exit 1)

                            Ok content ->
                                case Opal.parse content of
                                    Err err ->
                                        Process.logErr err
                                            |> IO.andThen (\() -> Process.exit 1)

                                    Ok code ->
                                        case Opal.Js.compile code of
                                            Ok compiledJs ->
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

                                            Err compileErr ->
                                                Process.logErr compileErr
                                                    |> IO.andThen (\() -> Process.exit 1)
                    )

        _ ->
            Process.logErr "Usage: npm run dev <start file>\n"

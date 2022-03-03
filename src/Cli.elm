module Cli exposing (..)

import Cli.ExitStatus exposing (ExitStatus)
import Cli.LowLevel
import Cli.Option
import Cli.OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.TypoSuggestion as TypoSuggestion
import List.Extra
import Opal
import Opal.C
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Process


program : Process -> IO ()
program process =
    case run cliOptions ("" :: "opal" :: List.drop 1 process.argv) "0.1.0" of
        SystemMessage Cli.ExitStatus.Failure str ->
            Process.logErr str
                |> IO.andThen (\() -> Process.exit 1)

        SystemMessage Cli.ExitStatus.Success str ->
            Debug.todo ("TODO - Success exit: " ++ str)

        CustomMatch Init ->
            Debug.todo "TODO - Initialize Opal project"

        CustomMatch (Compile inputFilename maybeOutputFilename) ->
            File.contentsOf inputFilename
                |> IO.exitOnError identity
                |> IO.map Opal.parse
                |> IO.exitOnError identity
                -- |> IO.map Opal.typeCheck
                -- |> IO.exitOnError identity
                |> IO.map Opal.C.compile
                |> IO.exitOnError identity
                |> IO.andThen
                    (\compiledJs ->
                        let
                            outputFilename =
                                case maybeOutputFilename of
                                    Nothing ->
                                        "opal.c"

                                    Just name ->
                                        name
                        in
                        File.writeContentsTo outputFilename compiledJs
                            |> IO.andThen (\() -> Process.print ("Compiled to " ++ outputFilename ++ "\n"))
                    )


type CliOptions
    = Init
    | Compile String (Maybe String)


cliOptions : Config CliOptions
cliOptions =
    config
        |> add
            (Cli.OptionsParser.buildSubCommand "init" Init
                |> Cli.OptionsParser.withDoc "Initialize your Opal project"
            )
        |> add
            (Cli.OptionsParser.buildSubCommand "build" Compile
                |> Cli.OptionsParser.with (Cli.Option.requiredPositionalArg "input file")
                |> Cli.OptionsParser.with (Cli.Option.optionalKeywordArg "output")
                |> Cli.OptionsParser.withDoc "Compile your Opal project"
            )



---- OPTIONS PARSER ----
-- Copied from dillonkears/elm-cli-options-parser


type RunResult match
    = SystemMessage ExitStatus String
    | CustomMatch match


type Config msg
    = Config (List (OptionsParser msg BuilderState.NoMoreOptions))


config : Config decodesTo
config =
    Config []


{-| Add an `OptionsParser` to your `Cli.Program.Config`.
-}
add : OptionsParser msg anything -> Config msg -> Config msg
add optionsParser (Config optionsParsers) =
    Config (optionsParsers ++ [ Cli.OptionsParser.end optionsParser ])


run : Config msg -> List String -> String -> RunResult msg
run (Config optionsParsers) argv versionMessage =
    let
        programName =
            case argv of
                _ :: programPath :: _ ->
                    programPath
                        |> String.split "/"
                        |> List.Extra.last
                        |> Maybe.withDefault errorMessage

                _ ->
                    errorMessage

        errorMessage =
            "TODO - show error message explaining that user needs to pass unmodified `process.argv` from node here."
    in
    case Cli.LowLevel.try optionsParsers argv of
        Cli.LowLevel.NoMatch unexpectedOptions ->
            if unexpectedOptions == [] then
                "\nUnknown command\n\nUsage:\n\n"
                    ++ Cli.LowLevel.helpText programName optionsParsers
                    |> SystemMessage Cli.ExitStatus.Failure

            else
                unexpectedOptions
                    |> List.map
                        (TypoSuggestion.toMessage
                            (optionsParsers
                                |> List.map
                                    (\optionsParser ->
                                        { usageSpecs = Cli.OptionsParser.getUsageSpecs optionsParser
                                        , subCommand = Cli.OptionsParser.getSubCommand optionsParser
                                        }
                                    )
                            )
                        )
                    |> String.join "\n"
                    |> SystemMessage Cli.ExitStatus.Failure

        Cli.LowLevel.ValidationErrors validationErrors ->
            ("Validation errors:\n\n"
                ++ (validationErrors
                        |> List.map
                            (\{ name, invalidReason } ->
                                "Invalid `--" ++ name ++ "` option.\n" ++ invalidReason
                            )
                        |> String.join "\n"
                   )
            )
                |> SystemMessage Cli.ExitStatus.Failure

        Cli.LowLevel.Match msg ->
            CustomMatch msg

        Cli.LowLevel.ShowHelp ->
            Cli.LowLevel.helpText programName optionsParsers
                |> SystemMessage Cli.ExitStatus.Success

        Cli.LowLevel.ShowVersion ->
            versionMessage
                |> SystemMessage Cli.ExitStatus.Success

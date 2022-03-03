module Opal.C exposing (compile)

import Dict
import Opal exposing (Module)
import Opal.Ast.Canonical
    exposing
        ( BinaryOperator(..)
        , Definition
        , Expression(..)
        , Literal(..)
        , UnaryOperator(..)
        )


compile : Module -> Result String String
compile module_ =
    let
        defs =
            module_.definitions
                |> List.map (simplifyDefinition >> (\def -> ( def.label, def.body )))
                |> Dict.fromList
    in
    case ( Dict.get "main" defs, Dict.toList (Dict.remove "main" defs) ) of
        ( Just mainBody, otherDefs ) ->
            """#include <stdlib.h>
#if defined(_MSC_VER)
#include "SDL.h"
#else
#include "SDL2/SDL.h"
#endif

"""
                ++ String.join "\n" (List.map compileDefinition otherDefs)
                ++ "\n"
                ++ compileDefinition ( "main", mainBody )
                ++ """

int main()
{
    if (SDL_Init(SDL_INIT_VIDEO) != 0)
    {
        printf("Failed to initialize");
        return EXIT_FAILURE;
    }

    SDL_Window *windowPtr = SDL_CreateWindow("Hello, Decal!", 100, 100, 800, 600, SDL_WINDOW_SHOWN);
    if (windowPtr == NULL)
    {
        printf("Failed to create a window");
        SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_Renderer *rendererPtr = SDL_CreateRenderer(windowPtr, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (rendererPtr == NULL)
    {
        printf("Failed to create a renderer");
        SDL_DestroyWindow(windowPtr);
        SDL_Quit();
        return EXIT_FAILURE;
    }
    printf("Should show window for 3s");

    while (1)
    {
        SDL_Event event;

        if (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
            case SDL_QUIT:
                goto quit_app;
                break;
            case SDL_KEYUP:
                switch (event.key.keysym.scancode)
                {
                case SDL_SCANCODE_ESCAPE:
                    goto quit_app;
                    break;
                default:
                    break;
                }
                break;
            default:
                break;
            }

            SDL_SetRenderDrawColor(rendererPtr, 100, 149, 237, 255);
            SDL_RenderClear(rendererPtr);
            // SDL_RenderDrawRect
            SDL_RenderPresent(rendererPtr);
        }
    }

quit_app:
    printf("Closing...");

    SDL_DestroyRenderer(rendererPtr);
    SDL_DestroyWindow(windowPtr);
    SDL_Quit();

    return EXIT_SUCCESS;
}"""
                |> Ok

        ( Nothing, _ ) ->
            Err "Missing a 'main' function"


simplifyDefinition : Definition -> Definition
simplifyDefinition definition =
    { definition | body = Opal.simplify definition.body }


compileDefinition : ( String, Expression ) -> String
compileDefinition ( label, body ) =
    case body of
        ExprAnonymousFunction args expr ->
            "void " ++ label ++ "(" ++ String.join ", " args ++ ") { return " ++ compileExpression expr ++ "; }"

        _ ->
            "var " ++ label ++ " = " ++ compileExpression body ++ ";"


compileExpression : Expression -> String
compileExpression expression =
    case expression of
        ExprLiteral (LitInt i) ->
            String.fromInt i

        ExprLiteral (LitString str) ->
            "'" ++ str ++ "'"

        ExprBinary Sum leftExpr rightExpr ->
            compileExpression leftExpr ++ " + " ++ compileExpression rightExpr

        ExprBinary Difference leftExpr rightExpr ->
            compileExpression leftExpr ++ " - " ++ compileExpression rightExpr

        ExprBinary Product leftExpr rightExpr ->
            compileExpression leftExpr ++ " * " ++ compileExpression rightExpr

        ExprBinary Quotient leftExpr rightExpr ->
            compileExpression leftExpr ++ " / " ++ compileExpression rightExpr

        ExprBinary PipedFunction _ _ ->
            "new Error('Piped functions should be reduced out')"

        ExprBinary Concat leftExpr rightExpr ->
            compileExpression leftExpr ++ " + " ++ compileExpression rightExpr

        ExprUnary Negate expr ->
            "-" ++ compileExpression expr

        ExprFunctionApplication label expressions ->
            label ++ "(" ++ String.join ", " (List.map compileExpression expressions) ++ ")"

        ExprAnonymousFunction args expr ->
            "function(" ++ String.join ", " args ++ ") { return " ++ compileExpression expr ++ "; }"

        ExprWord label ->
            label

        ExprLetIn defs expr ->
            "(function() {"
                ++ String.join "\n" (List.map (\def -> compileDefinition ( def.label, def.body )) defs)
                ++ "\n  return "
                ++ compileExpression expr
                ++ "\n})()"

        ExprIfElse conditionExpr thenExpr elseExpr ->
            "__core__ifElse(" ++ compileExpression conditionExpr ++ ", " ++ compileExpression thenExpr ++ ", " ++ compileExpression elseExpr ++ ")"

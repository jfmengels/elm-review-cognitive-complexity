module CognitiveComplexity exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ CognitiveComplexity.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-cognitive-complexity/example --rules CognitiveComplexity
```

-}
rule : Int -> Rule
rule threshold =
    Rule.newModuleRuleSchema "CognitiveComplexity" initialContext
        |> Rule.withDeclarationExitVisitor (declarationExitVisitor threshold)
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { complexity : Int
    , nesting : Int
    }


initialContext : Context
initialContext =
    { complexity = 0
    , nesting = 1
    }


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    case Node.value node of
        Expression.IfBlock _ _ _ ->
            ( [], { context | complexity = context.complexity + context.nesting } )

        _ ->
            ( [], context )


declarationExitVisitor : Int -> Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationExitVisitor threshold node context =
    let
        errors : List (Rule.Error {})
        errors =
            case Node.value node of
                Declaration.FunctionDeclaration function ->
                    reportComplexity threshold function context

                _ ->
                    []
    in
    ( errors, { context | complexity = 0 } )


reportComplexity : Int -> Expression.Function -> Context -> List (Rule.Error {})
reportComplexity threshold function context =
    let
        functionName : Node String
        functionName =
            function.declaration |> Node.value |> .name
    in
    if context.complexity > threshold then
        [ Rule.error
            { message = Node.value functionName ++ ": Cognitive complexity was " ++ String.fromInt context.complexity ++ ", higher than the allowed " ++ String.fromInt threshold
            , details = [ "REPLACEME" ]
            }
            (Node.range functionName)
        ]

    else
        []



{- TODO Ponder: How to handle let declarations

   - Expressions with lots of let declarations are/feel harder to read, so I think it should
   - increase the complexity by one, regardless of how deeply nested something is (otherwise people will move the let declaration to the root)
   - not increment the nesting value

   Is 1. inherently more complex than 2.?

    func n =
      let
        doThing1 b = blabla b + clacla b
        doThing2 b = blabla b + clacla b
        doThing3 b = blabla b + clacla b
      in
      doThing1 n + doThing2 n + doThing3 n
-}

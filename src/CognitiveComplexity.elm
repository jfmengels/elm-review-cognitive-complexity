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
    }


initialContext : Context
initialContext =
    { complexity = 0
    }


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    case Node.value node of
        Expression.IfBlock _ _ _ ->
            ( [], { context | complexity = context.complexity + 1 } )

        _ ->
            ( [], context )


declarationExitVisitor : Int -> Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationExitVisitor threshold node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            if context.complexity > threshold then
                ( [ Rule.error
                        { message = "Cognitive complexity was " ++ String.fromInt context.complexity ++ ", higher than the allowed " ++ String.fromInt threshold
                        , details = [ "REPLACEME" ]
                        }
                        (function.declaration |> Node.value |> .name |> Node.range)
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )

module CognitiveComplexity exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
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
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { complexity : Int
    , nesting : Int
    , operandsToIgnore : List Range
    }


initialContext : Context
initialContext =
    { complexity = 0
    , nesting = 1
    , operandsToIgnore = []
    }


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.IfBlock _ _ _ ->
            ( []
            , { context
                | complexity = context.complexity + context.nesting
                , nesting = context.nesting + 1
              }
            )

        Expression.CaseExpression _ ->
            ( []
            , { context
                | complexity = context.complexity + context.nesting
                , nesting = context.nesting + 1
              }
            )

        Expression.OperatorApplication operator _ left right ->
            if operator == "&&" || operator == "||" && not (List.member (Node.range node) context.operandsToIgnore) then
                let
                    ( complexity, operandsToIgnore ) =
                        incrementAndIgnoreForOperands
                            operator
                            0
                            left
                            right
                in
                ( []
                , { context
                    | complexity = context.complexity + complexity + 1
                    , operandsToIgnore = operandsToIgnore ++ context.operandsToIgnore
                  }
                )

            else
                ( [], context )

        _ ->
            ( [], context )


incrementAndIgnoreForOperands : String -> Int -> Node Expression -> Node Expression -> ( Int, List Range )
incrementAndIgnoreForOperands operator complexity left right =
    let
        ( leftComplexity, leftIgnore ) =
            incrementAndIgnore operator left

        ( rightComplexity, rightIgnore ) =
            incrementAndIgnore operator right
    in
    ( leftComplexity + rightComplexity + complexity
    , Node.range left :: Node.range right :: leftIgnore ++ rightIgnore
    )


incrementAndIgnore : String -> Node Expression -> ( Int, List Range )
incrementAndIgnore parentOperator node =
    case Node.value node of
        Expression.OperatorApplication operator _ left right ->
            if operator == "&&" || operator == "||" then
                let
                    newOperatorIncrement : Int
                    newOperatorIncrement =
                        if operator == parentOperator then
                            0

                        else
                            let
                                _ =
                                    Debug.log "node" (Node.range node)
                            in
                            1
                in
                incrementAndIgnoreForOperands
                    operator
                    newOperatorIncrement
                    left
                    right

            else
                ( 0, [] )

        _ ->
            ( 0, [] )


expressionExitVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.IfBlock _ _ _ ->
            ( []
            , { context
                | nesting = context.nesting - 1
              }
            )

        Expression.CaseExpression _ ->
            ( []
            , { context
                | nesting = context.nesting - 1
              }
            )

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
    ( errors, { complexity = 0, nesting = 1, operandsToIgnore = [] } )


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

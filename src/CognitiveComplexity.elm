module CognitiveComplexity exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports... REPLACEME

    config =
        [ CognitiveComplexity.rule 10
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
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation (finalEvaluation threshold)
        |> Rule.fromModuleRuleSchema


type alias Context =
    { nesting : Int
    , operandsToIgnore : List Range
    , elseIfToIgnore : List Range
    , increases : List Increase
    , references : Dict String Location
    , functionsToReport : List FunctionToReport
    }


type alias FunctionToReport =
    { functionName : Node String
    , increases : List Increase
    , references : Dict String Location
    }


type alias Increase =
    { line : Location
    , increase : Int
    , nesting : Int
    , kind : IncrementKind
    }


type IncrementKind
    = If
    | Case
    | Operator String
    | RecursiveCall
    | IndirectRecursiveCall String


initialContext : Context
initialContext =
    { nesting = 0
    , operandsToIgnore = []
    , elseIfToIgnore = []
    , references = Dict.empty
    , increases = []
    , functionsToReport = []
    }


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.IfBlock _ _ else_ ->
            if not (List.member (Node.range node) context.elseIfToIgnore) then
                ( []
                , { context
                    | increases =
                        { line = (Node.range node).start
                        , increase = context.nesting + 1
                        , nesting = context.nesting
                        , kind = If
                        }
                            :: context.increases
                    , nesting = context.nesting + 1
                    , elseIfToIgnore = Node.range else_ :: context.elseIfToIgnore
                  }
                )

            else
                ( [], { context | elseIfToIgnore = Node.range else_ :: context.elseIfToIgnore } )

        Expression.CaseExpression _ ->
            ( []
            , { context
                | increases =
                    { line = (Node.range node).start
                    , increase = context.nesting + 1
                    , nesting = context.nesting
                    , kind = Case
                    }
                        :: context.increases
                , nesting = context.nesting + 1
              }
            )

        Expression.OperatorApplication operator _ left right ->
            if (operator == "&&" || operator == "||") && not (List.member (Node.range node) context.operandsToIgnore) then
                let
                    ( increases, operandsToIgnore ) =
                        incrementAndIgnoreForOperands
                            operator
                            []
                            left
                            right
                in
                ( []
                , { context
                    | increases =
                        { line = (Node.range node).start
                        , increase = 1
                        , nesting = 0
                        , kind = Operator operator
                        }
                            :: increases
                            ++ context.increases
                    , operandsToIgnore = operandsToIgnore ++ context.operandsToIgnore
                  }
                )

            else
                ( [], context )

        Expression.LambdaExpression _ ->
            ( [], { context | nesting = context.nesting + 1 } )

        Expression.FunctionOrValue [] name ->
            ( []
            , { context
                | references =
                    if Dict.member name context.references then
                        -- The reference already exists, and we want to keep the first reference
                        -- for a better presentation
                        context.references

                    else
                        Dict.insert name (Node.range node).start context.references
              }
            )

        _ ->
            ( [], context )


incrementAndIgnoreForOperands : String -> List Increase -> Node Expression -> Node Expression -> ( List Increase, List Range )
incrementAndIgnoreForOperands operator increases left right =
    let
        ( leftIncreases, leftIgnore ) =
            incrementAndIgnore operator left

        ( rightIncreases, rightIgnore ) =
            incrementAndIgnore operator right
    in
    ( List.concat [ leftIncreases, rightIncreases, increases ]
    , Node.range left :: Node.range right :: leftIgnore ++ rightIgnore
    )


incrementAndIgnore : String -> Node Expression -> ( List Increase, List Range )
incrementAndIgnore parentOperator node =
    case Node.value node of
        Expression.OperatorApplication operator _ left right ->
            if operator == "&&" || operator == "||" then
                let
                    newOperatorIncrease : List Increase
                    newOperatorIncrease =
                        if operator == parentOperator then
                            []

                        else
                            [ { line = (Node.range node).start
                              , increase = 1
                              , nesting = 0
                              , kind = Operator operator
                              }
                            ]
                in
                incrementAndIgnoreForOperands
                    operator
                    newOperatorIncrease
                    left
                    right

            else
                ( [], [] )

        _ ->
            ( [], [] )


expressionExitVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.IfBlock _ _ _ ->
            if not (List.member (Node.range node) context.elseIfToIgnore) then
                ( []
                , { context
                    | nesting = context.nesting - 1
                  }
                )

            else
                ( [], context )

        Expression.CaseExpression _ ->
            ( []
            , { context
                | nesting = context.nesting - 1
              }
            )

        Expression.LambdaExpression _ ->
            ( [], { context | nesting = context.nesting - 1 } )

        _ ->
            ( [], context )


declarationExitVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationExitVisitor node context =
    let
        functionsToReport : List FunctionToReport
        functionsToReport =
            case Node.value node of
                Declaration.FunctionDeclaration function ->
                    { functionName = function.declaration |> Node.value |> .name
                    , increases = context.increases
                    , references = context.references
                    }
                        :: context.functionsToReport

                _ ->
                    context.functionsToReport
    in
    ( []
    , { nesting = 0
      , operandsToIgnore = []
      , elseIfToIgnore = []
      , references = Dict.empty
      , increases = []
      , functionsToReport = functionsToReport
      }
    )


finalEvaluation : Int -> Context -> List (Rule.Error {})
finalEvaluation threshold context =
    let
        potentialRecursiveFunctions : Set String
        potentialRecursiveFunctions =
            List.map (.functionName >> Node.value) context.functionsToReport
                |> Set.fromList

        recursiveCalls : RecursiveCalls
        recursiveCalls =
            context.functionsToReport
                |> List.map
                    (\{ functionName, references } ->
                        ( Node.value functionName, Dict.filter (\name _ -> Set.member name potentialRecursiveFunctions) references )
                    )
                |> Dict.fromList
                |> findRecursiveCalls
    in
    List.filterMap
        (\{ functionName, increases, references } ->
            let
                recursiveCallsForFunctionName : List String
                recursiveCallsForFunctionName =
                    Dict.get (Node.value functionName) recursiveCalls
                        |> Maybe.withDefault Set.empty
                        |> Set.toList

                allIncreases : List Increase
                allIncreases =
                    List.concat
                        [ increases
                        , recursiveCallsForFunctionName
                            |> List.filterMap
                                (\referenceToRecursiveFunction ->
                                    Dict.get referenceToRecursiveFunction references
                                        |> Maybe.map (Tuple.pair referenceToRecursiveFunction)
                                )
                            |> List.map
                                (\( reference, location ) ->
                                    { line = location
                                    , increase = 1
                                    , nesting = 0
                                    , kind =
                                        if Node.value functionName == reference then
                                            RecursiveCall

                                        else
                                            IndirectRecursiveCall reference
                                    }
                                )
                        ]

                finalComplexity : Int
                finalComplexity =
                    List.sum (List.map .increase allIncreases)
            in
            if finalComplexity > threshold then
                Just
                    (Rule.error
                        { message = Node.value functionName ++ " has a cognitive complexity of " ++ String.fromInt finalComplexity ++ ", higher than the allowed " ++ String.fromInt threshold
                        , details =
                            if List.isEmpty allIncreases then
                                explanation

                            else
                                explanation
                                    ++ [ allIncreases
                                            |> List.sortBy (\{ line } -> ( line.row, line.column ))
                                            |> List.map explain
                                            |> String.join "\n"
                                       ]
                        }
                        (Node.range functionName)
                    )

            else
                Nothing
        )
        context.functionsToReport


explanation : List String
explanation =
    [ "This metric is a heuristic to measure how easy to understand a piece of code is, primarily through increments for breaks in the linear flow and for nesting those breaks."
    , "The most common ways to reduce complexity is to extract section into methods and to unnest control flow structures. Following is a breakdown of where complexity was found:"
    ]


explain : Increase -> String
explain increase =
    "Line " ++ String.fromInt increase.line.row ++ ": +" ++ String.fromInt increase.increase ++ " for the " ++ kindToString increase.kind ++ mentionNesting increase.nesting


mentionNesting : Int -> String
mentionNesting nesting =
    if nesting == 0 then
        ""

    else
        " (incl " ++ String.fromInt nesting ++ " for nesting)"


kindToString : IncrementKind -> String
kindToString kind =
    case kind of
        If ->
            "if expression"

        Case ->
            "case expression"

        Operator operator ->
            "use of " ++ operator

        RecursiveCall ->
            "recursive call"

        IndirectRecursiveCall fnName ->
            "indirect recursive call to " ++ fnName



-- FINDING RECURSIVE FUNCTIONS
-- Inspired by the algorithm found at https://www.baeldung.com/cs/detecting-recursiveCalls-in-directed-graph


type alias RecursiveCalls =
    Dict String (Set String)


type alias Visited =
    Dict String VisitState


type VisitState
    = InStack
    | Done


findRecursiveCalls : Dict String (Dict String a) -> RecursiveCalls
findRecursiveCalls graph =
    graph
        |> Dict.keys
        |> List.foldl
            (\vertice ( recursiveCalls, visited ) ->
                let
                    res : { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
                    res =
                        processDFSTree
                            graph
                            [ vertice ]
                            (Dict.insert vertice InStack visited)
                in
                ( mergeRecursiveCallsDict res.recursiveCalls recursiveCalls, res.visited )
            )
            ( Dict.empty, Dict.empty )
        |> Tuple.first


mergeRecursiveCallsDict : RecursiveCalls -> RecursiveCalls -> RecursiveCalls
mergeRecursiveCallsDict left right =
    Dict.merge
        (\functionName calls dict -> Dict.insert functionName calls dict)
        (\functionName callsLeft callsRight dict -> Dict.insert functionName (Set.union callsLeft callsRight) dict)
        (\functionName calls dict -> Dict.insert functionName calls dict)
        left
        right
        Dict.empty


processDFSTree : Dict String (Dict String a) -> List String -> Visited -> { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
processDFSTree graph stack visited =
    let
        vertices : List String
        vertices =
            List.head stack
                |> Maybe.andThen (\v -> Dict.get v graph)
                |> Maybe.withDefault Dict.empty
                |> Dict.keys
    in
    List.foldl
        (\vertice acc ->
            case Dict.get vertice visited of
                Just InStack ->
                    { acc | recursiveCalls = insertCycle stack vertice acc.recursiveCalls }

                Just Done ->
                    acc

                Nothing ->
                    let
                        res : { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
                        res =
                            processDFSTree
                                graph
                                (vertice :: stack)
                                (Dict.insert vertice InStack visited)
                    in
                    { recursiveCalls = mergeRecursiveCallsDict res.recursiveCalls acc.recursiveCalls, visited = res.visited }
        )
        { recursiveCalls = Dict.empty, visited = visited }
        vertices
        |> (\res ->
                { recursiveCalls = res.recursiveCalls
                , visited =
                    List.head stack
                        |> Maybe.map (\v -> Dict.insert v Done res.visited)
                        |> Maybe.withDefault res.visited
                , stack = List.drop 1 stack
                }
           )


insertCycle : List String -> String -> RecursiveCalls -> RecursiveCalls
insertCycle stack vertice recursiveCalls =
    case stack of
        x :: xs ->
            List.foldl
                (\( functionName, reference ) acc ->
                    Dict.update
                        functionName
                        (Maybe.withDefault Set.empty >> Set.insert reference >> Just)
                        acc
                )
                recursiveCalls
                (takeTop xs ( x, [] ) vertice
                    |> toTuples x
                )

        [] ->
            recursiveCalls


toTuples : String -> ( String, List String ) -> List ( String, String )
toTuples x ( first, xs ) =
    ( x, first )
        :: (case xs of
                [] ->
                    []

                firstofXs :: restOfXs ->
                    toTuples first ( firstofXs, restOfXs )
           )


takeTop : List String -> ( String, List String ) -> String -> ( String, List String )
takeTop stack ( previousValue, previousValues ) stopValue =
    case stack of
        [] ->
            ( previousValue, previousValues )

        x :: xs ->
            if x /= stopValue then
                takeTop xs ( x, previousValue :: previousValues ) stopValue

            else
                ( x, previousValue :: previousValues )



{- TODO Add error details explaining how to simplify
   - Collapse conditions
   - Extract to methods
-}

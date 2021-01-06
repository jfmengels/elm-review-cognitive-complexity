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
            ( [], { context | references = Dict.insert name (Node.range node).start context.references } )

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
        (\{ functionName, increases } ->
            let
                allIncreases : List Increase
                allIncreases =
                    List.concat
                        [ increases
                        , Dict.get (Node.value functionName) recursiveCalls
                            |> Maybe.map Dict.toList
                            |> Maybe.withDefault []
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
                                [ "REPLACEME" ]

                            else
                                [ "REPLACEME"
                                , allIncreases
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
-- Algorithm found at https://www.baeldung.com/cs/detecting-recursiveCalls-in-directed-graph


type alias RecursiveCalls =
    Dict String (Dict String Location)


type alias Visited =
    Dict String VisitState


type VisitState
    = InStack
    | Done


findRecursiveCalls : Dict String (Dict String Location) -> RecursiveCalls
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
        (\functionName callsLeft callsRight dict -> Dict.insert functionName (Dict.union callsLeft callsRight) dict)
        (\functionName calls dict -> Dict.insert functionName calls dict)
        left
        right
        Dict.empty


processDFSTree : Dict String (Dict String Location) -> List String -> Visited -> { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
processDFSTree graph stack visited =
    let
        vertices : List ( String, Location )
        vertices =
            List.head stack
                |> Maybe.andThen (\v -> Dict.get v graph)
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
    in
    List.foldl
        (\( vertice, location ) acc ->
            case Dict.get vertice visited of
                Just InStack ->
                    { acc | recursiveCalls = insertCycle stack ( vertice, location ) acc.recursiveCalls }

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


insertCycle : List String -> ( String, Location ) -> RecursiveCalls -> RecursiveCalls
insertCycle stack ( vertice, location ) recursiveCalls =
    case stack of
        x :: xs ->
            List.foldl
                (\( functionName, reference ) acc ->
                    Dict.update
                        functionName
                        (Maybe.withDefault Dict.empty >> Dict.insert reference location >> Just)
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
-- TODO Add explanations of how complexity was increased at every step
-- TODO Add error details explaining how to simplify
-- TODO Add documentation

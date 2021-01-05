module CognitiveComplexity exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


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
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation (finalEvaluation threshold)
        |> Rule.fromModuleRuleSchema


type alias Context =
    { complexity : Int
    , nesting : Int
    , operandsToIgnore : List Range
    , references : Set String
    , functionsToReport : List FunctionToReport
    }


type alias FunctionToReport =
    { functionName : Node String
    , complexity : Int
    , references : Set String
    }


initialContext : Context
initialContext =
    { complexity = 0
    , nesting = 1
    , operandsToIgnore = []
    , references = Set.empty
    , functionsToReport = []
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
            if (operator == "&&" || operator == "||") && not (List.member (Node.range node) context.operandsToIgnore) then
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

        Expression.FunctionOrValue [] name ->
            ( [], { context | references = Set.insert name context.references } )

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


declarationExitVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationExitVisitor node context =
    let
        functionsToReport : List FunctionToReport
        functionsToReport =
            case Node.value node of
                Declaration.FunctionDeclaration function ->
                    { functionName = function.declaration |> Node.value |> .name
                    , complexity = context.complexity
                    , references = context.references
                    }
                        :: context.functionsToReport

                _ ->
                    context.functionsToReport
    in
    ( []
    , { complexity = 0
      , nesting = 1
      , operandsToIgnore = []
      , references = Set.empty
      , functionsToReport = functionsToReport
      }
    )


finalEvaluation : Int -> Context -> List (Rule.Error {})
finalEvaluation threshold context =
    let
        callsToRecursiveFunctions : Dict String Int
        callsToRecursiveFunctions =
            context.functionsToReport
                |> List.map
                    (\{ functionName, references } ->
                        ( Node.value functionName, references )
                    )
                |> Dict.fromList
                |> findRecursiveCalls
                |> Dict.map (\_ recursiveFunctions -> Set.size recursiveFunctions)

        --Dict.fromList [ ( "fib", 1 ), ( "fun1", 1 ), ( "fun2", 1 ) ]
    in
    context.functionsToReport
        --|> List.map
        --    (\{ functionName, complexity, references } ->
        --        { functionName = functionName
        --        , complexity = complexity + List.length (List.filter (\set -> Set.member (Node.value functionName) set) callGraph)
        --        }
        --    )
        |> List.filterMap
            (\{ functionName, complexity } ->
                let
                    finalComplexity : Int
                    finalComplexity =
                        complexity + (Dict.get (Node.value functionName) callsToRecursiveFunctions |> Maybe.withDefault 0)
                in
                if finalComplexity > threshold then
                    Just
                        (Rule.error
                            { message = Node.value functionName ++ ": Cognitive complexity was " ++ String.fromInt finalComplexity ++ ", higher than the allowed " ++ String.fromInt threshold
                            , details = [ "REPLACEME" ]
                            }
                            (Node.range functionName)
                        )

                else
                    Nothing
            )



-- FINDING RECURSIVE FUNCTIONS
-- Algorithm found at https://www.baeldung.com/cs/detecting-recursiveCalls-in-directed-graph


type alias RecursiveCalls =
    Dict String (Set String)


type alias Visited =
    Dict String VisitState


type VisitState
    = InStack
    | Done


findRecursiveCalls : Dict String (Set String) -> RecursiveCalls
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


processDFSTree : Dict String (Set String) -> List String -> Visited -> { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
processDFSTree graph stack visited =
    let
        vertices : List String
        vertices =
            List.head stack
                |> Maybe.andThen (\v -> Dict.get v graph)
                |> Maybe.withDefault Set.empty
                |> Set.toList
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
                        res =
                            processDFSTree
                                graph
                                (vertice :: stack)
                                (Dict.insert vertice InStack visited)
                    in
                    { recursiveCalls = res.recursiveCalls, visited = res.visited }
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


insertCycle : List String -> String -> Dict String (Set String) -> Dict String (Set String)
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
                (takeTop xs x vertice)

        [] ->
            recursiveCalls


takeTop : List String -> String -> String -> List ( String, String )
takeTop stack previousValue stopValue =
    case stack of
        [] ->
            []

        x :: xs ->
            if x /= stopValue then
                ( previousValue, x ) :: takeTop xs x stopValue

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
-- TODO Don't increment for else if
-- TODO Increment nesting level in lambda

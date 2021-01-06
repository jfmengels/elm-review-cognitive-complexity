module CognitiveComplexityTest exposing (all)

import CognitiveComplexity exposing (rule)
import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "CognitiveComplexity"
        [ test "should not report an error when the complexity is lower than the threshold" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule 1)
                    |> Review.Test.expectNoErrors
        , test "should report an error when the complexity is higher than the threshold" <|
            \() ->
                """module A exposing (..)
fun n =
    if cond then        -- +1
      if cond then      -- +2
        if cond then    -- +3
          if cond then  -- +4
            1
          else
            2
        else
          2
      else
        2
    else
      2
"""
                    |> Review.Test.run (rule -1)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "fun has a cognitive complexity of 10, higher than the allowed -1"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                        ]
        , test "should count a simple value or operation as 0" <|
            \() ->
                """module A exposing (..)
fun n =
    n + 1
"""
                    |> expect [ { name = "fun", complexity = 0, details = [ "REPLACEME" ] } ]
        , test "should count if expression as 1" <|
            \() ->
                """module A exposing (..)
fun n =
    if cond then
      1
    else
      2
"""
                    |> expect
                        [ { name = "fun"
                          , complexity = 1
                          , details =
                                [ "REPLACEME"
                                , String.trim """
Line 3: +1 for the if expression
"""
                                ]
                          }
                        ]
        , test "should not count if else expressions" <|
            \() ->
                """module A exposing (..)
fun n =
    if cond then        -- +1
      1
    else if cond then   -- +0
      2
    else
      2
"""
                    |> expectComplexity [ ( "fun", 1 ) ]
        , test "should properly decrement when exiting else expression" <|
            \() ->
                """module A exposing (..)
fun n =
  let
    _ =
      if cond then        -- +1
        1
      else if cond then   -- +0
        2
      else
        3
  in
  if cond then            -- +1
    4
  else
    5
"""
                    |> expectComplexity [ ( "fun", 2 ) ]
        , test "should count case expression as 1" <|
            \() ->
                """module A exposing (..)
fun n =
    case n of   -- +1
      1 -> ()
      2 -> ()
      3 -> ()
      4 -> ()
      _ -> ()
"""
                    |> expectComplexity [ ( "fun", 1 ) ]
        , test "should count nesting of case expressions" <|
            \() ->
                """module A exposing (..)
fun n =
    case n of           -- +1
      1 -> ()
      2 -> ()
      3 -> case n of    -- +2
            _ -> ()
      4 -> ()
      _ -> ()
"""
                    |> expectComplexity [ ( "fun", 3 ) ]
        , test "should decrement the nesting when leaving a nested structure" <|
            \() ->
                """module A exposing (..)
fun n =
    if cond then        -- +1
      if cond then      -- +2
        if cond then    -- +3
          1
        else
          2
      else
        2
    else
      case n of         -- +2
        () -> ()
"""
                    |> expectComplexity [ ( "fun", 8 ) ]
        , test "should increment once when using the && boolean operator" <|
            \() ->
                """module A exposing (..)
fun n =
    if                      -- +1
        a && b && c && d    -- +1 for the usage of &&
    then
        1
    else
        2
"""
                    |> expectComplexity [ ( "fun", 2 ) ]
        , test "should increment once when using the || boolean operator" <|
            \() ->
                """module A exposing (..)
fun n =
    if                      -- +1
        a || b || c || d    -- +1 for the usage of ||
    then
        1
    else
        2
"""
                    |> expectComplexity [ ( "fun", 2 ) ]
        , test "should increment when mixing boolean operators" <|
            \() ->
                """module A exposing (..)
fun n =
    if                  -- +1
        a && b && c     -- +1
        || d || e       -- +1
        && f            -- +1
    then
        1
    else
        2
"""
                    |> expectComplexity [ ( "fun", 4 ) ]
        , test "should not increment for anonymous functions" <|
            \() ->
                """module A exposing (..)
fun n =
  List.map (\\m -> m + 1) n
"""
                    |> expectComplexity [ ( "fun", 0 ) ]
        , test "should increment the nesting inside anonymous functions" <|
            \() ->
                """module A exposing (..)
fun n =
    List.map
        (\\m ->
            if cond then    -- +2
                1

            else
                2
        )
        n
"""
                    |> expectComplexity [ ( "fun", 2 ) ]
        , test "should properly decrement the nesting when exiting an anonymous function" <|
            \() ->
                """module A exposing (..)
fun n =
    let
        _ = List.map (\\m -> m + 1) n
    in
    if cond then    -- +1
        1
    else
        2
"""
                    |> expectComplexity [ ( "fun", 1 ) ]
        , test "should increment when find a recursive call" <|
            \() ->
                """module A exposing (..)
fun n =
    if n > 0 then           -- +1
        1 + fun (n - 1)     -- +1
    else
        1
"""
                    |> expectComplexityAt [ ( "fun", 2, { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } } ) ]
        , test "should only increment once, even if there are multiple recursive calls" <|
            \() ->
                """module A exposing (..)
fib n =
    if n > 0 then           -- +1
        fib (n - 1)         -- +1
        + fib (n - 2)       -- +0
    else
        0
"""
                    |> expectComplexityAt [ ( "fib", 2, { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } } ) ]
        , test "should increment the complexity for every recursive call in a chain" <|
            \() ->
                """module A exposing (..)
fun1 n =    -- +1
  fun2 n

fun2 n =    -- +1
  fun1 n
"""
                    |> expectComplexityAt
                        [ ( "fun1", 1, { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } } )
                        , ( "fun2", 1, { start = { row = 5, column = 1 }, end = { row = 5, column = 5 } } )
                        ]
        , test "should increment the complexity for every recursive call in a chain, for each different function call" <|
            \() ->
                """module A exposing (..)
fun1 n =
  fun2 n    -- +1
  + fun2 n  -- +0, already counted
  + fun1 n  -- +1

fun2 n =
  fun1 n    -- +1
"""
                    |> expectComplexityAt
                        [ ( "fun1", 2, { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } } )
                        , ( "fun2", 1, { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } )
                        ]
        , test "should increment the complexity for every recursive call in a chain, for long chains" <|
            \() ->
                """module A exposing (..)
fun1 n =
  fun2 n    -- +1
fun2 n =
  fun3 n    -- +1
fun3 n =
  fun4 n    -- +1
fun4 n =
  fun5 n    -- +1
fun5 n =
  fun1 n    -- +1
"""
                    |> expectComplexityAt
                        [ ( "fun1", 1, { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } } )
                        , ( "fun2", 1, { start = { row = 4, column = 1 }, end = { row = 4, column = 5 } } )
                        , ( "fun3", 1, { start = { row = 6, column = 1 }, end = { row = 6, column = 5 } } )
                        , ( "fun4", 1, { start = { row = 8, column = 1 }, end = { row = 8, column = 5 } } )
                        , ( "fun5", 1, { start = { row = 10, column = 1 }, end = { row = 10, column = 5 } } )
                        ]
        , test "the complexity of a function should not affect another function's computed complexity" <|
            \() ->
                """module A exposing (..)
simple n = 1

fun n =
    if cond then
        if cond then
          1
        else
          2
    else
      2

alsoSimple n =
    if cond then
      1
    else
      2
"""
                    |> expectComplexity
                        [ ( "simple", 0 )
                        , ( "fun", 3 )
                        , ( "alsoSimple", 1 )
                        ]
        ]


expectComplexity : List ( String, Int ) -> String -> Expectation
expectComplexity functionComplexities source =
    source
        |> Review.Test.run (rule -1)
        |> Review.Test.expectErrors
            (List.map
                (\( fnName, expected ) ->
                    Review.Test.error
                        { message = fnName ++ " has a cognitive complexity of " ++ String.fromInt expected ++ ", higher than the allowed -1"
                        , details = [ "REPLACEME" ]
                        , under = fnName
                        }
                )
                functionComplexities
            )


expect : List { name : String, complexity : Int, details : List String } -> String -> Expectation
expect functionComplexities source =
    source
        |> Review.Test.run (rule -1)
        |> Review.Test.expectErrors
            (List.map
                (\{ name, complexity, details } ->
                    Review.Test.error
                        { message = name ++ " has a cognitive complexity of " ++ String.fromInt complexity ++ ", higher than the allowed -1"
                        , details = details
                        , under = name
                        }
                )
                functionComplexities
            )


expectComplexityAt : List ( String, Int, Range ) -> String -> Expectation
expectComplexityAt functionComplexities source =
    source
        |> Review.Test.run (rule -1)
        |> Review.Test.expectErrors
            (List.map
                (\( fnName, expected, atExactly ) ->
                    Review.Test.error
                        { message = fnName ++ " has a cognitive complexity of " ++ String.fromInt expected ++ ", higher than the allowed -1"
                        , details = [ "REPLACEME" ]
                        , under = fnName
                        }
                        |> Review.Test.atExactly atExactly
                )
                functionComplexities
            )

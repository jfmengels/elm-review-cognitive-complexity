module CognitiveComplexityTest exposing (all)

import CognitiveComplexity exposing (rule)
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
                            { message = "fun: Cognitive complexity was 10, higher than the allowed -1"
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
                    |> expectComplexity [ ( "fun", 0 ) ]
        , test "should count if expression as 1" <|
            \() ->
                """module A exposing (..)
fun n =
    if cond then
      1
    else
      2
"""
                    |> expectComplexity [ ( "fun", 1 ) ]
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
        , test "should increment when find a recursive call" <|
            \() ->
                """module A exposing (..)
fun n =
    if n > 0 then           -- +1
        1 + fun (n - 1)     -- +1
    else
        1
"""
                    |> expectComplexity [ ( "fun", 2 ) ]
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
                    |> expectComplexity [ ( "fib", 2 ) ]
        , test "should increment the complexity for every recursive call in a chain" <|
            \() ->
                """module A exposing (..)
fun1 n =    -- +1
  fun2 n

fun2 n =    -- +1
  fun1 n
"""
                    |> expectComplexity [ ( "fun1", 1 ), ( "fun2", 1 ) ]
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
                        { message = fnName ++ ": Cognitive complexity was " ++ String.fromInt expected ++ ", higher than the allowed -1"
                        , details = [ "REPLACEME" ]
                        , under = fnName
                        }
                )
                functionComplexities
            )

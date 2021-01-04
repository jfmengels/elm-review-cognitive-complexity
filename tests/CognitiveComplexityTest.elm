module CognitiveComplexityTest exposing (all)

import CognitiveComplexity exposing (rule)
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
    if cond then
      if cond then
        if cond then
          if cond then
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
                            { message = "fun: Cognitive complexity was 4, higher than the allowed -1"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                        ]
        , test "should count if expression as 1" <|
            \() ->
                """module A exposing (..)
fun n =
    if cond then
      1
    else
      2
"""
                    |> Review.Test.run (rule -1)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "fun: Cognitive complexity was 1, higher than the allowed -1"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
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
                    |> Review.Test.run (rule 1)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "fun: Cognitive complexity was 2, higher than the allowed 1"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                        ]
        ]

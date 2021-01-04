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
                            { message = "Cognitive complexity was 0, higher than the allowed -1"
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
                            { message = "Cognitive complexity was 1, higher than the allowed -1"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                        ]
        ]

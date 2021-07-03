# elm-review-cognitive-complexity

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to measure the cognitive complexity of a function.


## Provided rules

- [`CognitiveComplexity`](https://package.elm-lang.org/packages/jfmengels/elm-review-cognitive-complexity/1.0.0/CognitiveComplexity) - Reports functions that have a too high cognitive complexity.


## Configuration

```elm
module ReviewConfig exposing (config)

import CognitiveComplexity
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ CognitiveComplexity.rule 15
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-cognitive-complexity/preview
```

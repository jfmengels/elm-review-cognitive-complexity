# elm-review-cognitive-complexity

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`CognitiveComplexity`](https://package.elm-lang.org/packages/jfmengels/elm-review-cognitive-complexity/1.0.0/CognitiveComplexity) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import CognitiveComplexity
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ CognitiveComplexity.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-cognitive-complexity/example
```

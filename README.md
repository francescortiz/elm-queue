# elm-triplet

**WARNING: Unpublished. Use [elm-tuple-extra](https://package.elm-lang.org/packages/TSFoster/elm-tuple-extra/latest/)**

3-tuple functions aligned with elm/core 2-tuple ones.

```elm

type alias Person =
    { name : String
    , age : Int
    , alive : Bool
    }

data =
    ("Mahatma Gandhi", 78, False)

gandhi =
    data
        |> Triplet.applyTo Person

gandhiName =
    data
        |> Triplet.first

gandhiAge =
    data
        |> Triplet.second

gandhiAlive =
    data
        |> Triplet.third

```

Since elm does not allow to have tuples with more than three elements it
makes sense to handle the 3-tuple with specific functions like the 2-tuple is
handled in the [elm/core](https://package.elm-lang.org/packages/elm/core/latest/Tuple) package.

Check the [full docs](https://package.elm-lang.org/packages/francescortiz/triplet/latest/Triplet).

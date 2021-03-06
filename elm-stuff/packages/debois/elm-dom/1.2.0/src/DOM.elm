module DOM
  ( target, offsetParent, parentElement
  , nextSibling, previousSibling
  , childNode, childNodes
  , offsetWidth, offsetHeight
  , offsetLeft, offsetTop
  , scrollLeft, scrollTop
  , Rectangle, boundingClientRect
  , className
  ) where

{-| You read values off the DOM by constructing a JSON decoder.
See the `target` value for example use.

# Traversing the DOM
@docs target, offsetParent, parentElement, nextSibling, previousSibling, childNode, childNodes

# Geometry
Decoders for reading sizing etc. properties off the DOM. All decoders return
measurements in pixels.

Refer to, e.g.,
[the Mozilla documentation](https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Determining_the_dimensions_of_elements)
for the precise semantics of these measurements. See also
[this stackoverflow answer](https://stackoverflow.com/questions/294250/how-do-i-retrieve-an-html-elements-actual-width-and-height).

@docs offsetWidth, offsetHeight
@docs offsetLeft, offsetTop
@docs Rectangle, boundingClientRect

# Scroll
@docs scrollLeft, scrollTop

# Miscellanous
@docs className
-}

import Json.Decode as Decode exposing ((:=), at, andThen, Decoder)


{-| Get the target DOM element of an event. You will usually start with this
decoder. E.g., to make a button which when clicked emit an Action that carries
the width of the button:

    import DOM exposing (target, offsetWidth)

    type Action = Click Float

    myButton : Signal.Address Action -> Html
    myButton addr =
      button
        [ on "click" (target offsetWidth) (Click >> Signal.message addr) ]
        [ text "Click me!" ]
-}
target : Decoder a -> Decoder a
target decoder =
  "target" := decoder


{-| Get the offsetParent of the current element. Returns first argument if the current
element is already the root; applies the second argument to the parent element
if not.

To do traversals of the DOM, exploit that Elm allows recursive values. 
-}
offsetParent : a -> Decoder a -> Decoder a
offsetParent x decoder =
  Decode.oneOf
  [ "offsetParent" := Decode.null x
  , "offsetParent" := decoder
  ]


{-| Get the next sibling of an element.
-}
nextSibling : Decoder a -> Decoder a
nextSibling decoder =
  "nextSibling" := decoder


{-| Get the previous sibling of an element.
-}
previousSibling : Decoder a -> Decoder a
previousSibling decoder =
  "previousSibling" := decoder


{-| Get the parent of an element. 
-}
parentElement : Decoder a -> Decoder a
parentElement decoder = 
  "parentElement" := decoder


{-| Find the ith child of an element. 
-}
childNode : Int -> Decoder a -> Decoder a
childNode idx = 
  at [ "childNodes", toString idx ]


{-| Get the children of an element.
-}
childNodes : Decoder a -> Decoder (List a)
childNodes decoder = 
  let
    loop idx xs = 
      Decode.maybe (toString idx := decoder)
        `andThen` (
          Maybe.map (\x -> loop (idx+1) (x::xs))
            >> Maybe.withDefault (Decode.succeed xs)
        )
  in
    ("childNodes" := loop 0 [])
      |> Decode.map List.reverse




-- GEOMETRY 


{-| Get the width of an element in pixels; underlying implementation
reads `.offsetWidth`.
-}
offsetWidth : Decoder Float
offsetWidth = "offsetWidth" := Decode.float


{-| Get the heigh of an element in pixels. Underlying implementation
reads `.offsetHeight`.
-}
offsetHeight : Decoder Float
offsetHeight = "offsetHeight" := Decode.float


{-| Get the left-offset of the element in the parent element in pixels.
Underlying implementation reads `.offsetLeft`.
-}
offsetLeft : Decoder Float
offsetLeft = "offsetLeft" := Decode.float


{-| Get the top-offset of the element in the parent element in pixels.
Underlying implementation reads `.offsetTop`.
-}
offsetTop : Decoder Float
offsetTop = "offsetTop" := Decode.float


{-| Get the amount of left scroll of the element in pixels.
Underlying implementation reads `.scrollLeft`.
-}
scrollLeft : Decoder Float
scrollLeft = "scrollLeft" := Decode.float


{-| Get the amount of top scroll of the element in pixels.
Underlying implementation reads `.scrollTop`.
-}
scrollTop : Decoder Float
scrollTop = "scrollTop" := Decode.float


{-| Types for rectangles.
-}
type alias Rectangle =
  { top : Float
  , left : Float
  , width : Float
  , height : Float
  }


{-| Approximation of the method
[getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XPCOM/Reference/Floaterface/nsIDOMClientRect),
based off
[this stackoverflow answer](https://stackoverflow.com/questions/442404/retrieve-the-position-x-y-of-an-html-element).

NB! This decoder is likely computationally expensive and may produce results
that differ slightly from `getBoundingClientRect` in browser-dependent ways.

(I don't get to call getBoundingClientRect directly from Elm without going
native or using ports; my packages don't get to go native and I can find no
solution with ports. So we do it like in the bad old days with an O(lg n)
traversal of the DOM, only now through presumably expensive JSON decoders.
It's 2007 forever, baby!)
-}
boundingClientRect : Decoder Rectangle
boundingClientRect =
  Decode.object3
    (\(x, y) width height ->
      { top = y
      , left = x
      , width = width
      , height = height
      })
    (position 0 0)
    offsetWidth
    offsetHeight


{- This is what we're implementing (from the above link).

    function getOffset( el ) {
        var _x = 0;
        var _y = 0;
        while( el && !isNaN( el.offsetLeft ) && !isNaN( el.offsetTop ) ) {
            _x += el.offsetLeft - el.scrollLeft;
            _y += el.offsetTop - el.scrollTop;
            el = el.offsetParent;
        }
        return { top: _y, left: _x };
    }
    var x = getOffset( document.getElementById('yourElId') ).left; )
-}
position : Float -> Float -> Decoder (Float, Float)
position x y =
  Decode.object4
    (\scrollLeft scrollTop offsetLeft offsetTop ->
      (x + offsetLeft - scrollLeft, y + offsetTop - scrollTop))
    scrollLeft
    scrollTop
    offsetLeft
    offsetTop
  `andThen` (\(x',y') ->
    offsetParent (x', y') (position x' y')
  )


-- MISC


{-| Get the class name(s) of an element.
-}
className : Decoder String
className = 
  at [ "className" ] Decode.string



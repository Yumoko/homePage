import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

import Json.Decode as Json exposing (Decoder)

import DOM exposing (..)


type alias Model = 
  () -> List Float


type Action 
  = Measure Json.Value
  | NoOp


init : (Model, Effects Action)
init = (always [], Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Measure val -> 
      ((\_ -> 
        Json.decodeValue decode val
          |> Result.toMaybe
          |> Maybe.withDefault [] 
       )
      , Effects.none)

    NoOp -> 
      (model, Effects.none)


-- VIEW


infixr 5 :>
(:>) : (a -> b) -> a -> b
(:>) f x = 
  f x


decode : Decoder (List Float)
decode = 
  DOM.target                    -- (a)
  :> parentElement              -- (b)
  :> childNode 0                -- (c)
  :> childNode 0                -- (d)
  :> childNodes                 -- (e)
       DOM.offsetHeight          -- read the width of each element


css : Attribute
css = 
  style [ ("padding", "1em") ]


view : Address Action -> Model -> Html
view addr model = 
  div -- parentElement (b)
    []
    [ div -- childNode 0 (c)
        [ css ]
        [ div -- childNode 0 (d)
            []
            [ span [ css ] [ text "short" ] 
            , span [ css ] [ text "somewhat long" ] 
            , span [ css ] [ text "longer than the others" ]
            , span [ css ] [ text "much longer than the others" ]
            ] -- childNodes (e)
        ]
    , button -- target (a)
        [ css
        , on "click" Json.value (Measure >> Signal.message addr)
        ]
        [ text "Measure!" ]
    , button 
        [ css 
        , onClick addr NoOp 
        ]
        [ text "Call the function."
        ]
    , div 
        [ css ]
        [ model ()
          |> List.map toString
          |> String.join ", "
          |> text 
        , text "!"
        ]
    ]


-- STARTAPP


app : StartApp.App Model
app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks



module HomePage where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp as StartApp
import Effects exposing (..)
import List exposing (..)
import String exposing (words, join, cons, uncons)
import Char
import Dict exposing (..)
import Lightbox exposing (Picture, defPic, picList)
import Gallery exposing (..)
import Time exposing (..)
import Task exposing (..)
import Gallery

-- Model
initialModel = []

-- View
view address model = page

-- Update

type Action = 
   NoOp 
 | GalleryAction String (Gallery.Action)

update action model = (model,none)

--Main

timer g = Signal.map (\_ -> GalleryAction (snd g) Gallery.TimeStep) (every (3*second))

timers = List.map timer galleries

app =
    StartApp.start
          { init = (initialModel, none)
          , view = view
          , update = update
          , inputs = timers
          }

main =
    app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


-- Data

galleries = []

page = 
  div [id "page"]
      [ header'
      , gallery
      , about
      , footer' 
      ]

header' = 
    header []
           [ h1  [] [text "Website's Title"]
           , div [ id "coverImg"] []
           , nav []
                 [ ul []
                      [ li [] [a [href "#coverImg"] [text "Top"]]
                      , li [] [a [href "#gallery"] [text "Gallery"]]
                      , li [] [a [href "#about"] [text "About"]]
                      ]
                 ]
           ]

gallery = 
  div [ id "gallery"]
      [ h2 [] [ text "Gallery"]
      , div [ class "entry"]
            [ figure []
                     [ img [src "images/WatercolourS.jpg"] []
                     , figcaption [] [ text "Watercolour"]
                     ]
            ]
      , div [ class "entry"]
            [ figure []
                     [ img [src "images/SketchingS.jpg"] []
                     , figcaption [] [ text "Drawing and Sketching"]
                     ]
            ]
      , div [ class "entry"]
            [ figure []
                     [ img [src "images/DigitalS.jpg"] []
                     , figcaption [] [ text "Digital painting"]
                     ]
            ]
      , div [ class "entry"]
            [ figure []
                     [ img [src "images/OthersS.jpg"] []
                     , figcaption [] [ text "Others"]
                     ]
            ]
      ]

about = 
  div [ id "about"]
      [ h2 [] [text "About"]
      , div []
            [ p [] [ text "Born in northeast Taiwan, ..."]
            ]
      ]

footer' = 
  footer []
         [ p [] [text "Yu momoko"]
         ]
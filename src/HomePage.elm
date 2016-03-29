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
import Listing exposing (..)
import Gallery

-- Model
type alias Model = 
  { current  : State
  , picMap   : Dict String (Lightbox.Model) 
  }

type State = Menu | Unfolded Category

type Category = 
    Digital
  | Sketching
  | Watercolour
  | Others

initialModel = 
  let toPics xs folder = 
        Lightbox.init (List.map (\filename -> { defPic | filename = filename}) xs)
                      folder
  in 
  Model Menu
       (Dict.fromList [ ("Digital", toPics digital "digital")
                      , ("Sketching", toPics sketching "sketching")
                      , ("Watercolour", toPics watercolour "watercolour")
                      , ("Others", toPics others "others")
                      ]
       )

-- View
view address model = page address model

-- Update

type Action = 
   NoOp
 | Open Category
 | Close 
 | LightboxAction (Lightbox.Action)

update : Action -> Model -> (Model, Effects Action) 
update action model = 
  case action of
    NoOp     -> (model,none)
    Open cat -> ({ model | current = Unfolded cat } , none)
    Close    -> ({ model | current = Menu }, none)
    
    LightboxAction act -> 
      let {current, picMap} = model
      in
      case Dict.get (toString current) picMap of 
        Nothing -> (model,none)
        Just lightbox ->
          let lightbox' = Lightbox.update act lightbox
          in
          ({model | picMap = Dict.insert (toString current) lightbox' picMap},none) 

toString : State -> String 
toString state = 
  case state of
    Menu -> "Menu"
    Unfolded cat ->
      case cat of  
        Digital     -> "Digital"
        Sketching   -> "Sketching"
        Watercolour -> "Watercolour"
        Others      -> "Others"

--Main

--timer g = Signal.map (\_ -> GalleryAction (snd g) Gallery.TimeStep) (every (3*second))

--timers = List.map timer galleries

port locationSearch : String

app =
    StartApp.start
          { init = (initialModel, none)
          , view = view
          , update = update
          , inputs = []
          }

main =
    app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


-- Data

galleries = []


page address model = 
  div [id "page"]
      [ header'
      , gallery address model
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

gallery address {current, picMap} = 
  case current of 
    Menu -> 
      div [ id "gallery"]
          [ h2 [] [ text "Gallery"]
          , div [ class "entry"]
                [ figure [ onClick address (Open Watercolour)]
                         [ img [src "images/WatercolourS.jpg"] []
                         , figcaption [] [ text "Watercolour"]
                         ]
                ]
          , div [ class "entry"]
                [ figure [ onClick address (Open Sketching)] 
                         [ img [src "images/SketchingS.jpg"] []
                         , figcaption [] [ text "Drawing and Sketching"]
                         ]
                ]
          , div [ class "entry"]
                [ figure [ onClick address (Open Digital)]
                         [ img [src "images/DigitalS.jpg"] []
                         , figcaption [] [ text "Digital painting"]
                         ]
                ]
          , div [ class "entry"]
                [ figure [ onClick address (Open Others)]
                         [ img [src "images/OthersS.jpg"] []
                         , figcaption [] [ text "Others"]
                         ]
                ]
          ]
    Unfolded cat ->
       case Dict.get (toString current) picMap of 
        Nothing -> div []
                       [ a [ onClick address Close]
                           [ text "Back to menu"]
                       ]

        Just lightbox ->
          let lightbox' = 
                Lightbox.view (Signal.forwardTo address LightboxAction) 
                              lightbox
          in      
              div []
                  [ lightbox'
                  , a [ onClick address Close]
                      [ text "Back to menu"]
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
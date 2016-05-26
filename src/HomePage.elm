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
import Lightbox exposing (Picture, defPic, picList, Action)
--import Gallery exposing (..)
import Time exposing (..)
import Task exposing (..)
import Listing exposing (..)
--import Gallery

-- Model
type alias Model = 
  { current  : State
  , picMap   : Dict String (Lightbox.Model)
  , noScroll : Bool
  , scrollValue : Int 
  }

type State = Menu | Unfolded Category

type Category = 
    Digital
  | Sketching
  | Watercolour
  | Others

initialModel = 
  let toPics xs folder = 
        Lightbox.init (List.map (\(filename,caption) ->
                                    { defPic | filename = filename
                                             , caption  = Just caption
                                    }) xs)
                      folder
  in 
  Model Menu
       (Dict.fromList [ ("Digital", toPics digital "digital")
                      , ("Sketching", toPics sketching "sketching")
                      , ("Watercolour", toPics watercolour "watercolour")
                      , ("Others", toPics others "others")
                      ]
       )
       False
       0

-- View
view address model = page address model

-- Update

type Action = 
   NoOp
 | Open Category
 | Close 
 | LightboxAction (Lightbox.Action)
 | ScrollY Int 

update : Action -> Model -> (Model, Effects Action) 
update action model = 
  case action of
    NoOp     -> (model,none)
    Open cat -> ({ model | current = Unfolded cat }, none)
    Close    -> ({ model | current = Menu }, none)
    
    LightboxAction act -> 
      let {current, picMap} = model
      in
      case Dict.get (state2String current) picMap of 
        Nothing -> (model,none)
        Just lightbox ->
          let lightbox' = Lightbox.update act lightbox
              noScroll' = Lightbox.blockScroll act
              
              eff       =
                case act of 
                  --Lightbox.Loaded _ -> sendAdjustMargin
                  --Lightbox.Zoomed -> sendAdjustMargin
                  _               -> none 
          in
          ({ model |
             picMap = Dict.insert (state2String current) lightbox' picMap
           , noScroll = noScroll' 
           },eff)

    ScrollY v -> ({ model | scrollValue = v }, none)

state2String : State -> String  
state2String state = 
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

port scrollY : Signal Int

scrollYUpdate : Signal Action
scrollYUpdate = Signal.map (\v -> ScrollY  v) scrollY

-- explanation for this mess at:
-- http://danielbachler.de/2016/02/26/ports-in-elm.html

requestAdjustMarginMailbox : Signal.Mailbox String
requestAdjustMarginMailbox =
  Signal.mailbox ""

port requestAdjustMargin : Signal String
port requestAdjustMargin =
  requestAdjustMarginMailbox.signal

sendAdjustMargin : Effects Action
sendAdjustMargin =
  Signal.send requestAdjustMarginMailbox.address ""
  |> Effects.task
  |> Effects.map (\_ -> NoOp)


app =
    StartApp.start
          { init = (initialModel, none)
          , view = view
          , update = update
          , inputs = [scrollYUpdate]
          }

main =
    app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


-- Data

galleries = []


page address model = 
  div [id "page", classList [("pageScroll", .noScroll model)]]
      [ header'
      , gallery address model
      , p [] [text (toString (.scrollValue model))]
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
       case Dict.get (state2String current) picMap of 
        Nothing -> div [id "tata"]
                       [ a [ onClick address Close, id "backMenuBtn"]
                           [ text "Back to menu"]
                       ]

        Just lightbox ->
          let lightbox' = 
                Lightbox.view (Signal.forwardTo address LightboxAction) 
                              lightbox
          in      
              div [id "tata"]
                  [ lightbox'
                  , a [ onClick address Close, id "backMenuBtn"]
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

unfolded model =
  case (.current model) of 
    Unfolded _ -> True
    _          -> False 
module Lightbox (Picture, Model, Action(..)
                , init
                , update
                , view
                , defPic
                , picList
                , picCaption) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import List exposing (..)
import String exposing (words, join, cons, uncons)
import Dict exposing (..)
import Date exposing (..)
import Streams exposing (..)

-- Model
type alias Model = 
     { pictures : BiStream Picture
     , nameList : List String
     , folder : String
     , display : Bool
     , diaporama : Bool
     , loading : Bool
     }

--, button [ ] [i [class "fa fa-spinner fa-spin"] []]

type alias Picture = 
    { filename : String
    , author : Maybe String
    , date : Maybe Date  
    , caption : Maybe String
    , linkHD : Bool
    }

defPic : Picture
defPic = Picture "" Nothing Nothing (Just "") False

picList : Int -> List Picture
picList n = 
  let go m = 
          let filename = if ( n - m ) < 10 
                         then "0" ++ toString (n - m) ++ ".jpg"
                         else toString (n - m) ++ ".jpg"  
              pic = 
                { defPic |
                  filename = filename
                , caption = Just ""
                }
          in if m == -1 then [] else pic :: go (m - 1)
  in go (n-1)  

picCaption : List (String,String) -> List Picture -> List Picture
picCaption cs ps = 
  let addCaption p = 
        let caption = 
          case List.head (List.filter (\(f,c) -> .filename p == f) cs) of
            Nothing -> Nothing
            Just (filename,caption) -> Just caption
        
        in { p | caption = caption }
        
  in List.map addCaption ps

init : List Picture -> String -> Model
init pics folder = 
  let nameList = List.map .filename pics
  in Model (biStream pics defPic) nameList folder False False False


-- Update
type Action = 
   NoOp
 | Left
 | Right
 --| Display
 | Close
 | GoTo String
 --| TimeStep
 --| Diaporama
 --| OpenDiapo
 | Loaded

update : Action -> Model -> Model
update action model =
  case action of 
    NoOp      -> model
    Left      -> { model | pictures  = left (.pictures model) , loading = True }
    Right     -> { model | pictures  = right (.pictures model), loading = True }
    --Display   -> { model | display   = not (.display model)   , loading = True }
    Close     -> { model | display   = False, loading = False }
                        
    GoTo n    -> { model |
                   pictures = goTo (.pictures model) (\p -> (.filename p) == n)
                 , display  = True
                 , loading  = not (n == .filename (current (.pictures model)))
                 }
    Loaded -> { model | loading = False }


-- View
thumbs : Signal.Address Action -> Model -> Html
thumbs address model = 
  let nameList = .nameList model

      thumb n  =
        a [ onClick address (GoTo n)
          ]
          [ img [src ("images/" 
                       ++ (.folder model)
                       ++ "/thumbs/"
                       ++ n)
                ] []
          ]
  in div [class "thumbs"] (List.map thumb nameList)

lightbox : Signal.Address Action -> Model -> Html
lightbox address model = 
  let currentPic = current (.pictures model)
  in 
  div [ classList [("lightbox",True),("display",(.display model))]
      , onKey address, tabindex 0, autofocus True
      ]
      [ div [ class "lightbox-content"
            , onKey address, tabindex 0, autofocus True
            ]
            [ div [ class "picContainer"]
                  
                  [ img [src ("images/" 
                             ++ (.folder model) ++ "/"
                             ++ (.filename currentPic))
                        , on "load" targetSrc (Signal.message address << (\s -> Loaded))
                        ] []

                  
                  , div [ class "halfPic"
                        , id "halfPicleft"
                        , onClick address Left
                        ] [span [class "noselect"] [text "<<"]]
                  , div [ class "halfPic"
                        , id "halfPicright"
                        , onClick address Right
                        ] [span [class "noselect"] [text ">>"]]
                  ]

            , div [ class "lightBoxcaption"]
                  [ text (Maybe.withDefault "" (.caption currentPic))
                  , a [ id "closebtn"
                      , class "noselect"
                      , onClick address Close
                      ] [i [class "fa fa-times"] []]
                  , a [ classList [("loader",True),("display",(.loading model) && (.display model))] ]
                      [ if (.loading model)
                        then i [class "fa fa-spinner fa-spin"] []     
                        else i [] []
                      ]
                  ]
            ]
      ]

onKey : Signal.Address Action -> Attribute
onKey address = 
  let keyToAct key = 
        if (key == 13) || (key == 39)
        then Right
        else if key == 37
        then Left
        else if key == 27
        then Close
        else NoOp
  in onKeyDown address keyToAct


view : Signal.Address Action -> Model -> Html
view address model =
  div []
      [ thumbs address model
      , lightbox address model
      ]


myStyle = 
  style [ ("animation", "fadein 2s")
        ] 

onLoad : Signal.Address a -> a -> Attribute
onLoad =
  messageOn "load"

targetSrc : Json.Decoder String
targetSrc =
  Json.at ["target", "src"] Json.string

messageOn : String -> Signal.Address a -> a -> Attribute
messageOn name addr msg =
  on name Json.value (\_ -> Signal.message addr msg)




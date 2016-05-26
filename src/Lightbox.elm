module Lightbox (Picture, Model, Action(..)
                , init
                , update
                , view
                , defPic
                , picList
                , picCaption
                , blockScroll) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import List exposing (..)
import String exposing (words, join, cons, uncons)
import Dict exposing (..)
import Date exposing (..)
import Streams exposing (..)
import DOM exposing (target, offsetWidth, offsetHeight, parentElement)


-- Model
type alias Model = 
     { pictures : BiStream Picture
     , nameList : List String
     , folder : String
     , display : Bool
     , diaporama : Bool
     , loading : Bool
     , zoomed : Bool
     , vpSize : Maybe (Float,Float)
     }

--, button [ ] [i [class "fa fa-spinner fa-spin"] []]

type alias Picture = 
    { filename : String
    , author : Maybe String
    , date : Maybe Date  
    , caption : Maybe String
    , linkHD : Bool
    , picSize : Maybe (Float,Float)
    }

defPic : Picture
defPic = Picture "" Nothing Nothing (Just "") False Nothing

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
  in Model (biStream pics defPic) nameList folder False False False False Nothing


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
 | Zoomed 
 | Loaded Dimension

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
    Zoomed -> {model | zoomed = not (.zoomed model)}
    Loaded { vpWidth
           , vpHeight 
           , picWidth 
           , picHeight 
           } -> { model | loading = False 
                , vpSize = Just (vpWidth,vpHeight)
                , pictures =
                    let old = current (.pictures model)
                    in updateCurrent 
                       {old | picSize = Just (picWidth,picHeight)}
                       (.pictures model)
                 }


-- View
thumbs : Signal.Address Action -> Model -> Html
thumbs address model = 
  let nameList = .nameList model

      thumb n  =
        a [ onClick address (GoTo n)
          , href "#top" 
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
      [ div [ classList [("lightbox-content", True)
                        ,("lbzoomed", .zoomed model)
                        ,("lbunzoomed", not (.zoomed model))
                        ]
            , onKey address, tabindex 0, autofocus True
            , id "lightBC"
            ]
            [ p [] [text ((toString (.vpSize model)) ++ (toString (.picSize (current (.pictures model)))))]
            , div [ class "picContainer", id "picContainer"]
                  
                  [ img [src ("images/" 
                             ++ (.folder model) ++ "/"
                             ++ (.filename currentPic))
                        --, on "load" targetSrc (Signal.message address << (\s -> Loaded))
                        , on "load" (DOM.target getDimension) (Loaded >> Signal.message address)
                        , classList [("zoomed", .zoomed model)
                                    ,("unzoomed", not (.zoomed model))
                                    ]
                        --, attribute "onload" "adjustMargin()"
                        , id "lightboxPic"
                        ] []

                  
                  , div [ class "halfPic"
                        , id "halfPicleft"
                        , onClick address Left
                        ] [span [class "noselect"] [text "<<"]]
                  , div [ id "centerPic"
                        --, onClick address Zoomed
                        ] [span [class "noselect"] [text "="]]

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
  div [id "toto"]
      [ thumbs address model
      , lightbox address model
      , div [classList [("fixedBack",True),("display",(.display model))]] []
      ]


blockScroll : Action -> Bool
blockScroll act = 
  case act of 
    GoTo _   -> True
    Left     -> True
    Right    -> True
    Loaded _ -> True
    _        -> False

myStyle = 
  style [ ("animation", "fadein 2s")
        ] 

onLoad : Signal.Address a -> a -> Attribute
onLoad =
  messageOn "load"

targetSrc : Json.Decoder String
targetSrc =
  Json.at ["target", "src"] Json.string

type alias Dimension =
  { vpWidth   : Float
  , vpHeight  : Float
  , picWidth  : Float
  , picHeight : Float
  } 

getWidthHeight : Json.Decoder (Float,Float)
getWidthHeight = 
  Json.object2
    (\dec1 dec2 ->
      (dec1, dec2)) offsetWidth offsetHeight

getLightBoxSize : Json.Decoder (Float,Float)
getLightBoxSize = 
  parentElement
   (parentElement
    (parentElement getWidthHeight))

getDimension : Json.Decoder Dimension
getDimension =
  Json.object3
    (\width height (vpW,vpH) ->
      { vpWidth  = vpW
      , vpHeight = vpH
      , picWidth = width
      , picHeight = height 
      })
    offsetWidth
    offsetHeight
    getLightBoxSize



messageOn : String -> Signal.Address a -> a -> Attribute
messageOn name addr msg =
  on name Json.value (\_ -> Signal.message addr msg)




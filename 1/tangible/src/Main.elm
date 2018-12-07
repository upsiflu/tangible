module Main exposing (..)

import Tuple exposing (pair)
import Browser
import Html exposing (a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)


---- MODEL ----


type alias Model =
    {live:Live}

type  Ambi
    = Embi --end of ambi: unit
    | Many Ambi
    | Product (List Ambi) --product type. The items are multiplied.
    | Emblem Ambi --allegory+poem.
    | Figure Ambi
    | Paragraph Ambi --contents, to put in a paragraph block.
    | Link Ambi --generator (target).
    | Title Ambi --generator (string). String doubles as id.
    | Caption Ambi --generator (string). String doubles as id.
    | Text Ambi --generator (string). just text.
    | Name String Ambi --none of the above

ambiLive : Ambi -> Live
ambiLive ambi = pair ambi <|
  case ambi of
    Embi          -> EmptyContents
    Product []    -> EmptyContents
    Many a        -> Multi EmptyContents <| ambiLive a
    Product (a::p)-> Both (ambiLive a) (ambiLive <| Product p)
    Emblem a      -> Both (ambiLive <| Figure Embi) (ambiLive a)
    Figure a      -> Only <| ambiLive a
    Paragraph a   -> Only <| ambiLive a
    Name s a      -> Only <| ambiLive a
    Link a        -> EmptyTarget <| ambiLive a
    Title a       -> EmptyText <| ambiLive a
    Caption a     -> EmptyText <| ambiLive a
    Text a        -> EmptyText <| ambiLive a

type alias Live = (Ambi, Data)
type Data
    = EmptyContents
    | Multi Data Live
    | Both  Live Live
    | Only  Live
    | EmptyTarget Live
    | EmptyText   Live
    | FullTarget  String Live
    | FullText    String Live


init : ( Model, Cmd Msg )
init =
    let
     initialAmbi
      = Name "flupsicom"
       <| Many <| Name "topic" <| Product
          [ Title Embi
          , Many <| Caption Embi
          , Many <| Emblem <| Many <| Paragraph <| Text Embi
          , Many <| Paragraph <| Product
            [ Many <| Text Embi
            , Many <| Link Embi
            ]
          ]
    in ( {live=ambiLive initialAmbi}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Milestone 1: Empty Data" ]
        , viewLive model.live
        ]

viewLive : (Ambi, Data) -> Html msg
viewLive (ambi, data) =
  let
    viewData dd = case dd of
      EmptyContents -> text "-"
      Multi d l     -> span [] [text "=Multi ", viewData d, viewLive l]
      Both l r      -> span [] [viewLive l, text " | ", viewLive r]
      Only l        -> span [] [text "=Only ", viewLive l]
      EmptyTarget l -> span [] [text "=Missing Link ", viewLive l]
      FullTarget s l-> span [] [text <| "=-> "++s, viewLive l]
      EmptyText l   -> span [] [text "=Missing Text ", viewLive l]
      FullText s l  -> span [] [text s, viewLive l]
    viewTemplate t = case t of
      Embi          -> span [] [text "(Embi)"]
      Many a        -> span [] [text "(Many ", viewTemplate a, text ")"]
      Product []    -> span [] [text "(})"]
      Product (a::p)-> span [] [text "({ ", viewTemplate a, viewTemplate (Product p), text "})"]
      Emblem a      -> span [] [text "(Emblem ", viewTemplate a, text ")"]
      Figure a      -> span [] [text "(Figure ", viewTemplate a, text ")"]
      Paragraph a   -> span [] [text "(Paragraph ", viewTemplate a, text ")"]
      Link a        -> span [] [text "(Link ", viewTemplate a, text ")"]
      Title a       -> span [] [text "(Title ", viewTemplate a, text ")"]
      Caption a     -> span [] [text "(Caption ", viewTemplate a, text ")"]
      Text a        -> span [] [text "(Text ", viewTemplate a, text ")"]
      Name s a      -> span [] [text "(Paragraph ", viewTemplate a, text ")"]

  in
    case ambi of
      Embi          -> div [class "Embi"] [text "Embi: ", viewData data]
      Many a        -> div [class "Many"] [text "+ ", viewTemplate a, text ": ", viewData data]
      Product []    -> div [class "Product0"] [text "}", viewData data]
      Product (a::_)-> div [class "Product"] [text "{ ", viewTemplate a, text ": ", viewData data]
      Emblem a      -> div [class "Emblem"] [text "Emblem<", viewTemplate a, text "> ", viewData data]
      Figure a      -> div [class "Figure"] [text "Figure<", viewTemplate a, text "> ", viewData data]
      Paragraph a   -> p   [class "Paragraph"] [text "Paragraph: ", viewData data]
      Link a        -> span[class "Link"] [text "Link: ", viewData data]
      Title a       -> h1  [class "Title"] [text "", viewData data]
      Caption a     -> h3  [class "Caption"] [text "", viewData data]
      Text a        -> span[class "Text"] [text "t: ", viewData data]
      Name s a      -> p   [class "Name"] [text <| "=="++s++"==", viewData data]






---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

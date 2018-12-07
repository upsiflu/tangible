module App exposing (App, initial, Ambi, view)

import List exposing (map)
import Html exposing (section, article, a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)


--App is an immutable type, consisting of nested Ambi.
--This module provides de/serialization. (TODO).
--An App is the result of a remix. (TODO).
--Each App streams its own, mutable collection of Receiver data. (TODO).


type App
  = App Ambi

type  Ambi
    = E --end of ambi: unit
    | Template Ambi
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


initial = App <|
      Name "flupsicom"
       <| Many <| Name "topic" <| Product
          [ Title E
          , Many <| Caption E
          , Many <| Emblem <| Many <| Paragraph <| Text E
          , Many <| Paragraph <| Product
            [ Many <| Text E
            , Many <| Link E
            ]
          ]


view (App ambi) =
  section [class "App"]
    [ h2 [] [text "Review this App"]
    , viewAmbi ambi
    ]

viewAmbi symbol = case symbol of
  E                -> text "END"
  Template  a      -> div [] [span [class "type"] [text "Template"], viewAmbi a]
  Many      a      -> div [] [span [class "type"] [text "Many"],     viewAmbi a]
  Product   aa     -> div [] [ol   [] <| map ( \a->li [] [viewAmbi a] ) aa     ]
  Emblem    a      -> div [] [span [class "type"] [text "Emblem"],   viewAmbi a]
  Figure    a      -> div [] [span [class "type"] [text "Figure"],   viewAmbi a]
  Paragraph a      -> div [] [span [class "type"] [text "Paragraph"],viewAmbi a]
  Link      a      -> div [] [span [class "type"] [text "Link"],     viewAmbi a]
  Title     a      -> div [] [span [class "type"] [text "Title"],    viewAmbi a]
  Caption   a      -> div [] [span [class "type"] [text "Caption"],  viewAmbi a]
  Text      a      -> div [] [span [class "type"] [text "Text"],     viewAmbi a]
  Name    s a      -> div [] [span [class "type"] [text s],          viewAmbi a]

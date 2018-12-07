module App exposing
  (getAmbi, App, initial, Ambi(..), view, viewAmbi)

import List exposing (map)
import Html exposing (section, article, a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)

--App is an immutable type, consisting of nested Ambi.
--This module provides de/serialization. (TODO).
--An App is the result of a remix. (TODO).
--Each App streams its own, mutable collection of Receiver data. (TODO).


type App
  = App Ambi

type Ambi
    = E --end of ambi: unit
    | Template Ambi
    | Many Ambi
    | Product (List Ambi) --product type. The items are multiplied.
    | Emblem Ambi --allegory+poem.
    | Paragraph Ambi --contents, to put in a paragraph block.
    | Name String Ambi --none of the above
    | Link Ambi --generator (target).
    | Title Ambi --generator (string). String doubles as id.
    | Caption Ambi --generator (string). String doubles as id.
    | Text Ambi --generator (string). just text.

getAmbi (App ambi) = ambi

{--
blank ambi =
  case ambi of
    --no data associable:--
    E             -> blankE
    Template a    -> blankTemplate a
    Many a        -> blankMany a
    Product []    -> blankEmptyProduct []
    --nesting and typecasting:--
    Product aa    -> blankProduct aa
    Emblem a      -> blankEmblem (blank a)
    Paragraph a   -> blankParagraph (blank a)
    Name s a      -> blankName s (blank a)
    --generators:--
    Link a        -> blankLink (blank a)
    Title a       -> blankTitle (blank a)
    Caption a     -> blankCaption (blank a)
    Text a        -> blankText (blank a)
    ==--
==--}

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


view (App a) =
  section [class "App"]
    [ h2 [] [text "App Structure"]
    , viewAmbi a
    ]

viewAmbi symbol =
 let
  viewContainer = div [class "app"]
  viewSymbol    = span [class "type"]
 in case symbol of
  E                -> text " ()"
  Template  a      -> viewContainer [viewSymbol [text " < "], viewAmbi a]
  Many      a      -> viewContainer [viewSymbol [text " + "],     viewAmbi a]
  Product   aa     -> viewContainer [ul [class "product"] <| map ( \a->li [] [viewAmbi a] ) aa     ]
  Emblem    a      -> viewContainer [viewSymbol [text "Emblem"],   viewAmbi a]
  Paragraph a      -> viewContainer [viewSymbol [text "Paragraph"],viewAmbi a]
  Link      a      -> viewContainer [viewSymbol [text "Link"],     viewAmbi a]
  Title     a      -> viewContainer [viewSymbol [text "Title"],    viewAmbi a]
  Caption   a      -> viewContainer [viewSymbol [text "Caption"],  viewAmbi a]
  Text      a      -> viewContainer [viewSymbol [text "Text"],     viewAmbi a]
  Name    s a      -> viewContainer [viewSymbol [text s],          viewAmbi a]

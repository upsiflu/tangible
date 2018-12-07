module Composition exposing (..)

import Site exposing (Site, currentVersion, publishVersion, Live(..))
import Html exposing (a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)


--An Avatar can create Compositions by [editing] a Site.
--Each Edit from each Session is accumulated in a Composition.
--The [publish] command merges the new Edits from this Composition
--into the referenced Site. Thus, Composition is a short-lived diff
--to Site.


--We got a site, i.e. published app with published data.
--What we do here is,
--(1) find possible actions, notably + (Many), Move (Product) and Modify (Media).
--(2) when viewing a site, attach locators with unique identifiers.
--    Locators with a visible target are clickable in the stream.
--(3) Show actions that have no visible locator in a separate panel (Action Bar).
--(4) Show a side panel that lists the history, and offers a button to publish.
--(5) On publish, push a new version into the site.


type Composition = Composition Site <| List Edit

edit : Edit -> Composition -> Composition
edit e (Composition site edits)
 = Composition site e::edits

publish : Composition -> Site
publish (Composition site edits)
 = Site.publishVersion site <| merge edits <| currentVersion site

merge : (List Edit) Live
merge (e:ee) live
  = case e of
    

view : Site -> Html msg
view (Composition site edits)
  = let
     viewInput  i = case i of
       Just (Input s)  -> text s
       Nothing         -> text "_________"
     viewFigure f = case f of
       Just (Figure m) -> div [] [text "Figure: ", viewMedia m]
       Nothing         -> div [] [text "No Figure"]
     viewMedia  m = case m of
       Just (Media s)  -> text s
       Nothing         -> text "No Media"
     viewAnchor a = case a of
       Just (Anchor s) -> text s
       Nothing         -> text "No Anchor"
     viewTemplate = App.viewAmbi
     viewLive x = case x of
       E                   -> text "()"
       Template  (T t)     -> viewTemplate t
       Many      (T t) prod-> section [class "Many"] [span [] [text "+"], viewTemplate t, div [class "prototype"] [viewLive (blank t)], viewLive <| Product prod]
       Product   (P ll)    -> ul      [class "Product"] <|   map ( \l->li [] [viewLive l] ) ll
       Emblem    figure l  -> section [class "Emblem"]    [viewFigure figure, viewLive l]
       Paragraph l         -> p       [class "Paragraph"]                    [viewLive l]
       Link      anchor l  -> span    [class "Link"]      [viewAnchor anchor, viewLive l]
       Title     input  l  -> h2      [class "Title"]     [viewInput input,   viewLive l]
       Caption   input  l  -> h3      [class "Caption"]   [viewInput input,   viewLive l]
       Name      s      l  -> section [class "Name"]      [span [] [text s],  viewLive l]
       Text      input  l  -> span    [class "Link"]      [viewInput input,   viewLive l]
     viewPrevious = case site.prev of
       [] -> text "this is the first version."
       p::pp -> text "there is an older version."
     viewNext = case site.next of
       []     -> text "this is the last version."
       n::nn  -> text "there is a newer version."
    in
      section  []
      [ article [class "Edits"]
        [ h3 [] [text "Edit History:"]
        , section [] [viewEdits]
        ]
      , App.view site.app
      , section [class "Live"]
        [ h2 [] [text "Live"]
        , viewLive site.live
        ]
      , viewActionBar
      ]



viewActionBar : (List Action) -> Html msg
viewPlus actions =
  case actions of
   [] -> text ""
   (Create ambi::aa) ->
    div [ class "plus"]
        [ div  [] [text "+"]
        , span [class "Template"] [text "(+ ", viewLive (blank ambi), text ")"]
        , viewPlus aa
        ]


viewLive : (Ambi, Data) -> Html msg
viewLive (ambi, data) =
  let
    viewData d = case d of
      Impossible    -> span [] [text "-"]
      Both l r      -> span [] [viewLive l, text " | ", viewLive r]
      Only l        -> span [] [text "", viewLive l]
      EmptyTarget l -> span [] [text "Missing Link ", viewLive l]
      FullTarget s l-> span [] [text <| "=-> "++s, viewLive l]
      EmptyText l   -> span [] [text "Missing Text ", viewLive l]
      FullText s l  -> span [] [text s, viewLive l]
    viewTemplate t = case t of
      E             -> span [class "Template"] [text "?"]
      Template a    -> span [class "Template"] [text "(Template)"]
      Many a        -> span [class "Template"] [text "(+ ", viewTemplate a, text ")"]
      Product []    -> span [class "Template"] [text " []"]
      Product (a::p)-> span [class "Template"] [viewTemplate a,text ", ", viewTemplate (Product p)]
      Emblem a      -> span [class "Template"] [text "(Emblem ", viewTemplate a, text ")"]
      Figure a      -> span [class "Template"] [text "(Figure ", viewTemplate a, text ")"]
      Paragraph a   -> span [class "Template"] [text "(P ", viewTemplate a, text ")"]
      Link a        -> span [class "Template"] [text "(--> ", viewTemplate a, text ")"]
      Title a       -> span [class "Template"] [text "(Title ", viewTemplate a, text ")"]
      Caption a     -> span [class "Template"] [text "(Caption ", viewTemplate a, text ")"]
      Text a        -> span [class "Template"] [text "(Text ", viewTemplate a, text ")"]
      Name s a      -> span [class "Template"] [text "('", text s, text "': ", viewTemplate a, text ")"]
  in
    case ambi of
      E             -> div [class "E"] [text "E"]
      Template a    -> div [class "Template Template0"] [text "T"]
      Many a        -> div [class "Many"] [text "+ ", viewTemplate a, text ": ", viewData data]
      Product []    -> div [class "Product0"] [text "}", viewData data]
      Product (a::_)-> div [class "Product"] [text "{ ", viewData data]
      Emblem a      -> div [class "Emblem"] [text "Emblem<", viewTemplate a, text "> ", viewData data]
      Figure a      -> div [class "Figure"] [text "Figure<", viewTemplate a, text "> ", viewData data]
      Paragraph a   -> p   [class "Paragraph"] [text "Paragraph: ", viewData data]
      Link a        -> span[class "Link"] [text "Link: ", viewData data]
      Title a       -> h1  [class "Title"] [text "", viewData data]
      Caption a     -> h3  [class "Caption"] [text "", viewData data]
      Text a        -> span[class "Text"] [text "t: ", viewData data]
      Name s a      -> p   [class "Name"] [text <| "=="++s++"==", viewData data]

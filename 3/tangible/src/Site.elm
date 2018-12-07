module Site exposing (Site, fromApp, view)

import Lazy exposing (lazy)
import App exposing (App, Ambi(..), viewAmbi)
import Tuple exposing (pair, second)
import List exposing (map)
import Html exposing (section, article, a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)


--A Site is the result of Data applied to an App.
--It evaluates the App's model (Ambi) and Receivers (TODO).
--The result is a Live object.

--Site, in itself, is a non-interactive object except for navigation.
--So you need a Composition to edit any data here, directly.

--Additionally, Site has Versions you can browse.



type Site
  = Site {app: App, live: Live, prev: PreviousVersion, next: List NextVersion}

type PreviousVersion = PreviousVersion Site | Beginning
type NextVersion     =     NextVersion Site

goPreviousVersion (Site {prev}) = prev
goNextVersions   (Site {next}) = next
thisVersion    (Site {app, live, prev, next})
 = {app=app, live=live, prev=prev, next=next}


publishVersion : Live -> Site -> Site
publishVersion newLive site
 = let
    current = thisVersion site
    newApp = current.app
    newPrev = PreviousVersion site
    newNext = []
   in
    Site { app=newApp, live=newLive
        , prev=newPrev
        , next=newNext
        }


fromApp : App -> Site
fromApp app
  = Site {app=app, live=blank <| App.getAmbi app, prev=Beginning, next=[]}

type T = T App.Ambi
type P = P (List Live)

type Live
  = E --end of live: unit
  | Template  T
  | Many      T P
  | Product   P
  | Emblem    (Maybe Figure) Live --allegory+poem.
  | Paragraph Live        --contents, to put in a paragraph block.
  | Link      (Maybe Anchor) Live --generator (target).
  | Title     (Maybe Input) Live   --generator (string). String doubles as id.
  | Caption   (Maybe Input) Live   --generator (string). String doubles as id.
  | Name      String Live --none of the above
  | Text      (Maybe Input) Live

type Input   = Input String
type Figure  = Figure (Maybe Media)
type Media   = Media String
type Anchor  = Anchor String

view : Site -> Html msg
view (Site site)
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
      Many  (T t) product -> section [] [viewTemplate t, viewLive <| Product product]
      Product   (P ll)    -> ul [class "product"] <| map ( \l->li [] [viewLive l] ) ll
      Emblem    figure l  -> section [class "Emblem"] [viewFigure figure, viewLive l]
      Paragraph l         -> p    [class "Paragraph"]                  [viewLive l]
      Link      anchor l  -> span [class "Link"]      [viewAnchor anchor, viewLive l]
      Title     input  l  -> h2   [class "Title"]     [viewInput input,   viewLive l]
      Caption   input  l  -> h3   [class "Caption"]   [viewInput input,   viewLive l]
      Name      s      l  -> section [class "Name"]      [span [] [text s],            viewLive l]
      Text      input  l  -> span [class "Link"]      [viewInput input, viewLive l]
    viewPrevious = case site.prev of
      Beginning -> text "this is the first version."
      PreviousVersion p -> text "there is an older version."
    viewNext = case site.next of
      []     -> text "this is the last version."
      v::[]  -> text "there is a newer version."
      _      -> text "there are several newer versions."
   in
     section  []
     [ article [] [h3 [] [text "Versions:"], section [] [viewPrevious], section [] [viewNext]]
     , App.view site.app
     , section [class "Live"]
       [ h2 [] [text "Live"]
       , viewLive site.live
       ]
     ]

--draw a blank canvas from a provided ambi--
--blank = Template << T << App.getAmbi
blank : Ambi -> Live
blank ambi =
  case ambi of
    --no data associable:--
    App.E             -> E
    App.Template a    -> Template (T a)
    App.Many a        -> Many (T a) (P [])
    App.Product []    -> Product (P [])
    --nesting and typecasting:--
    App.Product aa    -> Product (P <| map blank aa)
    App.Emblem a      -> Emblem Nothing (blank a)
    App.Paragraph a   -> Paragraph (blank a)
    App.Name s a      -> Name s (blank a)
    --generators:--
    App.Link a        -> Link Nothing (blank a)
    App.Title a       -> Title Nothing (blank a)
    App.Caption a     -> Caption Nothing (blank a)
    App.Text a        -> Text Nothing (blank a)



{--
loadTemplate : T -> Live
loadTemplate (T ambi) = case App.whatIs ambi of
  App.IsName Name s -> Name s
==--}

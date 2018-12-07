module Site exposing (..)

import App exposing (Ambi)
import Tuple exposing (pair, second)


--A Site is the result of Data applied to an App.
--It evaluates the App's model (Ambi) and Receivers (TODO).
--The result is a Live object.

--Site, in itself, is a non-interactive object except for navigation.
--So you need a Composition to edit any data here, directly.

--Additionally, Site has Versions you can browse.


type PreviousVersion = PreviousVersion Site | Beginning
type NextVersion     =     NextVersion Site | End

type Site
  = Site Live PreviousVersion (List NextVersion)

publishVersion : Live -> Site -> Site
publishVersion new (Site live previous next)
  = Site new (Site live previous (new::next)) End



fromAmbi : Ambi -> Site
fromAmbi ambi
    = Site (blank ambi) Beginning [End]

type alias Live = (Ambi, Data)
type Data
    --nesting--
    = Only  Live      --One Data. For wrapping types (casting).
    | Both  Live Live --Two Data, each with their type.
    --concrete data types--
    | Impossible   --Unit type, with just one member.
    | EmptyTarget Live
    | EmptyText   Live
    | FullTarget  String Live
    | FullText    String Live
type Action
    = Create Ambi

type alias Uid = Int


view : Site -> Html msg
view site
 = div []
     [ viewLive site
     , viewVersions site
     ]




--draw a blank canvas from a provided ambi--
blank : Ambi -> Live
blank ambi =
  let
    live = pair ambi <|
      case ambi of
        --no data associable:--
        E             -> Impossible
        Template a    -> Impossible
        Product []    -> Impossible
        Many a        -> Only (blank a)
        --nesting and typecasting:--
        Product (a::p)-> Both (blank a) (blank <| Product p)
        Emblem a      -> Both (blank <| Figure E) (blank a)
        Figure a      -> Only (blank a)
        Paragraph a   -> Only (blank a)
        Name s a      -> Only (blank a)
        --generators:--
        Link a        -> EmptyTarget <| blank a
        Title a       -> EmptyText <| blank a
        Caption a     -> EmptyText <| blank a
        Text a        -> EmptyText <| blank a
  in live

module Main exposing (..)

--import Composition exposing (Composition)
import Site exposing (Site, fromApp)
import App exposing (App, view)
import Tuple exposing (pair, second)
import Browser
import Html exposing (a, h1, h2, h3, p, span, li, ol, ul, Html, text, div, h1, img)
import Html.Attributes exposing (src, class)

--This describes a single session.
--A session has a unique number so that users can
--open multiple tabs with the same avatar and composition.

--Sessions persist indirectly through their commits.

--A Modification is always originating from a Session.

--  MILESTONE 2: Display the App type only.

{--
General Layout:

A Session is either visiting a Site,
or editing a Composition,
or reviewing an App.

--}

---- MODEL ----
type Model
  = Reviewing App
  | Visiting  Site
  {--
  | Editing    Composition
  --}

init : ( Model, Cmd Msg )
init = ( Visiting <| Site.fromApp App.initial, Cmd.none )
--Editing <| Composition.Edit <| Site.Fresh App.initialApp

---- UPDATE ----


type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Milestone 3: Data" ]
        , case model of
           Visiting site -> Site.view site
           --Editing composition -> Composition.view composition
           Reviewing app -> App.view app
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

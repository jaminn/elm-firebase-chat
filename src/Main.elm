port module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Transitions as Transition exposing (transition)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css, id, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy, lazy2)
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Decode.Pipeline as P exposing (required)
import Json.Encode as E exposing (Value(..))
import Task
import Time
import UIElement exposing (toggle)


port firebaseSend : E.Value -> Cmd msg


port fromFirebase : (E.Value -> msg) -> Sub msg


encodeFirebaseType : FirebaseType -> E.Value
encodeFirebaseType firebaseType =
    E.object <|
        [ ( "msg", E.string firebaseType.msg )
        , ( "obj", firebaseType.obj )
        ]


firebaseTypeDecoder : Decoder FirebaseType
firebaseTypeDecoder =
    D.succeed FirebaseType
        |> P.required "msg" D.string
        |> P.required "obj" D.value


type alias FirebaseType =
    { msg : String
    , obj : E.Value
    }


addPost : String -> String -> Cmd msg
addPost title content =
    Post title content -1
        |> encodePost
        |> FirebaseType "addPost"
        |> encodeFirebaseType
        |> firebaseSend


getPosts : Cmd msg
getPosts =
    FirebaseType "getPosts" E.null
        |> encodeFirebaseType
        |> firebaseSend



-- MAIN


main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Post =
    { title : String
    , content : String
    , createdAt : Int
    }


encodePost : Post -> E.Value
encodePost post =
    E.object <|
        [ ( "title", E.string post.title )
        , ( "content", E.string post.content )
        , ( "createdAt", E.int post.createdAt )
        ]


postDecoder : Decoder Post
postDecoder =
    D.succeed Post
        |> P.required "title" D.string
        |> P.required "content" D.string
        |> P.required "createdAt" D.int


type alias Model =
    { posts : List Post
    , postTitleInput : String
    , postContentInput : String
    , isPostBlocked : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "" "" False
    , getPosts
    )



-- UPDATE


type Msg
    = NoOp
    | PostTitleInputted String
    | PostContentInputted String
    | AddPost
    | Recv E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PostTitleInputted str ->
            ( { model | postTitleInput = str }, Cmd.none )

        PostContentInputted str ->
            ( { model | postContentInput = str }, Cmd.none )

        AddPost ->
            if String.isEmpty model.postTitleInput then
                ( model, Cmd.none )

            else
                ( { model | isPostBlocked = True }, addPost model.postTitleInput model.postContentInput )

        Recv val ->
            case decodeValue firebaseTypeDecoder val of
                Ok t ->
                    if t.msg == "getPostsSuccess" then
                        ( { model
                            | posts =
                                decodeValue (D.list postDecoder) t.obj
                                    |> Result.withDefault []
                                    |> List.sortBy .createdAt
                          }
                        , Cmd.none
                        )

                    else if t.msg == "addPostSuccess" then
                        ( { model | isPostBlocked = False, postTitleInput = "", postContentInput = "" }, getPosts )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    fromFirebase Recv



-- VIEW


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (D.andThen isEnter keyCode)


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput PostTitleInputted, value model.postTitleInput ] []
        , input [ onInput PostContentInputted, onEnter AddPost, value model.postContentInput ] []
        , button
            [ onClick AddPost
            , css
                [ if model.isPostBlocked then
                    pointerEvents none

                  else
                    pointerEvents auto
                , if model.isPostBlocked then
                    color (hex "#333")

                  else
                    color (hex "#000")
                ]
            ]
            [ text "addPost" ]
        , div [] (List.map (\p -> div [] [ text p.title, text " : ", text p.content ]) model.posts)
        ]

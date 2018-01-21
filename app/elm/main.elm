module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN


type alias Book =
    { title : String
    , author : String
    , published : Bool
    , id : Int
    }


type alias Model =
    { books : List Book
    , title : String
    , author : String
    , published : Bool
    , errorMsg : String
    }


init =
    ( Model [] "" "" False "", Cmd.none )


type Msg
    = GetBooks (Result Http.Error (List Book))
    | SetBook (Result Http.Error String)
    | DeleteBook (Result Http.Error String)
    | RequestBooks
    | PostBook
    | DelBook Int
    | GetTitle String
    | GetAuthor String
    | GetPublished
    | NoOp


httpBookCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
httpBookCompleted model result =
    case result of
        Ok json ->
            ( { model | errorMsg = "" } |> Debug.log "Status Complete", Cmd.none )

        Err e ->
            ( { model | errorMsg = (toString e) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetBooks (Ok json) ->
            ( { model | books = json }, Cmd.none )

        GetBooks (Err e) ->
            ( Debug.log (toString e) model, Cmd.none )

        RequestBooks ->
            ( model, getBooks )

        SetBook result ->
            httpBookCompleted model result

        DeleteBook result ->
            httpBookCompleted model result

        PostBook ->
            ( { model | title = "", author = "" }, bookPostCmd model )

        DelBook i ->
            ( { model | books = List.filter (\b -> b.id /= i) model.books }, delete i )

        GetTitle s ->
            ( { model | title = s }, Cmd.none )

        GetAuthor s ->
            ( { model | author = s }, Cmd.none )

        GetPublished ->
            ( { model | published = not model.published }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getBooks : Cmd Msg
getBooks =
    let
        url =
            "http://localhost:8000/api/v1/books"

        req =
            Http.get url decodeBooks
    in
        Http.send GetBooks req


decodeBooks : Json.Decoder (List Book)
decodeBooks =
    Json.at [ "result" ] (Json.list bookDecoder)


bookDecoder : Json.Decoder Book
bookDecoder =
    Json.map4
        Book
        (Json.at [ "title" ] Json.string)
        (Json.at [ "author" ] Json.string)
        (Json.at [ "published" ] Json.bool)
        (Json.at [ "id" ] Json.int)


bookPostCmd : Model -> Cmd Msg
bookPostCmd model =
    Http.send SetBook (addBook model)


addBook : Model -> Http.Request String
addBook model =
    let
        url =
            "http://localhost:8000/api/v1/books"

        body =
            model
                |> bookEncoder
                |> Http.jsonBody
    in
        Http.post url body statusDecoder


bookEncoder : Model -> Encode.Value
bookEncoder model =
    Encode.object
        [ ( "title", Encode.string model.title )
        , ( "author", Encode.string model.author )
        , ( "published", Encode.bool model.published )
        ]


statusDecoder : Json.Decoder String
statusDecoder =
    Json.field "status" Json.string


delete : Int -> Cmd Msg
delete i =
    let
        decoder =
            Json.succeed ""

        a =
            i |> toString

        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = "http://localhost:8000/api/v1/books/" ++ a
                , body = Http.emptyBody
                , expect = Http.expectJson decoder
                , timeout = Maybe.Nothing
                , withCredentials = False
                }
    in
        Http.send DeleteBook request


isChecked : Bool -> Msg
isChecked bool =
    if bool then
        GetPublished
    else
        NoOp


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ CDN.stylesheet
        , cardView model
        ]


cardView : Model -> Html Msg
cardView model =
    Card.config []
        |> Card.header [] [ h2 [] [ text "Elm Rust Book Database" ] ]
        |> Card.block []
            [ Card.custom <| bookForm model
            , Card.custom <|
                Button.button
                    [ Button.onClick RequestBooks
                    , Button.success
                    ]
                    [ text "Get Books!" ]
            , Card.custom <| br [] []
            , Card.custom <| div [] <| List.map bookView model.books
            ]
        |> Card.view


bookView : Book -> Html Msg
bookView book =
    ListGroup.ul
        [ ListGroup.li []
            [ span []
                [ b [] [ text "Title: " ]
                , p [] [ text book.title ]
                ]
            ]
        , ListGroup.li []
            [ span []
                [ b [] [ text "Author: " ]
                , p [] [ text book.author ]
                ]
            ]
        , ListGroup.li [ ListGroup.attrs [ class "justify-content-between" ] ]
            [ span []
                [ b [] [ "Published: " |> text ]
                , p [] [ book.published |> toString |> text ]
                ]
            , Button.button [ Button.onClick (DelBook book.id), Button.danger ] [ text "X" ]
            ]
        ]


bookForm : Model -> Html Msg
bookForm model =
    Form.form []
        [ Form.group
            []
            [ Form.label [ for "title" ] [ text " Title " ]
            , Input.text [ Input.value model.title, Input.onInput GetTitle ]
            ]
        , Form.group
            []
            [ Form.label [ for "author" ] [ text " Author " ]
            , Input.text [ Input.value model.author, Input.onInput GetAuthor ]
            ]
        , Form.group
            []
            [ Form.label [ for "author" ] [ text " Author " ]
            , Checkbox.checkbox [ Checkbox.checked False, Checkbox.onCheck isChecked ] "Published?"
            ]
        , Button.button [ Button.primary, Button.onClick PostBook ] [ text "Submit" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

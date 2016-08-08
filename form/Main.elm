import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (length)
import Regex exposing (contains, regex)

main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
    { name : String
    , passwd: String
    , passwdAgain: String
    , age: String
    , submit: Bool
    }

model : Model
model =
    Model "" "" "" "" False

-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name, submit = False }

        Password passwd ->
            { model | passwd = passwd, submit = False }

        PasswordAgain passwd ->
            { model | passwdAgain = passwd, submit = False }

        Age age ->
            { model | age = age, submit = False }

        Submit ->
            { model | submit = True }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "Name", onInput Name ] []
        , input [ type' "password", placeholder "Password", onInput Password ] []
        , input [ type' "password", placeholder "Password Again", onInput PasswordAgain ] []
        , input [ type' "age", placeholder "Age", onInput Age ] []
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]

viewValidation : Model -> Html Msg
viewValidation model =
    let
        (color, message) =
            if not model.submit then
                ("", "")
            else if length model.passwd < 8 then
                ("red", "Password less than 8 characters")
            else if not (contains (regex "[a-z]") model.passwd) then
                ("red", "Password does not contains lowercase letters")
            else if not (contains (regex "[A-Z]") model.passwd) then
                ("red", "Password does not contains uppercase letters")
            else if not (contains (regex "[0-9]") model.passwd) then
                ("red", "Password does not contains numbers")
            else if model.passwd /= model.passwdAgain then
                ("red", "Password do not match!")
            else if not (contains (regex "^[0-9]+$") model.age) then
                ("red", "Age should only contains numbers")
            else
                ("green", "OK")
    in
        div [ style [("color", color)] ] [ text message ]

module StringValidation exposing
    ( Check
    , Result(..)
    , Validation
    , email
    , greaterThan
    , validate
    , validation
    )

import Regex


type Validation
    = Validation Check String


type alias Check =
    String -> Bool


validation : Check -> String -> Validation
validation =
    Validation


type Result
    = Ok
    | Err String


validate : String -> List Validation -> Result
validate value validations =
    List.foldl
        (\(Validation check err) acc ->
            case acc of
                Err _ ->
                    acc

                Ok ->
                    if check value then
                        Ok

                    else
                        Err err
        )
        Ok
        validations


validateAll : String -> List Validation -> List String
validateAll value validations =
    List.foldr
        (\(Validation check err) acc ->
            if check value then
                err :: acc

            else
                acc
        )
        []
        validations


greaterThan : Int -> Check
greaterThan amount value =
    String.length value > amount


email : Check
email =
    Regex.contains emailRegex


emailRegex : Regex.Regex
emailRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = True, multiline = False }
            "^[A-Z0-9._%+-]{1,64}@(?:[A-Z0-9-]{1,63}\\.){1,125}[A-Z]{2,63}$"

port module BtLelo exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (float)
import Json.Encode as E



-- ports OUT


port initConnection : () -> Cmd msg


port calibrateAccelerometer : () -> Cmd msg


port stopMotors : () -> Cmd msg


port shutdown : () -> Cmd msg


port setMotorSpeed : MotorSpeed -> Cmd msg



-- ports IN


port bluetoothEvent : (E.Value -> msg) -> Sub msg


port foundDevice : (String -> msg) -> Sub msg


port fullyInitiated : (String -> msg) -> Sub msg


port securityAcceptRequired : (Int -> msg) -> Sub msg


port characteristicUpdate : (( String, String ) -> msg) -> Sub msg


port updatePosition : (Position -> msg) -> Sub msg


port updateMotorSpeed : (MotorSpeed -> msg) -> Sub msg


port characteristicIntUpdate : (( String, Int ) -> msg) -> Sub msg


port errorConnecting : (String -> msg) -> Sub msg


port disconnected : (Int -> msg) -> Sub msg


type CharacteristicUpdate
    = BatteryLevel Int
    | Depth Int
    | PositionUpdate Position
    | MotorSpeedUpdate MotorSpeed
    | Other ( String, String )


type BtEvent
    = GotDevice String


type Msg
    = StartConnection
    | GotBtEvent E.Value
    | ErrorConnecting String
    | FoundDevice String
    | NeedAuth
    | FinishedConnection String
    | CharacteristicWasUpdated CharacteristicUpdate
    | DeviceDisconnected
    | RequestedShutdown
    | RequestedStopMotors
    | CalibrateAccelerometer
    | RequestedMotorSpeedChange MotorSpeed
    | Noop


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Initial { error = Nothing }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ bluetoothEvent GotBtEvent
        , errorConnecting ErrorConnecting
        , foundDevice FoundDevice
        , fullyInitiated FinishedConnection
        , securityAcceptRequired (\_ -> NeedAuth)
        , characteristicIntUpdate (characteristicIntUpdateDecoder >> CharacteristicWasUpdated)
        , characteristicUpdate
            (\( k, v ) ->
                String.toInt v
                    |> Maybe.map (\vv -> ( k, vv ))
                    |> Maybe.map (characteristicIntUpdateDecoder >> CharacteristicWasUpdated)
                    |> Maybe.withDefault (CharacteristicWasUpdated (Other ( k, v )))
            )
        , updatePosition (PositionUpdate >> CharacteristicWasUpdated)
        , disconnected (\_ -> DeviceDisconnected)
        , updateMotorSpeed (MotorSpeedUpdate >> CharacteristicWasUpdated)
        ]


type Model
    = Initial { error : Maybe String }
    | ConnectionStarting
    | DeviceFound String
    | AwaitingSecurity
    | Connected ConnectedState
    | Disconnected


type alias Position =
    { x : Int, y : Int, z : Int, direction : Bool }


type alias MotorSpeed =
    { mainMotor : Int, vibrationMotor : Int }


type alias ConnectedState =
    { name : String
    , batteryLevel : Int
    , position : Position
    , depth : Int
    , motorSpeed : MotorSpeed
    , otherCharacteristics : Dict String String
    }


btEventDecoder : D.Decoder BtEvent
btEventDecoder =
    D.andThen
        (\t ->
            case String.toLower t of
                _ ->
                    D.fail ("no decoder for type " ++ t)
        )
        (D.field "type" D.string)


characteristicIntUpdateDecoder : ( String, Int ) -> CharacteristicUpdate
characteristicIntUpdateDecoder ( k, v ) =
    case k of
        "batteryLevel" ->
            BatteryLevel v

        "depth" ->
            Depth v

        _ ->
            Other ( k, String.fromInt v )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartConnection ->
            ( ConnectionStarting, initConnection () )

        ErrorConnecting err ->
            ( Initial { error = Just err }, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        FinishedConnection name ->
            ( Connected { name = name, position = { x = 0, y = 0, z = 0, direction = True }, batteryLevel = 0, depth = 0, motorSpeed = { mainMotor = 0, vibrationMotor = 0 }, otherCharacteristics = Dict.empty }, Cmd.none )

        CharacteristicWasUpdated event ->
            case model of
                Connected m ->
                    ( Connected (updateCharacteristicInState event m), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NeedAuth ->
            ( AwaitingSecurity, Cmd.none )

        -- not really required anymore, practically dead code
        GotBtEvent ev ->
            let
                parsedEv =
                    D.decodeValue btEventDecoder ev
            in
            case model of
                Connected state ->
                    case parsedEv of
                        Ok event ->
                            processEventWhenConnected state event

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FoundDevice name ->
            ( DeviceFound name, Cmd.none )

        DeviceDisconnected ->
            ( Disconnected, Cmd.none )

        RequestedShutdown ->
            ( model, shutdown () )

        RequestedStopMotors ->
            ( model, stopMotors () )

        CalibrateAccelerometer ->
            ( model, calibrateAccelerometer () )

        RequestedMotorSpeedChange newMotorSpeed ->
            ( model, setMotorSpeed newMotorSpeed )


updateCharacteristicInState : CharacteristicUpdate -> ConnectedState -> ConnectedState
updateCharacteristicInState event state =
    case event of
        BatteryLevel level ->
            { state | batteryLevel = level }

        Depth value ->
            { state | depth = value }

        PositionUpdate position ->
            { state | position = position }

        MotorSpeedUpdate ms ->
            Debug.log ("got motor speed update " ++ String.fromInt ms.mainMotor ++ " " ++ String.fromInt ms.vibrationMotor)
                { state | motorSpeed = { mainMotor = motorSpeedToPercentage ms.mainMotor, vibrationMotor = motorSpeedToPercentage ms.vibrationMotor } }

        Other ( k, v ) ->
            { state | otherCharacteristics = Dict.insert k v state.otherCharacteristics }


processEventWhenConnected : ConnectedState -> BtEvent -> ( Model, Cmd Msg )
processEventWhenConnected state msg =
    ( Connected state, Cmd.none )


disableInitConnection : Model -> Bool
disableInitConnection model =
    case model of
        Initial _ ->
            False

        Disconnected ->
            False

        _ ->
            True


isConnected : Model -> Bool
isConnected model =
    case model of
        Connected _ ->
            True

        _ ->
            False


motorSpeedToPercentage : Int -> Int
motorSpeedToPercentage spd =
    ceiling (toFloat spd / 0.64)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Bluetooth Lelo F1S v2 connector" ]
        , if isConnected model then
            text ""

          else
            button [ onClick StartConnection, disabled (disableInitConnection model) ] [ text "Init connection" ]
        , p []
            [ case model of
                Initial { error } ->
                    case error of
                        Just err ->
                            div []
                                [ text "There was an error. Your device was disconnected. Please reload the page and restart the pairing process."
                                , br [] []
                                , text ("Error message: " ++ err)
                                ]

                        Nothing ->
                            text "Hold the power button on your Lelo device until it starts blinking. Then push 'Init connection'"

                ConnectionStarting ->
                    text "Please choose device to connect to."

                Connected state ->
                    let
                        ms =
                            state.motorSpeed

                        updateMainMotorSpeed add =
                            RequestedMotorSpeedChange { ms | mainMotor = ms.mainMotor + add }

                        updateVibrationMotorSpeed add =
                            RequestedMotorSpeedChange { ms | vibrationMotor = ms.vibrationMotor + add }
                    in
                    div []
                        [ text ("Connected to " ++ state.name)
                        , br [] []
                        , text ("Battery level: " ++ String.fromInt state.batteryLevel)
                        , br [] []
                        , text
                            ("Position (x, y, z): ("
                                ++ String.fromInt state.position.x
                                ++ ", "
                                ++ String.fromInt state.position.y
                                ++ ", "
                                ++ String.fromInt state.position.z
                                ++ ")."
                                ++ "Turned upwards: "
                                ++ (if state.position.direction then
                                        "true"

                                    else
                                        "false"
                                   )
                            )
                        , br [] []
                        , text
                            ("Depth: "
                                ++ String.fromInt state.depth
                                ++ "/8"
                            )
                        , br [] []
                        , text ("Main motor speed: " ++ String.fromInt state.motorSpeed.mainMotor)
                        , br [] []
                        , text ("Vibration motor speed: " ++ String.fromInt state.motorSpeed.vibrationMotor)
                        , div []
                            [ text "Would you like to:"
                            , br [] []
                            , button [ onClick RequestedStopMotors ] [ text "Stop motors" ]
                            , button [ onClick CalibrateAccelerometer ] [ text "Calibrate accelerometer" ]
                            , button [ onClick RequestedShutdown ] [ text "Shutdown device" ]
                            ]
                        , br [] []
                        , div []
                            [ text "Change motor speed: "
                            , br [] []
                            , button [ onClick (updateMainMotorSpeed -20) ] [ text " - " ]
                            , text "Main motor"
                            , button [ onClick (updateMainMotorSpeed 20) ] [ text " + " ]
                            , span [] [ text "      " ]
                            , button [ onClick (updateVibrationMotorSpeed -20) ] [ text " - " ]
                            , text "Vibration motor"
                            , button [ onClick (updateVibrationMotorSpeed 20) ] [ text " + " ]
                            ]
                        ]

                AwaitingSecurity ->
                    text "Press the power button again to pair your Lelo F1S."

                DeviceFound name ->
                    text ("Connecting to " ++ name ++ ". Please, wait...")

                Disconnected ->
                    text "Your device disconnected, please reconnect."
            ]
        ]

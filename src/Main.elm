port module Main exposing (..)
import Audio exposing (Audio, AudioCmd, AudioData)
import Html exposing (..)
import Time
import Json.Decode
import Json.Encode
import Html.Events exposing (onClick)
import Html.Attributes exposing (value)

type alias Model =
    {
      state: MetronomeState
    , bpm : Float
    , fileStatus: FileStatus
    }

type MetronomeState 
    = Started
    | Stopped


type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    }

type FileStatus
    = Loaded LoadedModel_
    | Failed
    | NotStarted

type Msg
    = Tick Time.Posix
    | IncreaseSpeed
    | DecreaseSpeed
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | Stop
    | Start

type SoundState
    = NotPlaying
    | Playing Time.Posix

init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init _ = 
    ( { bpm = 120, fileStatus = NotStarted, state = Stopped }
    , Cmd.none
    , Audio.loadAudio
        SoundLoaded
        "tick.mp3"
    )

update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg)
update _ msg model =
    case (msg, model.fileStatus) of
        (Tick time, Loaded loadedModel) -> 
            ({model | fileStatus = Loaded { loadedModel | soundState = Playing time} }, Cmd.none, Audio.cmdNone)
        (Tick _, _) ->
            (model, Cmd.none, Audio.cmdNone)
        (IncreaseSpeed, _) -> ({model | bpm = model.bpm + 10}, Cmd.none, Audio.cmdNone)
        (DecreaseSpeed, _) -> ({model | bpm = model.bpm - 10}, Cmd.none, Audio.cmdNone)
        (SoundLoaded result, _) ->
            case result of
                Ok sound -> ({model | fileStatus = Loaded {sound = sound, soundState = NotPlaying }}, Cmd.none, Audio.cmdNone)
                Err _ -> ({model | fileStatus = Failed}, Cmd.none, Audio.cmdNone)
        (Start, _) ->
            ({model | state = Started}, Cmd.none, Audio.cmdNone)
        (Stop, _) ->
            ({model | state = Stopped}, Cmd.none, Audio.cmdNone)

second : Float
second =
    1000
minuteInSecond : Float
minuteInSecond = 
    60 * second

speedFromBPM : Float -> Float
speedFromBPM bpm =
    minuteInSecond / bpm

subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ model =
    case model.state of
        Started ->
            Time.every (speedFromBPM model.bpm) Tick
        Stopped ->
            Sub.none

view : AudioData -> Model -> Html Msg
view _ model =
    case model.fileStatus of
        Loaded _ ->
            div [][
                text ("BPM:")
                , input [value (String.fromFloat model.bpm)][]
                , button [onClick IncreaseSpeed][text "+"]
                , button [onClick DecreaseSpeed][text "-"]
                , div[][
                    button [onClick Start][text "Start"]
                    , button [onClick Stop][text "Stop"]
                ]
            ]
        Failed ->
            div [][
                text "Failed"
            ]
        NotStarted ->
            div [][
                text "Loading"
            ]

audio : AudioData -> Model -> Audio
audio _ model =
    case model.fileStatus of
        Loaded loadedModel ->
            case loadedModel.soundState of
                NotPlaying -> Audio.silence
                Playing time -> Audio.audio loadedModel.sound time
        _ ->
            Audio.silence

port audioPortToJS : Json.Encode.Value -> Cmd msg
port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

main : Platform.Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }
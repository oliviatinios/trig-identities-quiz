module Main exposing (main)

import ArcCreator
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import PolygonCreator
import QuizGame
import SinCreator exposing (..)
import TextCreator exposing (..)
import TriCreator



main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Shape Creator"
        , view = view
        , update = update
        }


type Pages
    = QuizGame
    | TriCreator
    | PolygonCreator
    | ArcCreator
    | SinCreator
    | TextCreator


init =
    { page = QuizGame
    , model1 = QuizGame.init
    , model2 = TriCreator.init
    , model3 = PolygonCreator.init
    , model4 = ArcCreator.init
    , model5 = SinCreator.init
    , model6 = TextCreator.init
    , oneSat = 1
    , twoSat = 0
    , threeSat = 0
    , fourSat = 0
    , fiveSat = 0
    , sixSat = 0
    , currentPage = 1
    }


oneColour model =
    hsl (degrees 30) model.oneSat 0.85


oneAccent model =
    hsl (degrees 22) model.oneSat 0.6


twoColour model =
    hsl (degrees 285) model.twoSat 0.85


twoAccent model =
    hsl (degrees 280) model.twoSat 0.6


threeColour model =
    hsl (degrees 250) model.threeSat 0.9


threeAccent model =
    hsl (degrees 245) model.threeSat 0.6


fourColour model =
    hsl (degrees 135) model.fourSat 0.9


fourAccent model =
    hsl (degrees 130) model.fourSat 0.6


fiveColour model =
    hsl (degrees 0) model.fiveSat 0.9


fiveAccent model =
    hsl (degrees 355) model.fiveSat 0.75


sixColour model =
    hsl (degrees 180) model.sixSat 0.85


sixAccent model =
    hsl (degrees 180) model.sixSat 0.5


type Msg m1 m2 m3 m4 m5 m6
    = Tick Float GetKeyState
    | Msg1 (QuizGame.Msg m1)
    | Msg2 (TriCreator.Msg m2)
    | Msg3 (PolygonCreator.Msg m3)
    | Msg4 (ArcCreator.Msg m4)
    | Msg5 (SinCreator.Msg m5)
    | Msg6 (TextCreator.Msg m6)
    | Goto1
    | Goto2
    | Goto3
    | Goto4
    | Goto5
    | Goto6
    | In1
    | In2
    | In3
    | In4
    | In5
    | In6
    | Out1
    | Out2
    | Out3
    | Out4
    | Out5
    | Out6
    | MoveInRect ( Float, Float )


view model =
    -- collage 512 380 <|
    collage 640 380 <|
        (case model.page of
            QuizGame ->
                List.map (map Msg1) (QuizGame.view model.model1)

            TriCreator ->
                List.map (map Msg2) (TriCreator.view model.model2)

            PolygonCreator ->
                List.map (map Msg3) (PolygonCreator.view model.model3)

            ArcCreator ->
                List.map (map Msg4) (ArcCreator.view model.model4)

            SinCreator ->
                List.map (map Msg5) (SinCreator.view model.model5)

            TextCreator ->
                List.map (map Msg6) (TextCreator.view model.model6)
        )
            ++ [ rect 200 70 |> filled blank |> move ( 0, -190 ) |> notifyMouseMoveAt MoveInRect ]
            ++ [ group
                    [ rectangle 150 50
                        |> filled (oneColour model)
                        |> addOutline (solid 2)
                            (if model.page == QuizGame then
                                orange

                             else if model.oneSat == 0 then
                                blank

                             else
                                orange
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, 120 )
                    , text "Fundamental Identities" |> bold |> sansserif |> centered |> filled (oneAccent model) |> move ( -230, 115 )
                    ]
                    |> notifyTap Goto1
                    |> notifyEnter In1
                    |> notifyLeave Out1
               , group
                    [ rectangle 150 50
                        |> filled (twoColour model)
                        |> addOutline (solid 2)
                            (if model.page == TriCreator then
                                purple

                             else if model.twoSat == 0 then
                                blank

                             else
                                purple
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, 60 )
                    , text "Pythagorean Identities" |> bold |> sansserif |> centered |> filled (twoAccent model) |> move ( -230, 55 )
                    ]
                    |> notifyTap Goto2
                    |> notifyEnter In2
                    |> notifyLeave Out2
               , group
                    [ rectangle 150 50
                        |> filled (threeColour model)
                        |> addOutline (solid 2)
                            (if model.page == PolygonCreator then
                                darkBlue

                             else if model.threeSat == 0 then
                                blank

                             else
                                darkBlue
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, 0 )
                    , text "Reciprocal Identities" |> bold |> sansserif |> centered |> filled (threeAccent model) |> move ( -230, -5 )
                    ]
                    |> notifyTap Goto3
                    |> notifyEnter In3
                    |> notifyLeave Out3
               , group
                    [ rectangle 150 50
                        |> filled (fourColour model)
                        |> addOutline (solid 2)
                            (if model.page == ArcCreator then
                                darkGreen

                             else if model.fourSat == 0 then
                                blank

                             else
                                darkGreen
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, -60 )
                    , text "Product Identities" |> bold |> sansserif |> centered |> filled (fourAccent model) |> move ( -230, -65 )
                    ]
                    |> notifyTap Goto4
                    |> notifyEnter In4
                    |> notifyLeave Out4
               , group
                    [ rectangle 150 50
                        |> filled (fiveColour model)
                        |> addOutline (solid 2)
                            (if model.page == SinCreator then
                                darkRed

                             else if model.fiveSat == 0 then
                                blank

                             else
                                darkRed
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, -120 )
                    , text "Other Formulas" |> bold |> sansserif |> centered |> filled (fiveAccent model) |> move ( -230, -125 )
                    ]
                    |> notifyTap Goto5
                    |> notifyEnter In5
                    |> notifyLeave Out5
               ]


distTo pointA pointB =
    sqrt ((Tuple.first pointB - Tuple.first pointA) ^ 2 + (Tuple.second pointB - Tuple.second pointA) ^ 2)


update msg model =
    case msg of
        In1 ->
            { model | oneSat = 1 }

        In2 ->
            { model | twoSat = 1 }

        In3 ->
            { model | threeSat = 1 }

        In4 ->
            { model | fourSat = 1 }

        In5 ->
            { model | fiveSat = 1 }

        In6 ->
            { model | sixSat = 1 }

        Out1 ->
            { model
                | oneSat =
                    case model.page of
                        QuizGame ->
                            model.oneSat

                        _ ->
                            0
            }

        Out2 ->
            { model
                | twoSat =
                    case model.page of
                        TriCreator ->
                            model.twoSat

                        _ ->
                            0
            }

        Out3 ->
            { model
                | threeSat =
                    case model.page of
                        PolygonCreator ->
                            model.threeSat

                        _ ->
                            0
            }

        Out4 ->
            { model
                | fourSat =
                    case model.page of
                        ArcCreator ->
                            model.fourSat

                        _ ->
                            0
            }

        Out5 ->
            { model
                | fiveSat =
                    case model.page of
                        SinCreator ->
                            model.fiveSat

                        _ ->
                            0
            }

        Out6 ->
            { model
                | sixSat =
                    case model.page of
                        TextCreator ->
                            model.sixSat

                        _ ->
                            0
            }

        _ ->
            case model.page of
                QuizGame ->
                    case msg of
                        Tick f g ->
                            { model | model1 = QuizGame.update (QuizGame.Tick f g) model.model1 }

                        Msg1 m1 ->
                            { model | model1 = QuizGame.update m1 model.model1 }

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Msg6 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = QuizGame
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = TriCreator
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = PolygonCreator
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ArcCreator
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = SinCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , sixSat = 0
                            }

                        Goto6 ->
                            { model
                                | page = TextCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        _ ->
                            model

                TriCreator ->
                    case msg of
                        Tick f g ->
                            { model | model2 = TriCreator.update (TriCreator.Tick f g) model.model2 }

                        Msg1 _ ->
                            model

                        Msg2 m2 ->
                            { model | model2 = TriCreator.update m2 model.model2 }

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Msg6 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = QuizGame
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = TriCreator
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = PolygonCreator
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ArcCreator
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = SinCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , sixSat = 0
                            }

                        Goto6 ->
                            { model
                                | page = TextCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        _ ->
                            model

                PolygonCreator ->
                    case msg of
                        Tick f g ->
                            { model | model3 = PolygonCreator.update (PolygonCreator.Tick f g) model.model3 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 m3 ->
                            { model | model3 = PolygonCreator.update m3 model.model3 }

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Msg6 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = QuizGame
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = TriCreator
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = PolygonCreator
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ArcCreator
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = SinCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , sixSat = 0
                            }

                        Goto6 ->
                            { model
                                | page = TextCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        _ ->
                            model

                ArcCreator ->
                    case msg of
                        Tick f g ->
                            { model | model4 = ArcCreator.update (ArcCreator.Tick f g) model.model4 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 m4 ->
                            { model | model4 = ArcCreator.update m4 model.model4 }

                        Msg5 _ ->
                            model

                        Msg6 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = QuizGame
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = TriCreator
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = PolygonCreator
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ArcCreator
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = SinCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , sixSat = 0
                            }

                        Goto6 ->
                            { model
                                | page = TextCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        _ ->
                            model

                SinCreator ->
                    case msg of
                        Tick f g ->
                            { model | model5 = SinCreator.update (SinCreator.Tick f g) model.model5 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 m5 ->
                            { model | model5 = SinCreator.update m5 model.model5 }

                        Msg6 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = QuizGame
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = TriCreator
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = PolygonCreator
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ArcCreator
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = SinCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , sixSat = 0
                            }

                        Goto6 ->
                            { model
                                | page = TextCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        _ ->
                            model

                TextCreator ->
                    case msg of
                        Tick f g ->
                            { model | model6 = TextCreator.update (TextCreator.Tick f g) model.model6 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Msg6 m6 ->
                            { model | model6 = TextCreator.update m6 model.model6 }

                        Goto1 ->
                            { model
                                | page = QuizGame
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = TriCreator
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = PolygonCreator
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ArcCreator
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                                , sixSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = SinCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , sixSat = 0
                            }

                        Goto6 ->
                            { model
                                | page = TextCreator
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        _ ->
                            model

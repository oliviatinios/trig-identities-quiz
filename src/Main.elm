module Main exposing (main)

import ProductIdentities
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ReciprocalIdentities
import FundamentalIdentities
import Summary exposing (..)
import PythagoreanIdentities



main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Trig Identites"
        , view = view
        , update = update
        }


type Pages
    = Home
    | FundamentalIdentities
    | PythagoreanIdentities
    | ReciprocalIdentities
    | ProductIdentities
    | Summary


init =
    { page = Home
    , model1 = FundamentalIdentities.init
    , model2 = PythagoreanIdentities.init
    , model3 = ReciprocalIdentities.init
    , model4 = ProductIdentities.init
    , model5 = Summary.init
    , oneSat = 0
    , twoSat = 0
    , threeSat = 0
    , fourSat = 0
    , fiveSat = 0
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
    hsl (degrees 355) model.fiveSat 0.6


type Msg m1 m2 m3 m4 m5
    = Tick Float GetKeyState
    | Msg1 (FundamentalIdentities.Msg m1)
    | Msg2 (PythagoreanIdentities.Msg m2)
    | Msg3 (ReciprocalIdentities.Msg m3)
    | Msg4 (ProductIdentities.Msg m4)
    | Msg5 (Summary.Msg m5)
    | Goto1
    | Goto2
    | Goto3
    | Goto4
    | Goto5
    | In1
    | In2
    | In3
    | In4
    | In5
    | Out1
    | Out2
    | Out3
    | Out4
    | Out5
    | MoveInRect ( Float, Float )


view model =
    -- collage 512 380 <|
    collage 640 380 <|
        (case model.page of
            Home ->
                [ rectangle 400 300 |> filled blank |> addOutline (solid 2) charcoal |> move ( 55, 0 )
                , text "Welcome!" |> size 20 |> bold |> filled charcoal |> move ( 10, 10 )
                , text "Please select a lesson in the left panel to get started." |> size 16 |> filled charcoal |> move ( -110, -10 )
                ]
            FundamentalIdentities ->
                List.map (map Msg1) (FundamentalIdentities.view model.model1)

            PythagoreanIdentities ->
                List.map (map Msg2) (PythagoreanIdentities.view model.model2)

            ReciprocalIdentities ->
                List.map (map Msg3) (ReciprocalIdentities.view model.model3)

            ProductIdentities ->
                List.map (map Msg4) (ProductIdentities.view model.model4)

            Summary ->
                List.map (map Msg5) (Summary.view model.model5)

        )
            ++ [ rect 200 70 |> filled blank |> move ( 0, -190 ) |> notifyMouseMoveAt MoveInRect ]
            ++ [ group
                    [ rectangle 150 60
                        |> filled (oneColour model)
                        |> addOutline (solid 2)
                            (if model.page == FundamentalIdentities then
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
                    [ rectangle 150 60
                        |> filled (twoColour model)
                        |> addOutline (solid 2)
                            (if model.page == PythagoreanIdentities then
                                purple

                             else if model.twoSat == 0 then
                                blank

                             else
                                purple
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, 40 )
                    , text "Pythagorean Identities" |> bold |> sansserif |> centered |> filled (twoAccent model) |> move ( -230, 35 )
                    ]
                    |> notifyTap Goto2
                    |> notifyEnter In2
                    |> notifyLeave Out2
               , group
                    [ rectangle 150 60
                        |> filled (threeColour model)
                        |> addOutline (solid 2)
                            (if model.page == ReciprocalIdentities then
                                darkBlue

                             else if model.threeSat == 0 then
                                blank

                             else
                                darkBlue
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, -40 )
                    , text "Reciprocal Identities" |> bold |> sansserif |> centered |> filled (threeAccent model) |> move ( -230, -45 )
                    ]
                    |> notifyTap Goto3
                    |> notifyEnter In3
                    |> notifyLeave Out3
               , group
                    [ rectangle 150 60
                        |> filled (fourColour model)
                        |> addOutline (solid 2)
                            (if model.page == ProductIdentities then
                                darkGreen

                             else if model.fourSat == 0 then
                                blank

                             else
                                darkGreen
                            )
                        |> makeTransparent 0.7
                        |> move ( -230, -120 )
                    , text "Product Identities" |> bold |> sansserif |> centered |> filled (fourAccent model) |> move ( -230, -125 )
                    ]
                    |> notifyTap Goto4
                    |> notifyEnter In4
                    |> notifyLeave Out4
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

        Out1 ->
            { model
                | oneSat =
                    case model.page of
                        FundamentalIdentities ->
                            model.oneSat

                        _ ->
                            0
            }

        Out2 ->
            { model
                | twoSat =
                    case model.page of
                        PythagoreanIdentities ->
                            model.twoSat

                        _ ->
                            0
            }

        Out3 ->
            { model
                | threeSat =
                    case model.page of
                        ReciprocalIdentities ->
                            model.threeSat

                        _ ->
                            0
            }

        Out4 ->
            { model
                | fourSat =
                    case model.page of
                        ProductIdentities ->
                            model.fourSat

                        _ ->
                            0
            }

        Out5 ->
            { model
                | fiveSat =
                    case model.page of
                        Summary ->
                            model.fiveSat

                        _ ->
                            0
            }

        _ ->
            case model.page of
                Home ->
                    case msg of
                        Goto1 ->
                            { model
                                | page = FundamentalIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = PythagoreanIdentities
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = ReciprocalIdentities
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ProductIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = Summary
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                            }
                        _ ->
                            model

                FundamentalIdentities ->
                    case msg of
                        Tick f g ->
                            { model | model1 = FundamentalIdentities.update (FundamentalIdentities.Tick f g) model.model1 }

                        Msg1 m1 ->
                            { model | model1 = FundamentalIdentities.update m1 model.model1 }

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = FundamentalIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = PythagoreanIdentities
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = ReciprocalIdentities
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ProductIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = Summary
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                            }

                        _ ->
                            model

                PythagoreanIdentities ->
                    case msg of
                        Tick f g ->
                            { model | model2 = PythagoreanIdentities.update (PythagoreanIdentities.Tick f g) model.model2 }

                        Msg1 _ ->
                            model

                        Msg2 m2 ->
                            { model | model2 = PythagoreanIdentities.update m2 model.model2 }

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = FundamentalIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = PythagoreanIdentities
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = ReciprocalIdentities
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ProductIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = Summary
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                            }

                        _ ->
                            model

                ReciprocalIdentities ->
                    case msg of
                        Tick f g ->
                            { model | model3 = ReciprocalIdentities.update (ReciprocalIdentities.Tick f g) model.model3 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 m3 ->
                            { model | model3 = ReciprocalIdentities.update m3 model.model3 }

                        Msg4 _ ->
                            model

                        Msg5 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = FundamentalIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = PythagoreanIdentities
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = ReciprocalIdentities
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ProductIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = Summary
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                            }

                        _ ->
                            model

                ProductIdentities ->
                    case msg of
                        Tick f g ->
                            { model | model4 = ProductIdentities.update (ProductIdentities.Tick f g) model.model4 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 m4 ->
                            { model | model4 = ProductIdentities.update m4 model.model4 }

                        Msg5 _ ->
                            model

                        Goto1 ->
                            { model
                                | page = FundamentalIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = PythagoreanIdentities
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = ReciprocalIdentities
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ProductIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = Summary
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                            }

                        _ ->
                            model

                Summary ->
                    case msg of
                        Tick f g ->
                            { model | model5 = Summary.update (Summary.Tick f g) model.model5 }

                        Msg1 _ ->
                            model

                        Msg2 _ ->
                            model

                        Msg3 _ ->
                            model

                        Msg4 _ ->
                            model

                        Msg5 m5 ->
                            { model | model5 = Summary.update m5 model.model5 }

                        Goto1 ->
                            { model
                                | page = FundamentalIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto2 ->
                            { model
                                | page = PythagoreanIdentities
                                , oneSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto3 ->
                            { model
                                | page = ReciprocalIdentities
                                , twoSat = 0
                                , oneSat = 0
                                , fourSat = 0
                                , fiveSat = 0
                            }

                        Goto4 ->
                            { model
                                | page = ProductIdentities
                                , twoSat = 0
                                , threeSat = 0
                                , oneSat = 0
                                , fiveSat = 0
                            }

                        Goto5 ->
                            { model
                                | page = Summary
                                , twoSat = 0
                                , threeSat = 0
                                , fourSat = 0
                                , oneSat = 0
                            }

                        _ ->
                            model

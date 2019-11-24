module FundamentalIdentities exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import String exposing (..)



init =
    { time = 0
    , notify = NotifyTap
    , answer = Default
    , step = Step1
    , question = Question1
    }

type Choices = Default | Incorrect | Correct

type Steps
    = Step1
    | Step2
    | Step3
    | Step4
    | Step5

type Questions
    = Question1
    | Question2

-- change you app's state based on your new messages
update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
            }

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            { model | notify = notif }

        WrongAnswer
            -> { model
                | answer = Incorrect
                }

        RightAnswer
            -> { model
                | answer = Correct
                }

        GoToStep2
            -> { model
                | answer = Default
                , step = Step2
                }

        GoToStep3
            -> { model
                | answer = Default
                , step = Step3
                }

        GoToStep4
            -> { model
                | answer = Default
                , step = Step4
                }

        GoToStep5
            -> { model
                | answer = Default
                , step = Step5
                }

        GoToQuestion1
            -> { model
                | answer = Default
                , step = Step1
                , question = Question1
                }

        GoToQuestion2
            -> { model
                | answer = Default
                , step = Step1
                , question = Question2
                }



--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads

view model =
    (case model.question of
        Question1 ->
            (case model.step of
                Step1 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 1" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap GoToQuestion2
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "What is the first step?" |> size 12 |> filled orange |> move ( -130, 65 )
                    , text "a) tan y * (sin y / cos y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 45 )
                        |> notifyTap WrongAnswer
                    , text "b) tan y * (cos y / sin y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 25 )
                        |> notifyTap WrongAnswer
                    , text "c) (cos y / sin y) * (1 / sin y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 5 )
                        |> notifyTap WrongAnswer
                    , text "d) (sin y / cos y) * (1 / sin y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -15 )
                        |> notifyTap RightAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct"
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -40 )
                    , rectangle 60 25
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then orange
                                            else blank
                                    )
                        |> move ( -100, -60 )
                        |> notifyTap GoToStep2
                    , text "Next"
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then white
                                            else blank
                                    )
                        |> move ( -112, -63 )
                        |> notifyTap GoToStep2
                    ]

                Step2 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 1" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap GoToQuestion2
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
                    , text "What is the second step?" |> size 12 |> filled orange |> move ( -130, 45 )
                    , text "a) sin y / cos y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 25 )
                        |> notifyTap WrongAnswer
                    , text "b) 1 / cos y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 5 )
                        |> notifyTap RightAnswer
                    , text "c) 1 / sin y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -15 )
                        |> notifyTap WrongAnswer
                    , text "d) (sin y * cos y) / (cos2 y * sin y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -35 )
                        |> notifyTap WrongAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct"
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -60 )
                    , rectangle 60 25
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then orange
                                            else blank
                                    )
                        |> move ( -100, -80 )
                        |> notifyTap GoToStep3
                    , text "Next"
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then white
                                            else blank
                                    )
                        |> move ( -112, -83 )
                        |> notifyTap GoToStep3
                    ]

                Step3 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 1" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap GoToQuestion2
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
                    , text "Step 2: 1 / cos y" |> size 12 |> filled orange |> move ( -130, 55 )
                    , text "What is the third step?" |> size 12 |> filled orange |> move ( -130, 15 )
                    , text "a) sec y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -5 )
                        |> notifyTap RightAnswer
                    , text "b) tan y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -25 )
                        |> notifyTap WrongAnswer
                    , text "c) cos y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -45 )
                        |> notifyTap WrongAnswer
                    , text "d) sin y * cos y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -65 )
                        |> notifyTap WrongAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct! You have solved the problem."
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -85 )
                    ]
                Step4 -> []
                Step5 -> []
            )

        Question2 ->
            (case model.step of
                Step1 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that sin y + sin y * cot2 y = cscy" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "What is the first step?" |> size 12 |> filled orange |> move ( -130, 65 )
                    , text "a) sin y + sin y * (1 / tan2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 45 )
                        |> notifyTap WrongAnswer
                    , text "b) sin y * (1 + cot2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 25 )
                        |> notifyTap RightAnswer
                    , text "c) sin y + (sin y / tan2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 5 )
                        |> notifyTap WrongAnswer
                    , text "d) sin y + ( (sin y * cos2 y) / sin2 y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -15 )
                        |> notifyTap WrongAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct"
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -40 )
                    , rectangle 60 25
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then orange
                                            else blank
                                    )
                        |> move ( -100, -60 )
                        |> notifyTap GoToStep2
                    , text "Next"
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then white
                                            else blank
                                    )
                        |> move ( -112, -63 )
                        |> notifyTap GoToStep2
                    ]

                Step2 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "Step 1: sin y * (1 + cot2 y)" |> size 12 |> filled orange |> move ( -130, 75 )
                    , text "What is the second step?" |> size 12 |> filled orange |> move ( -130, 45 )
                    , text "a) sin y * (1 + (cos2 y / sin2 y))"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 25 )
                        |> notifyTap WrongAnswer
                    , text "b) sin y * (1 + (sin2 y / cos2 y))"
                        |> size 12
                        |> filled orange
                        |> move ( -130, 5 )
                        |> notifyTap WrongAnswer
                    , text "c) sin y * (csc2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -15 )
                        |> notifyTap RightAnswer
                    , text "d) sin y * (cos2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -35 )
                        |> notifyTap WrongAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct"
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -60 )
                    , rectangle 60 25
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then orange
                                            else blank
                                    )
                        |> move ( -100, -80 )
                        |> notifyTap GoToStep3
                    , text "Next"
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then white
                                            else blank
                                    )
                        |> move ( -112, -83 )
                        |> notifyTap GoToStep3
                    ]

                Step3 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
                    , text "Step 2: sin y * (csc2 y)" |> size 12 |> filled orange |> move ( -130, 55 )
                    , text "What is the third step?" |> size 12 |> filled orange |> move ( -130, 15 )
                    , text "a) sin y * (1 / cos2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -5 )
                        |> notifyTap WrongAnswer
                    , text "b) sin y * (cos2 y / sin2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -25 )
                        |> notifyTap WrongAnswer
                    , text "c) sin2 y / cosy"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -45 )
                        |> notifyTap WrongAnswer
                    , text "d) sin y / sin2 y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -65 )
                        |> notifyTap RightAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct"
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -90 )
                    , rectangle 60 25
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then orange
                                            else blank
                                    )
                        |> move ( -100, -110 )
                        |> notifyTap GoToStep4
                    , text "Next"
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then white
                                            else blank
                                    )
                        |> move ( -112, -113 )
                        |> notifyTap GoToStep4
                    ]
                Step4 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
                    , text "Step 2: sin y * (csc2 y)" |> size 12 |> filled orange |> move ( -130, 55 )
                    , text "Step 3: sin y / sin2 y" |> size 12 |> filled orange |> move ( -130, 35 )
                    , text "What is the fourth step?" |> size 12 |> filled orange |> move ( -130, 5 )
                    , text "a) 1 / sin y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -15 )
                        |> notifyTap RightAnswer
                    , text "b) sin y / (1 + tan2 y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -35 )
                        |> notifyTap WrongAnswer
                    , text "c) (1 + sin y) / sin y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -55 )
                        |> notifyTap WrongAnswer
                    , text "d) 1 / sin2 y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -75 )
                        |> notifyTap WrongAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct"
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -100 )
                    , rectangle 60 25
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then orange
                                            else blank
                                    )
                        |> move ( -100, -120 )
                        |> notifyTap GoToStep5
                    , text "Next"
                        |> filled (if model.answer == Incorrect
                                        then blank
                                        else if model.answer == Correct
                                            then white
                                            else blank
                                    )
                        |> move ( -112, -123 )
                        |> notifyTap GoToStep5
                    ]
                Step5 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
                    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
                    , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
                    , text "Step 2: sin y * (csc2 y)" |> size 12 |> filled orange |> move ( -130, 55 )
                    , text "Step 3: sin y / sin2 y" |> size 12 |> filled orange |> move ( -130, 35 )
                    , text "Step 4: 1 / sin y" |> size 12 |> filled orange |> move ( -130, 15 )
                    , text "What is the fifth step?" |> size 12 |> filled orange |> move ( -130, -25 )
                    , text "a) cos y / sin2 y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -45 )
                        |> notifyTap WrongAnswer
                    , text "b) csc y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -65 )
                        |> notifyTap RightAnswer
                    , text "c) tan y / (sin y * tan y)"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -85 )
                        |> notifyTap WrongAnswer
                    , text "d) cot y"
                        |> size 12
                        |> filled orange
                        |> move ( -130, -105 )
                        |> notifyTap WrongAnswer
                    , text (if model.answer == Incorrect
                                then "Incorrect"
                                else if model.answer == Correct
                                    then "Correct! You have solved the problem."
                                    else ""
                            )
                        |> size 12
                        |> filled (if model.answer == Incorrect
                                        then red
                                        else if model.answer == Correct
                                            then green
                                            else white
                                    )
                        |> move ( -130, -130 )
                    ]
            )
    )
    ++ [ group 
            [ circle 12 |> filled blank |> addOutline (solid 3) orange |> makeTransparent 0.75 |> move ( 234, 125 )
            , text "?" |> bold |> sansserif |> size 20 |> filled orange |> makeTransparent 0.75 |> move ( 228, 118 ) ]
       ]

-- messages generated by the framework (Tick) and by user interactions
-- note that we let elm figure out the type of the model by making it a type parameter, m


type Msg m
    = Tick Float GetKeyState
    | Notif Notifications
    | WrongAnswer
    | RightAnswer
    | GoToStep2
    | GoToStep3
    | GoToStep4
    | GoToStep5
    | GoToQuestion1
    | GoToQuestion2


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt




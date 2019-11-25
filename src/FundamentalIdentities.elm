module FundamentalIdentities exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import String exposing (..)
import Tuple



init =
    { time = 0
    , notify = NotifyTap
    , answer = Default
    , step = Step1
    , question = Question1
    , hintState = NoPopUp
    }

type HintState = NoPopUp | PopUp Questions Steps

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
 --   | Question3
--    | Question4
--    | Question5


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

        NextStep
            -> { model
                | answer = Default
                , step = 
                    case model.step of
                        Step1 -> Step2
                        Step2 -> Step3
                        Step3 -> Step4
                        Step4 -> Step5
                        Step5 -> Step5
                }

        NextQuestion
            -> { model
                | answer = Default
                , step = Step1
                , question = 
                    case model.question of
                        Question1 -> Question2
                        Question2 -> Question1
                        -- Question3 -> Question4
                        -- Question4 -> Question5
                        -- Question5 -> Question1
                }

        PreviousQuestion
            -> { model
                | answer = Default
                , step = Step1
                , question = 
                    case model.question of
                        Question2 -> Question1
                        Question1 -> Question1
                        -- Add to this if more questions are added
                }

        ClickedHint question step -> {model | hintState = PopUp question step }

        ExitHint -> {model | hintState = NoPopUp }

--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads

view model =
    -- (case model.question of
    --     Question1 ->
    --         (case model.step of
    --             Step1 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 1" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap GoToQuestion2
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "What is the first step?" |> size 12 |> filled orange |> move ( -130, 65 )
    --                 , text "a) tan y * (sin y / cos y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 45 )
    --                     |> notifyTap WrongAnswer
    --                 , text "b) tan y * (cos y / sin y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 25 )
    --                     |> notifyTap WrongAnswer
    --                 , text "c) (cos y / sin y) * (1 / sin y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 5 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) (sin y / cos y) * (1 / sin y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -15 )
    --                     |> notifyTap RightAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct"
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -40 )
    --                 , rectangle 60 25
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then orange
    --                                         else blank
    --                                 )
    --                     |> move ( -100, -60 )
    --                     |> notifyTap GoToStep2
    --                 , text "Next"
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then white
    --                                         else blank
    --                                 )
    --                     |> move ( -112, -63 )
    --                     |> notifyTap GoToStep2
    --                 ]

    --             Step2 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 1" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap GoToQuestion2
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
    --                 , text "What is the second step?" |> size 12 |> filled orange |> move ( -130, 45 )
    --                 , text "a) sin y / cos y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 25 )
    --                     |> notifyTap WrongAnswer
    --                 , text "b) 1 / cos y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 5 )
    --                     |> notifyTap RightAnswer
    --                 , text "c) 1 / sin y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -15 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) (sin y * cos y) / (cos2 y * sin y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -35 )
    --                     |> notifyTap WrongAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct"
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -60 )
    --                 , rectangle 60 25
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then orange
    --                                         else blank
    --                                 )
    --                     |> move ( -100, -80 )
    --                     |> notifyTap GoToStep3
    --                 , text "Next"
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then white
    --                                         else blank
    --                                 )
    --                     |> move ( -112, -83 )
    --                     |> notifyTap GoToStep3
    --                 ]

    --             Step3 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 1" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap GoToQuestion2
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
    --                 , text "Step 2: 1 / cos y" |> size 12 |> filled orange |> move ( -130, 55 )
    --                 , text "What is the third step?" |> size 12 |> filled orange |> move ( -130, 15 )
    --                 , text "a) sec y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -5 )
    --                     |> notifyTap RightAnswer
    --                 , text "b) tan y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -25 )
    --                     |> notifyTap WrongAnswer
    --                 , text "c) cos y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -45 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) sin y * cos y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -65 )
    --                     |> notifyTap WrongAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct! You have solved the problem."
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -85 )
    --                 ]
    --             Step4 -> []
    --             Step5 -> []
    --         )

    --     Question2 ->
    --         (case model.step of
    --             Step1 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that sin y + sin y * cot2 y = cscy" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "What is the first step?" |> size 12 |> filled orange |> move ( -130, 65 )
    --                 , text "a) sin y + sin y * (1 / tan2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 45 )
    --                     |> notifyTap WrongAnswer
    --                 , text "b) sin y * (1 + cot2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 25 )
    --                     |> notifyTap RightAnswer
    --                 , text "c) sin y + (sin y / tan2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 5 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) sin y + ( (sin y * cos2 y) / sin2 y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -15 )
    --                     |> notifyTap WrongAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct"
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -40 )
    --                 , rectangle 60 25
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then orange
    --                                         else blank
    --                                 )
    --                     |> move ( -100, -60 )
    --                     |> notifyTap GoToStep2
    --                 , text "Next"
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then white
    --                                         else blank
    --                                 )
    --                     |> move ( -112, -63 )
    --                     |> notifyTap GoToStep2
    --                 ]

    --             Step2 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "Step 1: sin y * (1 + cot2 y)" |> size 12 |> filled orange |> move ( -130, 75 )
    --                 , text "What is the second step?" |> size 12 |> filled orange |> move ( -130, 45 )
    --                 , text "a) sin y * (1 + (cos2 y / sin2 y))"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 25 )
    --                     |> notifyTap WrongAnswer
    --                 , text "b) sin y * (1 + (sin2 y / cos2 y))"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, 5 )
    --                     |> notifyTap WrongAnswer
    --                 , text "c) sin y * (csc2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -15 )
    --                     |> notifyTap RightAnswer
    --                 , text "d) sin y * (cos2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -35 )
    --                     |> notifyTap WrongAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct"
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -60 )
    --                 , rectangle 60 25
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then orange
    --                                         else blank
    --                                 )
    --                     |> move ( -100, -80 )
    --                     |> notifyTap GoToStep3
    --                 , text "Next"
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then white
    --                                         else blank
    --                                 )
    --                     |> move ( -112, -83 )
    --                     |> notifyTap GoToStep3
    --                 ]

    --             Step3 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
    --                 , text "Step 2: sin y * (csc2 y)" |> size 12 |> filled orange |> move ( -130, 55 )
    --                 , text "What is the third step?" |> size 12 |> filled orange |> move ( -130, 15 )
    --                 , text "a) sin y * (1 / cos2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -5 )
    --                     |> notifyTap WrongAnswer
    --                 , text "b) sin y * (cos2 y / sin2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -25 )
    --                     |> notifyTap WrongAnswer
    --                 , text "c) sin2 y / cosy"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -45 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) sin y / sin2 y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -65 )
    --                     |> notifyTap RightAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct"
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -90 )
    --                 , rectangle 60 25
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then orange
    --                                         else blank
    --                                 )
    --                     |> move ( -100, -110 )
    --                     |> notifyTap GoToStep4
    --                 , text "Next"
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then white
    --                                         else blank
    --                                 )
    --                     |> move ( -112, -113 )
    --                     |> notifyTap GoToStep4
    --                 ]
    --             Step4 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
    --                 , text "Step 2: sin y * (csc2 y)" |> size 12 |> filled orange |> move ( -130, 55 )
    --                 , text "Step 3: sin y / sin2 y" |> size 12 |> filled orange |> move ( -130, 35 )
    --                 , text "What is the fourth step?" |> size 12 |> filled orange |> move ( -130, 5 )
    --                 , text "a) 1 / sin y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -15 )
    --                     |> notifyTap RightAnswer
    --                 , text "b) sin y / (1 + tan2 y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -35 )
    --                     |> notifyTap WrongAnswer
    --                 , text "c) (1 + sin y) / sin y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -55 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) 1 / sin2 y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -75 )
    --                     |> notifyTap WrongAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct"
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -100 )
    --                 , rectangle 60 25
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then orange
    --                                         else blank
    --                                 )
    --                     |> move ( -100, -120 )
    --                     |> notifyTap GoToStep5
    --                 , text "Next"
    --                     |> filled (if model.answer == Incorrect
    --                                     then blank
    --                                     else if model.answer == Correct
    --                                         then white
    --                                         else blank
    --                                 )
    --                     |> move ( -112, -123 )
    --                     |> notifyTap GoToStep5
    --                 ]
    --             Step5 ->
    --                 [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    --                 , text "Question 2" |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    --                 , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
    --                 , text "Prove that tan y / sin y = sec y" |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    --                 , text "Step 1: (sin y / cos y) * (1 / sin y)" |> size 12 |> filled orange |> move ( -130, 75 )
    --                 , text "Step 2: sin y * (csc2 y)" |> size 12 |> filled orange |> move ( -130, 55 )
    --                 , text "Step 3: sin y / sin2 y" |> size 12 |> filled orange |> move ( -130, 35 )
    --                 , text "Step 4: 1 / sin y" |> size 12 |> filled orange |> move ( -130, 15 )
    --                 , text "What is the fifth step?" |> size 12 |> filled orange |> move ( -130, -25 )
    --                 , text "a) cos y / sin2 y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -45 )
    --                     |> notifyTap WrongAnswer
    --                 , text "b) csc y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -65 )
    --                     |> notifyTap RightAnswer
    --                 , text "c) tan y / (sin y * tan y)"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -85 )
    --                     |> notifyTap WrongAnswer
    --                 , text "d) cot y"
    --                     |> size 12
    --                     |> filled orange
    --                     |> move ( -130, -105 )
    --                     |> notifyTap WrongAnswer
    --                 , text (if model.answer == Incorrect
    --                             then "Incorrect"
    --                             else if model.answer == Correct
    --                                 then "Correct! You have solved the problem."
    --                                 else ""
    --                         )
    --                     |> size 12
    --                     |> filled (if model.answer == Incorrect
    --                                     then red
    --                                     else if model.answer == Correct
    --                                         then green
    --                                         else white
    --                                 )
    --                     |> move ( -130, -130 )
    --                 ]
    --         )


    -- )
    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    , resultsSection model.question model.step model.answer
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) orange |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled orange |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
       ]
    ++
        case model.hintState of
            PopUp question step -> [hintCard question step]
            otherwise -> []


-- helper function
getIndexFromStep step = case step of 
                Step1 -> 0
                Step2 -> 1
                Step3 -> 2
                Step4 -> 3
                Step5 -> 4


questionTitleStr question = case question of
                Question1 -> "Question 1"
                Question2 -> "Question 2"


questionStr question = case question of
                Question1 -> "Prove that tan y / sin y = sec y"
                Question2 -> "Prove that sin y + sin y * cot2 y = cscy"


stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"

            
solutionStr question = case question of
                Question1 -> [ "Step 1: (sin y / cos y) * (1 / sin y)"
                             , "Step 2: 1 / cos y"
                             ]
                Question2 -> [ "Step 1: sin y * (1 + cot2 y)"
                             , "Step 2: sin y * (csc2 y)"
                             , "Step 3: (sin y / sin2 y)"
                             , "Step 4: 1 / sin y"
                             ]


solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled orange
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


solutionSection question step = solutionText step (solutionStr question)


optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) tan y * (sin y / cos y)", WrongAnswer)
                                      , ( "b) tan y * (cos y / sin y)", WrongAnswer)
                                      , ("c) (cos y / sin y) * (1 / sin y)", WrongAnswer)
                                      , ("d) (sin y / cos y) * (1 / sin y)", RightAnswer)
                                      ]
                (Question1, Step2) -> [ ( "a) sin y / cos y", WrongAnswer)
                                      , ( "b) 1 / cos y", RightAnswer)
                                      , ( "c) 1 / sin y", WrongAnswer)
                                      , ( "d) (sin y * cos y)/(cos2 y * sin y)", WrongAnswer)
                                      ]
                (Question1, Step3) -> [ ( "a) sec y", RightAnswer)
                                      , ( "b) tan y", WrongAnswer)
                                      , ( "c) cos y", WrongAnswer)
                                      , ( "d) sin y * cos y", WrongAnswer)
                                      ]
                (Question2, Step1) -> [ ( "a) sin y + sin y * (1 / tan2 y)", WrongAnswer)
                                      , ( "b) sin y * (1 + cot2 y)", RightAnswer)
                                      , ( "c) sin y + (sin y / tan2 y)", WrongAnswer)
                                      , ( "d) sin y + ( (sin y * cos2 y) / sin2 y", WrongAnswer)
                                      ]
                (Question2, Step2) -> [ ( "a) sin y * (1 + (cos2 y / sin2 y))", WrongAnswer)
                                      , ( "b) sin y * (1 + (sin2 y / cos2 y))", WrongAnswer)
                                      , ( "c) sin y * (csc2 y)", RightAnswer)
                                      , ( "d) sin y * (cos2 y)", WrongAnswer)
                                      ]
                (Question2, Step3) -> [ ( "a) sin y * (1 / cos2 y)", WrongAnswer)
                                      , ( "b) sin y * (cos2 y / sin2 y)", WrongAnswer)
                                      , ( "c) (sin2 y / cos y)", WrongAnswer)
                                      , ( "d) (sin y / sin2 y)", RightAnswer)
                                      ]
                (Question2, Step4) -> [ ( "a) 1 / sin y", RightAnswer)
                                      , ( "b) sin y / (1 + tan2 y)", WrongAnswer)
                                      , ( "c) (1 + sin y) / sin y", WrongAnswer)
                                      , ( "d) 1 / sin2 y", WrongAnswer)
                                      ]
                (Question2, Step5) -> [ ( "a) cos y / sin2 y", WrongAnswer)
                                      , ( "b) csc y", RightAnswer)
                                      , ( "c) tan y / (sin y * tan y)", WrongAnswer)
                                      , ( "d) cot y", WrongAnswer)
                                      ]
                otherwise -> []


optionsText lst = group (List.indexedMap (\idx tuple -> text (Tuple.first tuple)
                                                            |> size 12
                                                            |> filled orange
                                                            |> move ( -130, 45-20*(Basics.toFloat idx))
                                                            |> notifyTap (Tuple.second tuple) ) lst)


optionsSection question step = group [ text (stepStr step) 
                                            |> size 12 
                                            |> filled orange 
                                            |> move ( -130, 65 )
                                     , optionsText (optionsStr question step)
                                     ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- helper function, this could probably be improved by putting the steps in a list
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step5) -> True
                otherwise -> False


resultsSection question step answer =
                if (isLastStep question step) 
                    then group [ text (if answer == Incorrect
                                    then "Incorrect"
                                    else if answer == Correct
                                        then "Correct! You have solved the problem."
                                        else ""
                                    )
                                    |> size 12
                                    |> filled (if answer == Incorrect
                                                    then red
                                                    else if answer == Correct
                                                        then green
                                                        else white
                                                )
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                ] 
                    else group [ text (if answer == Incorrect
                                    then "Incorrect"
                                    else if answer == Correct
                                        then "Correct"
                                        else ""
                                    )
                                    |> size 12
                                    |> filled (if answer == Incorrect
                                                    then red
                                                    else if answer == Correct
                                                        then green
                                                        else white
                                                )
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                , rectangle 60 25
                                    |> filled (if answer == Incorrect
                                                    then blank
                                                    else if answer == Correct
                                                        then orange
                                                        else blank
                                                )
                                    |> move ( -100, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , text "Next"
                                    |> filled (if answer == Incorrect
                                                    then blank
                                                    else if answer == Correct
                                                        then white
                                                        else blank
                                                )
                                    |> move ( -112, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                ] 
                    


hintStr question step = case (question, step) of
                (Question1, Step1) -> ["Express", "with", "sine", "and", "cosine" ]
                (Question1, Step2) -> ["Use", "the" , "relationship", "between secant", "and cosine", "to simplify"]
                (Question1, Step3) -> ["Simplify", "the" , "derived", "result", "from", "step 2"]
                (Question2, Step1) -> ["Take the", "comon", "denominator", "sine" ]
                (Question2, Step2) -> ["Express", "as" , "secant"]
                (Question2, Step3) -> ["Express", "as" , "sine"]
                (Question2, Step4) -> ["Simplify"]
                (Question2, Step5) -> ["Express", "as" , "cosecant"]
                otherwise -> []


hintText lst =  group (List.indexedMap (\idx line -> text line
                                                        -- |> sansserif
                                                        |> size 10
                                                        |> centered
                                                        |> filled black
                                                        |> move (0, 14-7*(Basics.toFloat idx)) ) lst)


hintCard question step = group [rect 70 70 |> filled black |> makeTransparent 0.3 |> addOutline (solid 0.3) black
                               , hintText (hintStr question step)
                               ] |> move ( 60, 0 )






-- messages generated by the framework (Tick) and by user interactions
-- note that we let elm figure out the type of the model by making it a type parameter, m

type Msg m
    = Tick Float GetKeyState
    | Notif Notifications
    | WrongAnswer
    | RightAnswer
    | NextStep
    | NextQuestion
    | PreviousQuestion
    | ClickedHint Questions Steps
    | ExitHint


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

module PythagoreanIdentities exposing (..)

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
    | Question3
    | Question4
    | Question5


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
                        Question2 -> Question3
                        Question3 -> Question4
                        Question4 -> Question5
                        Question5 -> Question1
                }

        PreviousQuestion
            -> { model
                | answer = Default
                , step = Step1
                , question =
                    case model.question of
                        Question2 -> Question1
                        Question1 -> Question1
                        Question3 -> Question2
                        Question4 -> Question3
                        Question5 -> Question4
                        -- Add to this if more questions are added
                }

        ClickedHint question step -> {model | hintState = PopUp question step }

        ExitHint -> {model | hintState = NoPopUp }

--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads

view model =
    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) purple |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled purple |> move ( 20, 120 )
    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled purple |> move ( -130, 95 )
    , resultsSection model.question model.step model.answer
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) purple |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled purple |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
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
                Question3 -> "Question 3"
                Question4 -> "Question 4"
                Question5 -> "Question 5"


questionStr question = case question of
                Question1 -> "Simplify sin(y)*cos^2*(y)-sin(y)"
                Question2 -> "Simplify csc^2(y)-cot(y)-3"
                Question3 -> "Simplify 1 / (1-cos^2(y))"
                Question4 -> "Simplify (1-cos^2(y))/(sin(y)*cos(y))"
                Question5 -> "Simplify cot(2y)*cos(y)*sin(y)+cos(y)*sin(y)"

stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


solutionStr question = case question of
                Question1 -> [ "Step 1: sin y(cos^2(y)-1)"
                             , "Step 2: sin y(-sin^2(y))"
                             , "Step 3: sin^3(y)"
                             ]
                Question2 -> [ "Step 1: 1+cot^2(y)-cot(y)-3"
                             , "Step 2: cot^2(y)-cot(y)-2"
                             , "Step 3: (cot(y)-2)*(cot(y)+1)"
                             ]
                Question3 -> [ "Step 1: 1/sin^2(y)"
                             , "Step 2: csc^2(y)"
                             ]
                Question4 -> [ "Step 1: sin^2(y)/(cos(y)sin(y))",
                               "Step 2: sin(y)/cos(y)",
                               "Step 3: tan(y)"
                             ]
                Question5 -> [ "Step 1: (sin(2y)/2)+(cos(y)*cot(2*y)*sin(y))",
                               "Step 2: (sin(2y)/2)+(sin(2y)*cot(2y))/2",
                               "Step 3: (sin(2y)+sin(2y)cot(2y))/2"
                             ]



solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled purple
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


solutionSection question step = solutionText step (solutionStr question)


optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) tan y * (sin y / cos y)", WrongAnswer)
                                      , ( "b) tan y * (cos y / sin y)", WrongAnswer)
                                      , ( "c) (cos y / sin y) * (1 / sin y)", WrongAnswer)
                                      , ( "d) sin y(cos^2(y)-1)", RightAnswer)
                                      ]
                (Question1, Step2) -> [ ( "a) csc y / cos y", WrongAnswer)
                                      , ( "b) sin y(-sin^2(y))", RightAnswer)
                                      , ( "c) 2 / sin y", WrongAnswer)
                                      , ( "d) (cos2 y * sin y)", WrongAnswer)
                                      ]
                (Question1, Step3) -> [ ( "a) sin^3(y)", RightAnswer)
                                      , ( "b) tan y", WrongAnswer)
                                      , ( "c) cos y", WrongAnswer)
                                      , ( "d) sin y", WrongAnswer)
                                      ]
                (Question2, Step1) -> [ ( "a) (1 / tan2 y)", WrongAnswer)
                                      , ( "b) 1+cot^2(y)-cot(y)-3", RightAnswer)
                                      , ( "c) cos y + (sin y / tan2 y)", WrongAnswer)
                                      , ( "d) (sin y * cos2 y) / sin2 y", WrongAnswer)
                                      ]
                (Question2, Step2) -> [ ( "a) (1 + (cos2 y / sin2 y))", WrongAnswer)
                                      , ( "b) (1 + (sin2 y / cos2 y))", WrongAnswer)
                                      , ( "c) cot^2(y)-cot(y)-2", RightAnswer)
                                      , ( "d) cot y * (cot2 y)", WrongAnswer)
                                      ]
                (Question2, Step3) -> [ ( "a) cot y * (1 / cos2 y)", WrongAnswer)
                                      , ( "b) sin y * (cos2 y / cot2 y)", WrongAnswer)
                                      , ( "c) (sin2 y / cot y+1)", WrongAnswer)
                                      , ( "d) (cot(y)-2)*(cot(y)+1)", RightAnswer)
                                      ]
                (Question3, Step1) -> [ ( "a) cos2 y / sin2 y", WrongAnswer)
                                      , ( "b) 1/sin^2(y)", RightAnswer)
                                      , ( "c) secy / (tan y * tan y)", WrongAnswer)
                                      , ( "d) sin y", WrongAnswer)
                                      ]
                (Question3, Step2) -> [ ( "a) cos3 y / sin3 y", WrongAnswer)
                                      , ( "b) (cos(y)/sin(y))/csc(y)", WrongAnswer)
                                      , ( "c) csc^2(y)", RightAnswer)
                                      , ( "d) sin y", WrongAnswer)
                                      ]
                (Question4, Step1) -> [ ( "a) cos2 y / sin2 y", WrongAnswer)
                                      , ( "b) (cos(y)/sin(y))/csc(y)", WrongAnswer)
                                      , ( "c) sin^2(y)/(cos(y)sin(y))", RightAnswer)
                                      , ( "d) csc y", WrongAnswer)
                                      ]
                (Question4, Step2) -> [ ( "a) sin(y)/cos(y)", RightAnswer)
                                      , ( "b) (cos(y)/sin(y))/sin(y)", WrongAnswer)
                                      , ( "c) cos(y)/(1/cos(y)", WrongAnswer)
                                      , ( "d) cot y", WrongAnswer)
                                      ]
                (Question4, Step3) -> [ ( "a) cos y / sin y", WrongAnswer)
                                      , ( "b) (sin(y))/csc(y)", WrongAnswer)
                                      , ( "c) tan(y)", RightAnswer)
                                      , ( "d) sec y", WrongAnswer)
                                      ]
                (Question5, Step1) -> [ ( "a) sin2 y", WrongAnswer)
                                      , ( "b) cos2(y)/csc(y)", WrongAnswer)
                                      , ( "c) (cos(y)/sin(y))+(sin(y)/cos(y))", WrongAnswer)
                                      , ( "d) (sin(2y)/2)+(cos(y)*cot(2*y)*sin(y))", RightAnswer)
                                      ]
                (Question5, Step2) -> [ ( "a) (sin(2y)/2)+(sin(2y)*cot(2y))/2", RightAnswer)
                                      , ( "b) csc y/(cos(y)/sin(y))", WrongAnswer)
                                      , ( "c) tan(y)/(1/tan(y)", WrongAnswer)
                                      , ( "d) cos y", WrongAnswer)
                                      ]
                (Question5, Step3) -> [ ( "a) (sin(2y)+sin(2y)cot(2y))/2", RightAnswer)
                                      , ( "b) (cot(y))/csc(y)", WrongAnswer)
                                      , ( "c) (csc(y)*sin(y)", WrongAnswer)
                                      , ( "d) csc y", WrongAnswer)
                                      ]
                otherwise -> []


optionsText lst = group (List.indexedMap (\idx tuple -> text (Tuple.first tuple)
                                                            |> size 12
                                                            |> filled purple
                                                            |> move ( -130, 45-20*(Basics.toFloat idx))
                                                            |> notifyTap (Tuple.second tuple) ) lst)


optionsSection question step = group [ text (stepStr step)
                                            |> size 12
                                            |> filled purple
                                            |> move ( -130, 65 )
                                     , optionsText (optionsStr question step)
                                     ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- helper function, this could probably be improved by putting the steps in a list
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step3) -> True
                (Question3, Step2) -> True
                (Question4, Step3) -> True
                (Question5, Step3) -> True
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
                                                        then purple
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
                (Question1, Step1) -> ["Divide", "by", "sine" ]
                (Question1, Step2) -> ["Use" , "sin^x+cos^x=1"]
                (Question1, Step3) -> ["Multiply", "the" , "terms", "in", "step 1"]
                (Question2, Step1) -> ["Use", "1+cot^2(x)=csc^2(x)" ]
                (Question2, Step2) -> ["Simplify", "the" , "terms"]
                (Question2, Step3) -> ["Express", "in form" , "of factors"]
                (Question3, Step1) -> ["Use", "1-cos^2(x)=sin^2(x)" ]
                (Question3, Step2) -> ["Simplify", "1/(sin(x)) = csc(x)"]
                (Question4, Step1) -> ["Use" , "sin^x+cos^x=1" ]
                (Question4, Step2) -> ["Refine"]
                (Question4, Step3) -> ["Express in terms of tangent"]
                (Question5, Step1) -> ["Use", "cos(x)*sin(x) =", "sin(2*x)/2" ]
                (Question5, Step2) -> ["Simplify" ]
                (Question5, Step3) -> ["Add", "the terms", "in step 3"]
                otherwise -> []


hintText lst =  group (List.indexedMap (\idx line -> text line
                                                        -- |> sansserif
                                                        |> size 10
                                                        |> centered
                                                        |> filled black
                                                        |> move (0, 14-7*(Basics.toFloat idx)) ) lst)


hintCard question step = group [rect 90 90 |> filled black |> makeTransparent 0.3 |> addOutline (solid 0.3) black
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

module ReciprocalIdentities exposing (..)

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
    , optionColourA = darkBlue
    , optionColourB = darkBlue
    , optionColourC = darkBlue
    , optionColourD = darkBlue
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
                        Question1 -> Question5
                        Question2 -> Question1
                        Question3 -> Question2
                        Question4 -> Question3
                        Question5 -> Question4
                        -- Add to this if more questions are added
                }

        ClickedHint question step -> {model | hintState = PopUp question step }

        ExitHint -> {model | hintState = NoPopUp }

        ChangeOptionColour t -> 
            t model

--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads

view model =
    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkBlue |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled darkBlue |> move ( 20, 120 )
    , triangle 8|> filled darkBlue |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled darkBlue |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled darkBlue |> move ( -130, 95 )
    , resultsSection model.question model.step model.answer
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) darkBlue |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled darkBlue |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
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
                Question1 -> "Which of the following expressions is equal to (sin y - tan y)(cos y - cot y)?"
                Question2 -> "Prove that (sec^2(y) - 1)/sec^2(y) = sin^2(y)"
                Question3 -> "Prove that 2tan(y)sec(y) = 2(sin(y)/cos(y))(1/cos(y))"
                Question4 -> "Prove that 4cos^2(y) - 1 = (2cos(y))^2 - 1"
                Question5 -> "Simplify csc^2(y) - cot^2(y)"

stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


solutionStr question = case question of
                Question1 -> [ "Step 1: = (sin y - (sin y / cos y)) ( cos y - (cos y/sin y))"
                             , "Step 2: = ((sin y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)"
                             , "Step 3: = (sin y * ((cos y - 1) / cos y)) (cos y * ((sin y - 1)/sin y))"
                             , "Step 4: = sin y cos y * ((cos y - 1) (sin y - 1) / cos y sin y)"
                             , "Step 5: = (cos y - 1) (sin y - 1)"
                             ]
                Question2 -> [ "Step 1: RHS = ((tan^2(y) + 1) - 1)/sec^2(y)"
                             , "Step 2: RHS = tan^2(y) / sec^2(y)"
                             , "Step 3: RHS = tan^2(y) * (1/sec^2(y))"
                             , "Step 4: RHS = tan^2(y) cos^2(y) "
                             , "Step 5: RHS = (sin^2(y) / cos^2(y))(cos^2(y)) = sin^2(y)"
                             ]
                Question3 -> [ "Step 1: RHS = 2sin(y)/cos^2(y)"
                             , "Step 2: RHS = sin(y)/(1-sin^2(y))"
                             ]
                Question4 -> [ "Step 1: RHS = (2cos(y) - 1)(2(cos(y)) + 1)"
                             ]
                Question5 -> [ "Step 1: = 1 + cot^2(y) - cot^2(y)",
                               "Step 2: = 1"
                             ]



solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled darkBlue
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


solutionSection question step = solutionText step (solutionStr question)


optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) = ((sin y sin y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", WrongAnswer)
                                      , ( "b) = ((sin y cos y - sin y)/cos y) ((tan y sin y - cos y)/sin y)", WrongAnswer)
                                      , ( "c) = ((tan y cos y - sin y)/cos y) ((tan y sin y - cos y)/tan y)", WrongAnswer)
                                      , ( "d) = ((sin y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", RightAnswer)
                                      ]
                (Question1, Step2) -> [ ( "a) = ((tan y cos y - sin y)/cos y) ((cos y tan y - cos y)/sin y)", WrongAnswer)
                                      , ( "b) = ((sin y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", RightAnswer)
                                      , ( "c) = ((tan y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", WrongAnswer)
                                      , ( "d) = ((sin y tan y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", WrongAnswer)
                                      ]
                (Question1, Step3) -> [ ( "a) = (sin y * ((cos y - 1) / cos y)) (cos y * ((sin y - 1)/sin y))", RightAnswer)
                                      , ( "b) = (tan y * ((cos y - 1) / cos y)) (cos y * ((tan y - 1)/sin y))", WrongAnswer)
                                      , ( "c) = (tan y * ((cos y - 1) / cos y)) (cos y * ((sin y - 1)/sin y))", WrongAnswer)
                                      , ( "d) = (sin y * ((cos y - 1) / cos y)) (cos y * ((tan y - 1)/sin y))", WrongAnswer)
                                      ]
                (Question1, Step4) -> [ ( "a) = sin y cos y * ((cos y - 1) (sin y - 1) / cos y sin y)", RightAnswer)
                                      , ( "b)= sin y cos y * ((sin y - 1) (sin y - 1) / cos y sin y)", WrongAnswer)
                                      , ( "c) = sin y cos y * ((cos y - 1) (sin y - 1) / sin y sin y)", WrongAnswer)
                                      , ( "d) = sin y cos y * ((sin y - 1) (sin y - 1) / cos y sin y)", WrongAnswer)
                                      ]
                (Question1, Step5) -> [ ( "a) = (cos y - 1) (sin y - 1)", RightAnswer)
                                      , ( "b) = (tan y - 1) (sin y - 1)", WrongAnswer)
                                      , ( "c) = (cos y - 1) (tan y - 1)", WrongAnswer)
                                      , ( "d) = (tan y - 1) (sin y - 1)", WrongAnswer)
                                      ]
                (Question2, Step1) -> [ ( "a) RHS = ((cos^2(y) + 1) - 1)/sec^2(y)", WrongAnswer)
                                      , ( "b) RHS = ((tan^2(y) + 1) - 1)/sec^2(y))", RightAnswer)
                                      , ( "c) RHS = ((cos^2(y) + 1) - 1)/sin^2(y)", WrongAnswer)
                                      , ( "d) RHS = ((tan^2(y) + 1) - 1)/sin^2(y)", WrongAnswer)
                                      ]
                (Question2, Step2) -> [ ( "a) RHS = cos^2(y) / sec^2(y)", WrongAnswer)
                                      , ( "b) RHS = cos^2(y) / tan^2(y)", WrongAnswer)
                                      , ( "c) RHS = tan^2(y) / sec^2(y)", RightAnswer)
                                      , ( "d) RHS = sec^2(y) / tan^2(y)", WrongAnswer)
                                      ]
                (Question2, Step3) -> [ ( "a) RHS = sec^2(y) * (1/tan^2(y))", WrongAnswer)
                                      , ( "b) RHS = sec^2(y) * (2/tan^2(y))", WrongAnswer)
                                      , ( "c) RHS = tan^2(y) * (2/sec^2(y))", WrongAnswer)
                                      , ( "d) RHS = tan^2(y) * (/sec^2(y))", RightAnswer)
                                      ]
                (Question2, Step4) -> [ ( "a) RHS = tan^2(y) cos^2(y) ", RightAnswer)
                                      , ( "b) RHS = cos^2(y) tan^2(y) ", WrongAnswer)
                                      , ( "c) RHS = cos^2(2y) tan^2(y) ", WrongAnswer)
                                      , ( "d) RHS = tan^2(2y) cos^2(y) ", WrongAnswer)
                                      ]
                (Question2, Step5) -> [ ( "a) RHS = (cos^2(y) / cos^2(y))(cos^2(y)) = sin^2(y)", WrongAnswer)
                                      , ( "b) RHS = (sin^2(y) / cos^2(y))(cos^2(y)) = sin^2(y)", RightAnswer)
                                      , ( "c) RHS = (cos^2(y) / cos^2(y))(tan^2(y)) = sin^2(y)", WrongAnswer)
                                      , ( "d) RHS = (sin^2(y) / cos^2(y))(tan^2(y)) = sin^2(y)", WrongAnswer)
                                      ]
                (Question3, Step1) -> [ ( "a) RHS = 4sin(y)/cos^2(y)", WrongAnswer)
                                      , ( "b) RHS = 2sin(y)/cos^2(y)", RightAnswer)
                                      , ( "c) RHS = 4sin(y)/cos^4(y)", WrongAnswer)
                                      , ( "d) RHS = 2sin(y)/cos^4(y)", WrongAnswer)
                                      ]
                (Question3, Step2) -> [ ( "a) RHS = sin(y)/(1-cot^2(y)", WrongAnswer)
                                      , ( "b) RHS = tan(y)/(1-cot^2(y)", WrongAnswer)
                                      , ( "c) RHS = sin(y)/(1-sin^2(y)", RightAnswer)
                                      , ( "d) RHS = tan(y)/(1-sin^2(y)", WrongAnswer)
                                      ]
                (Question4, Step1) -> [ ( "a) RHS = (2cos(y) - 1)(2(tan(y)) + 1)", WrongAnswer)
                                      , ( "b) RHS = (2sin(y) - 1)(2(tan(y)) + 1)", WrongAnswer)
                                      , ( "c) RHS = (2cos(y) - 1)(2(cos(y)) + 1)", RightAnswer)
                                      , ( "d) RHS = (2cos(y) - 1)(2(cos(y)) + 1)", WrongAnswer)
                                      ]
                (Question5, Step1) -> [ ( "a) = 1 + csc^2(y) - cot^2(y)", WrongAnswer)
                                      , ( "b) = 1 + cot^2(y) - csc^2(y)", WrongAnswer)
                                      , ( "c) = 1 + csc^2(y) - csc^2(y)", WrongAnswer)
                                      , ( "d) = 1 + cot^2(y) - cot^2(y)", RightAnswer)
                                      ]
                (Question5, Step2) -> [ ( "a) = 1", RightAnswer)
                                      , ( "b) = cosy", WrongAnswer)
                                      , ( "c) = siny/tany", WrongAnswer)
                                      , ( "d) = tany/siny", WrongAnswer)
                                      ]
                otherwise -> []


-- this could be improved
getOptionColour idx optionColourA optionColourB optionColourC optionColourD = case idx of
                                                                                0 -> optionColourA
                                                                                1 -> optionColourB
                                                                                2 -> optionColourC
                                                                                3 -> optionColourD
                                                                                otherwise -> optionColourA


-- there's probably a more efficient way of doing this
updateOptionColour idx colour = case idx of
                                    0 -> ChangeOptionColour (\m -> { m | optionColourA = colour })
                                    1 -> ChangeOptionColour (\m -> { m | optionColourB = colour })
                                    2 -> ChangeOptionColour (\m -> { m | optionColourC = colour })
                                    3 -> ChangeOptionColour (\m -> { m | optionColourD = colour })
                                    otherwise -> ChangeOptionColour (\m -> { m | optionColourA = colour })


optionsText lst optionColourA optionColourB optionColourC optionColourD = group (List.indexedMap (\idx tuple -> text (Tuple.first tuple)
                                                                                                                    |> size 12
                                                                                                                    |> filled (getOptionColour idx optionColourA optionColourB optionColourC optionColourD)
                                                                                                                    |> move ( -130, 45-20*(Basics.toFloat idx))
                                                                                                                    |> notifyEnter (updateOptionColour idx lightBlue)
                                                                                                                    |> notifyLeave (updateOptionColour idx darkBlue)
                                                                                                                    |> notifyTap (Tuple.second tuple) ) lst)


optionsSection question step optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled darkBlue
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- helper function, this could probably be improved by putting the steps in a list
isLastStep question step = case (question, step) of
                (Question1, Step5) -> True
                (Question2, Step5) -> True
                (Question3, Step2) -> True
                (Question4, Step1) -> True
                (Question5, Step2) -> True
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
                                                        then darkBlue
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
                (Question1, Step1) -> ["Simplify", "the" , "cotangent", "and", "tangent" ]
                (Question1, Step2) -> ["Try", "rearranging" , "the equation", "using trig", "identities"]
                (Question1, Step3) -> ["Try factorizing", "the" , "equation", "so you can", "use a trig identity"]
                (Question1, Step4) -> ["Try expanding", "the" , "equation"]
                (Question1, Step5) -> ["Simplify", "the" , "derived", "result", "from", "step 4"]
                (Question2, Step1) -> ["Use that", "sec^2(y) = tan^2(y) + 1"]
                (Question2, Step2) -> ["Simplify"]
                (Question2, Step3) -> ["How else can", "you express", "a fraction?"]
                (Question2, Step4) -> ["Use", "cos^2(y) = 1/sec^2(y)"]
                (Question2, Step5) -> ["Use", "a reciprocal", "identity", "then simplify"]
                (Question3, Step1) -> ["Use a", "fundamental", "trig", "identity" ]
                (Question3, Step2) -> ["Substitute", "1 - sin^2(y)"]
                (Question4, Step1) -> ["Use the", "difference", "of squares" ]
                (Question5, Step1) -> ["Use the", "Pythagorean", "identity" ]
                (Question5, Step2) -> ["Simplify" ]
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
    | ChangeOptionColour (m -> m)


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




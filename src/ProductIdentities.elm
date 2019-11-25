module ProductIdentities exposing (..)

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
                        Question1 -> Question5
                        Question2 -> Question1
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
                Question3 -> "Question 3"
                Question4 -> "Question 4"
                Question5 -> "Question 5"


questionStr question = case question of
                Question1 -> "Write the following product of cosines as a sum: 2cos(7x/2)cos(3x/2)"
                Question2 -> "Simplify the following: sin(4y)cos(2y)"
                Question3 -> "Write cos(3y)cos(5y) as a sum or difference."
                Question4 -> "Write the following difference of sines expression as a product: sin(4y) - sin(2y)"
                Question5 -> "Prove that (1-sin(y))(1+csc(y))=cos(y)*cot(y)"

stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


solutionStr question = case question of
                Question1 -> [ "Step 1: = (2*(1/2)) [ cos(7x/2 - 3x/2) + cos(7x/2 + 3x/2)"
                             , "Step 2: = [cos(4x/2) + cos(10x/2)"
                             , "Step 3: = cos2x + cos5x"
                             ]
                Question2 -> [ "Step 1: = 1/2 [sin(4y + 2y) + sin(4y - 2y)]"
                             , "Step 2: = 1/2 [sin(6y) + sin(2y)]"
                             ]
                Question3 -> [ "Step 1: = 1/2 [ cos(3y - 5y) + cos(3y + 5y)]"
                             , "Step 2: = 1/2 [cos(2y) + cos(8y)]"
                             ]
                Question4 -> [ "Step 1: = 2sin((4y - 2y) / 2) cos((4y + 2y) / 2)",
                               "Step 2: = 2sin(2y/2)cos(6y/2)",
                               "Step 3: = 2sinycos3y"
                             ]
                Question5 -> [ "Step 1: LHS = (1+(1/sin(y)))(1-(sin(y)))",
                               "Step 2: LHS = (1+sin(y))*(1-sin(y))/sin(y)",
                               "Step 3: LHS = (cos^2(y))/sin(y)"
                             ]



solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled orange
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


solutionSection question step = solutionText step (solutionStr question)


optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) = (2*(1/2)) [ cos(8x/2 - 3x/2) + cos(7x/2 + 3x/2)", WrongAnswer)
                                      , ( "b) = (2*(1/2)) [ cos(9x/2 - 3x/2) + cos(6x/2 + x/2)", WrongAnswer)
                                      , ( "c) = (2*(1/2)) [ cos(7x/2 - 4x/2) + cos(7x/2 + 3x/2)", WrongAnswer)
                                      , ( "d) = (2*(1/2)) [ cos(7x/2 - 3x/2) + cos(7x/2 + 3x/2)", RightAnswer)
                                      ]
                (Question1, Step2) -> [ ( "a) = [cos(3x/2) + cos(10x/2)", WrongAnswer)
                                      , ( "b) = [cos(8x/2) + cos(10x/2)", WrongAnswer)
                                      , ( "c) = [cos(4x/2) + cos(10x/2)", RightAnswer)
                                      , ( "d) = [cos(7x/2) + cos(10x/2)", WrongAnswer)
                                      ]
                (Question1, Step3) -> [ ( "a) = cos2x + cos5x", RightAnswer)
                                      , ( "b) = cos3x + cos7x", WrongAnswer)
                                      , ( "c) = cos5x + cos5x", WrongAnswer)
                                      , ( "d) = cos2x + cos6x", WrongAnswer)
                                      ]
                (Question2, Step1) -> [ ( "a) = 1/2 [sin(4y + 2y) + sin(4y - 2y)]", WrongAnswer)
                                      , ( "b) = 1/2 [sin(4y + 2y) + sin(4y - 2y)]", RightAnswer)
                                      , ( "c) = 1/2 [sin(4y + 2y) + sin(4y - 2y)]", WrongAnswer)
                                      , ( "d) = 1/2 [sin(4y + 2y) + sin(4y - 2y)]", WrongAnswer)
                                      ]
                (Question2, Step2) -> [ ( "a) = 1/2 [sin(6y) + sin(2y)]", WrongAnswer)
                                      , ( "b) = 1/2 [sin(6y) + sin(2y)]", WrongAnswer)
                                      , ( "c) = 1/2 [sin(6y) + sin(2y)]", RightAnswer)
                                      , ( "d) = 1/2 [sin(6y) + sin(2y)]", WrongAnswer)
                                      ]
                (Question3, Step1) -> [ ( "a) = 1/2 [ cos(5y - 5y) + cos(3y + 5y)]", WrongAnswer)
                                      , ( "b) = 1/2 [ cos(3y - 5y) + cos(3y + 5y)]", RightAnswer)
                                      , ( "c) = 1/2 [ cos(9y - 5y) + cos(3y + 5y)]", WrongAnswer)
                                      , ( "d) = 1/2 [ cos(2y - 5y) + cos(8y + 5y)]", WrongAnswer)
                                      ]
                (Question3, Step2) -> [ ( "a) = 1/3 [cos(2y) + cos(8y)]", WrongAnswer)
                                      , ( "b) = 1/3 [cos(2y) + cos(8y)]", WrongAnswer)
                                      , ( "c) = 1/2 [cos(2y) + cos(8y)]", RightAnswer)
                                      , ( "d) = 1/2 [cos(8y) + cos(8y)]", WrongAnswer)
                                      ]
                (Question4, Step1) -> [ ( "a) = 6sin((4y - 2y) / 2) cos((4y + 2y) / 2)", WrongAnswer)
                                      , ( "b) = 6sin((4y - 2y) / 2) cos((7y + 2y) / 2)", WrongAnswer)
                                      , ( "c) = 2sin((4y - 2y) / 2) cos((4y + 2y) / 2)", RightAnswer)
                                      , ( "d) = 2sin((4y - 2y) / 2) cos((7y + 2y) / 2)", WrongAnswer)
                                      ]
                (Question4, Step2) -> [ ( "a) = 2sin(2y/2)cos(6y/2)", RightAnswer)
                                      , ( "b) = 2sin(2y/3)cos(6y/2)", WrongAnswer)
                                      , ( "c) = 2sin(2y/3)cos(6y/2)", WrongAnswer)
                                      , ( "d) = 2sin(2y/2)cos(8y/2)", WrongAnswer)
                                      ]
                (Question4, Step3) -> [ ( "a) = 6sinycos3y", WrongAnswer)
                                      , ( "b) = 2sinycos7y", WrongAnswer)
                                      , ( "c) = 2sinycos3y", RightAnswer)
                                      , ( "d) = 2sinycos7y", WrongAnswer)
                                      ]
                (Question5, Step1) -> [ ( "a) LHS =  sin2 y", WrongAnswer)
                                      , ( "b) LHS =  cos(y)/csc(y)", WrongAnswer)
                                      , ( "c) LHS = (cos(y)/sin(y))+(sin(y)/cos(y))", WrongAnswer)
                                      , ( "d) LHS = (1+(1/sin(y)))(1-(sin(y)))", RightAnswer)
                                      ]
                (Question5, Step2) -> [ ( "a) LHS = (1+sin(y))*(1-sin(y))/sin(y)", RightAnswer)
                                      , ( "b) LHS = (cos(y)/sin(y))", WrongAnswer)
                                      , ( "c) LHS = sin(y)/(1/tan(y)", WrongAnswer)
                                      , ( "d) LHS = sin y", WrongAnswer)
                                      ]
                (Question5, Step3) -> [ ( "a) LHS = (cos^2(y))/sin(y)", RightAnswer)
                                      , ( "b) LHS = (sin(y))/csc(y)", WrongAnswer)
                                      , ( "c) LHS = (cos(y)*sin(y)", WrongAnswer)
                                      , ( "d) LHS = csc y", WrongAnswer)
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
                (Question2, Step2) -> True
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
                (Question1, Step1) -> ["Simplify!" ]
                (Question1, Step2) -> ["Simplify!" ]
                (Question1, Step3) -> ["Simplify!" ]
                (Question2, Step1) -> ["Use the", "product", "of sines" ]
                (Question2, Step2) -> ["Simplify"]
                (Question3, Step1) -> ["Use the", "product", "of cosines" ]
                (Question3, Step2) -> ["Simplify"]
                (Question4, Step1) -> ["Use the", "difference", "of sines" ]
                (Question4, Step2) -> ["Simplify"]
                (Question4, Step3) -> ["Simplify"]
                (Question5, Step1) -> ["Express", "with", "sine", "and", "cosine" ]
                (Question5, Step2) -> ["Simplify", "terms in", "step 1" ]
                (Question5, Step3) -> ["Use", "1-sin^2(y)=cos^2(y)"]
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




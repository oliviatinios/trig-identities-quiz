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
    , choiceState = NoPopUpChoice
    , optionColourA = purple
    , optionColourB = purple
    , optionColourC = purple
    , optionColourD = purple
    , option = Option1
    , state = None
    }

type HintState = NoPopUp | PopUp Questions Steps

type ChoiceState = NoPopUpChoice | PopUpChoice Questions Steps Options

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

type Options
    = Option1
    | Option2
    | Option3
    | RightOption

type State
    = None
    | Hint
    | Choice

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

        WrongAnswer1
            -> { model
                | answer = Incorrect
                , option = Option1
                }

        WrongAnswer2
            -> { model
                | answer = Incorrect
                , option = Option2
                }

        WrongAnswer3
            -> { model
                | answer = Incorrect
                , option = Option3
                }

        RightAnswer
            -> { model
                | answer = Correct
                , option = RightOption
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

        ClickedHint question step
            -> {model
                | state = Hint
                , hintState = PopUp question step
                }

        ClickedChoice question step option
            -> {model
                | state = Choice
                , choiceState = PopUpChoice question step option
                }

        ExitHint
            -> {model
                | state = None
                , hintState = NoPopUp
                }

        ExitChoice
            -> {model
                | state = None
                , choiceState = NoPopUpChoice
                }

        ChangeOptionColour t -> 
            t model

--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads

view model =
    [ rectangle 400 300 |> filled blank |> addOutline (solid 2) purple |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled purple |> move ( 20, 120 )
    , triangle 8|> filled purple |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled purple |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled purple |> move ( -130, 95 )
    , resultsSection model.question model.step model.answer model.option
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) purple |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled purple |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
       ]
    ++
        case model.state of
            Hint -> case model.hintState of
                        PopUp question step -> [hintCard question step]
                        otherwise -> []
            Choice -> case model.choiceState of
                        PopUpChoice question step option -> [choiceCard question step option]
                        otherwise -> []
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
                (Question1, Step1) -> [ ( "a) tan y * (sin y / cos y)", WrongAnswer1)
                                      , ( "b) tan y * (cos y / sin y)", WrongAnswer2)
                                      , ( "c) (cos y / sin y) * (1 / sin y)", WrongAnswer3)
                                      , ( "d) sin y(cos^2(y)-1)", RightAnswer)
                                      ]
                (Question1, Step2) -> [ ( "a) csc y / cos y", WrongAnswer1)
                                      , ( "b) sin y(-sin^2(y))", RightAnswer)
                                      , ( "c) 2 / sin y", WrongAnswer2)
                                      , ( "d) (cos2 y * sin y)", WrongAnswer3)
                                      ]
                (Question1, Step3) -> [ ( "a) -sin^3(y)", RightAnswer)
                                      , ( "b) tan y", WrongAnswer1)
                                      , ( "c) cos y", WrongAnswer2)
                                      , ( "d) sin y", WrongAnswer3)
                                      ]
                (Question2, Step1) -> [ ( "a) (1 / tan2 y)", WrongAnswer1)
                                      , ( "b) 1+cot^2(y)-cot(y)-3", RightAnswer)
                                      , ( "c) cos y + (sin y / tan2 y)", WrongAnswer2)
                                      , ( "d) (sin y * cos2 y) / sin2 y", WrongAnswer3)
                                      ]
                (Question2, Step2) -> [ ( "a) (1 + (cos2 y / sin2 y))", WrongAnswer1)
                                      , ( "b) (1 + (sin2 y / cos2 y))", WrongAnswer2)
                                      , ( "c) cot^2(y)-cot(y)-2", RightAnswer)
                                      , ( "d) cot y * (cot2 y)", WrongAnswer3)
                                      ]
                (Question2, Step3) -> [ ( "a) cot y * (1 / cos2 y)", WrongAnswer1)
                                      , ( "b) sin y * (cos2 y / cot2 y)", WrongAnswer2)
                                      , ( "c) (sin2 y / cot y+1)", WrongAnswer3)
                                      , ( "d) (cot(y)-2)*(cot(y)+1)", RightAnswer)
                                      ]
                (Question3, Step1) -> [ ( "a) cos2 y / sin2 y", WrongAnswer1)
                                      , ( "b) 1/sin^2(y)", RightAnswer)
                                      , ( "c) secy / (tan y * tan y)", WrongAnswer2)
                                      , ( "d) sin y", WrongAnswer3)
                                      ]
                (Question3, Step2) -> [ ( "a) cos3 y / sin3 y", WrongAnswer1)
                                      , ( "b) (cos(y)/sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) csc^2(y)", RightAnswer)
                                      , ( "d) sin y", WrongAnswer3)
                                      ]
                (Question4, Step1) -> [ ( "a) cos2 y / sin2 y", WrongAnswer1)
                                      , ( "b) (cos(y)/sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) sin^2(y)/(cos(y)sin(y))", RightAnswer)
                                      , ( "d) csc y", WrongAnswer3)
                                      ]
                (Question4, Step2) -> [ ( "a) sin(y)/cos(y)", RightAnswer)
                                      , ( "b) (cos(y)/sin(y))/sin(y)", WrongAnswer1)
                                      , ( "c) cos(y)/(1/cos(y)", WrongAnswer2)
                                      , ( "d) cot y", WrongAnswer3)
                                      ]
                (Question4, Step3) -> [ ( "a) cos y / sin y", WrongAnswer1)
                                      , ( "b) (sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) tan(y)", RightAnswer)
                                      , ( "d) sec y", WrongAnswer3)
                                      ]
                (Question5, Step1) -> [ ( "a) sin2 y", WrongAnswer1)
                                      , ( "b) cos2(y)/csc(y)", WrongAnswer2)
                                      , ( "c) (cos(y)/sin(y))+(sin(y)/cos(y))", WrongAnswer3)
                                      , ( "d) (sin(2y)/2)+(cos(y)*cot(2*y)*sin(y))", RightAnswer)
                                      ]
                (Question5, Step2) -> [ ( "a) (sin(2y)/2)+(sin(2y)*cot(2y))/2", RightAnswer)
                                      , ( "b) csc y/(cos(y)/sin(y))", WrongAnswer1)
                                      , ( "c) tan(y)/(1/tan(y)", WrongAnswer2)
                                      , ( "d) cos y", WrongAnswer3)
                                      ]
                (Question5, Step3) -> [ ( "a) (sin(2y)+sin(2y)cot(2y))/2", RightAnswer)
                                      , ( "b) (cot(y))/csc(y)", WrongAnswer1)
                                      , ( "c) (csc(y)*sin(y)", WrongAnswer2)
                                      , ( "d) csc y", WrongAnswer3)
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
                                                                                                                    |> notifyEnter (updateOptionColour idx lightPurple)
                                                                                                                    |> notifyLeave (updateOptionColour idx purple)
                                                                                                                    |> notifyTap (Tuple.second tuple) ) lst)


optionsSection question step optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled purple
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- helper function, this could probably be improved by putting the steps in a list
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step3) -> True
                (Question3, Step2) -> True
                (Question4, Step3) -> True
                (Question5, Step3) -> True
                otherwise -> False


resultsSection question step answer option =
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
                                , rectangle 90 25
                                    |> filled (if answer == Default
                                            then blank
                                            else purple
                                            )
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                , text "Explanation"
                                    |> filled (if answer == Default
                                            then blank
                                            else white
                                            )
                                    |> move ( -115, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                ]
                    else if (answer == Incorrect)
                        then group [
                                text "Incorrect"
                                    |> size 12
                                    |> filled red
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                , rectangle 90 25
                                    |> filled purple
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                , text "Explanation"
                                    |> filled white
                                    |> move ( -115, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                ]
                        else if (answer == Correct)
                            then group [
                                text "Correct"
                                    |> size 12
                                    |> filled green
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                 , rectangle 60 25
                                    |> filled purple
                                    |> move ( -100, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , text "Next"
                                    |> filled white
                                    |> move ( -112, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , rectangle 90 25
                                    |> filled purple
                                    |> move ( -5, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                , text "Explanation"
                                    |> filled white
                                    |> move ( -35, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                ]
                            else group []



hintStr question step = case (question, step) of
                (Question1, Step1) -> ["Factor out", "sine"]
                (Question1, Step2) -> ["Use" , "sin^x+cos^x=1", "to simplify Step1"]
                (Question1, Step3) -> ["Multiply", "the" , "terms", "in", "step 1"]
                (Question2, Step1) -> ["Use", "1+cot^2(x)=csc^2(x)" ]
                (Question2, Step2) -> ["Simplify", "the" , "constants in Step1"]
                (Question2, Step3) -> ["Express", "in form" , "of factors"]
                (Question3, Step1) -> ["Use", "1-cos^2(x)=sin^2(x)" ]
                (Question3, Step2) -> ["Simplify", "1/(sin(x)) = csc(x)"]
                (Question4, Step1) -> ["Use" , "sin^x+cos^x=1" ]
                (Question4, Step2) -> ["Factor", "out", "sin y"]
                (Question4, Step3) -> ["Use tan(y)=sin(y)/cos(y)"]
                (Question5, Step1) -> ["Use", "cos(x)*sin(x) =", "sin(2*x)/2" ]
                (Question5, Step2) -> ["Reduce","fraction obtained", "in Step1" ]
                (Question5, Step3) -> ["Add", "the terms", "in Step2", "and simplify further"]
                otherwise -> []

explanationStr question step option = case (question, step) of
                (Question1, Step1) -> case option of
                                        Option1 -> ["tan y equals sin y/cos y. ", "Multiplying tan y by sin y / cos y", "does not lead back", "to the original expression." ]
                                        Option2 -> ["tan y equals sin y/cos y. ", "Multiplying tan y by cos y / sin y", "does not lead back", "to the original expression." ]
                                        Option3 -> ["It is not possible", "to simplify to this expression.", "You can double check by", "expanding this expression and checking", "if it equals the original expression." ]
                                        RightOption -> ["sin y is common in both", "the terms around the subtraction.", "We can factor out sin y. ", "Therefore, this option is correct"]
                (Question1, Step2) -> case option of
                                        Option1 -> ["csy equals 1/siny, ", "This expression would", "expand to 1/(siny * cosy).", "This cannot be derived back to Step 1." ]
                                        Option2 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        Option3 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        RightOption -> ["After substituting", "1 as sin2y + cos2y,", "the expression becomes", "siny(cos2y - sin2y - cos2y).", "The cos2y terms cancel out.", "Therefore, this option is correct"]
                (Question1, Step3) -> case option of
                                        Option1 -> ["It is not possible", "to get tan y", "from Step 2." ]
                                        Option2 -> ["It is not possible", "to get cos y", "from Step 2." ]
                                        Option3 -> ["It is not possible", "to get sin y", "from Step 2." ]
                                        RightOption -> ["Multiplying siny with -sin2y", "equals -sin3y.", "Therefore, this option is correct"]
                (Question2, Step1) -> case option of
                                        Option1 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        Option2 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        Option3 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        RightOption -> ["We can use the", "1 + cot2y = csc2y identity", "and subsitute it in as csc2y.", "Therefore, this option is correct"]
                (Question2, Step2) -> case option of
                                        Option1 -> ["Leaving the constant 1 as is,", "cot2y - coty - 3 cannot", "be simplified to", "cos2y/sin2y." ]
                                        Option2 -> ["Leaving the constant 1 as is,", "cot2y - coty - 3 cannot", "be simplified to", "sin2y/cos2y." ]
                                        Option3 -> ["It is not possible", "to simplify to this expression.", "You can double check by", "expanding this expression and checking", "if it equals Step 1.", "cot y * (cot2 y) equals cot3 y", "cot3 y cannot be expanded", "to the expression in Step 1" ]
                                        RightOption -> ["We can simplify further by", "evaluating the constants.", "1 - 3 equals -2.", "Therefore, this option is correct"]
                (Question2, Step3) -> case option of
                                        Option1 -> ["It is not possible", "to simplify to this expression.", "You can double check by", "expanding this expression and checking", "if it equals Step 2.", "cot y * (1/cos2 y) equals cot y / cos2 y.", "This cannot be expanded", "to the expression in Step 2" ]
                                        Option2 -> ["It is not possible to get this expression.", "You can double check by", "expanding this expression and checking", "if it equals Step 2.", "sin y * (cos2 y/cot2 y) equals", "(sin y * cos2 y) / cot2 y.", "This cannot be expanded", "to the expression in Step 2" ]
                                        Option3 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        RightOption -> ["We can express the expression", "in terms of factors.", "Using the quadratic formula, ", "we get (cot y - 2)(cot y + 1).", "Therefore, this option is correct"]
                (Question3, Step1) -> case option of
                                        Option1 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        Option2 -> ["It is not possible", "to simplify the expression", "to secy / (tan y * tan y)" ]
                                        Option3 -> ["It is not possible", "to simplify the expression", "to sin y" ]
                                        RightOption -> ["We can use the identity:", "sin2y + cos2y = 1 by rearranging it to", "sin2y = 1 - cos2y.", "We can now substitute the denominator", "with sin2y.", "Therefore, this option is correct."]
                (Question3, Step2) -> case option of
                                        Option1 -> ["It is not possible", "to simplify the expression", "to cos3 y/ sin3 y using 1/siny = cscy" ]
                                        Option2 -> ["It is not possible", "to simplify the expression", "to (cosy/siny)/ csc y using 1/siny = cscy" ]
                                        Option3 -> ["It is not possible", "to simplify the expression", "to sin y using 1/siny = cscy" ]
                                        RightOption -> ["Using 1/siny = cscy", "the expression simplifies to csc^2y.", "Therefore, this option is correct"]
                (Question4, Step1) -> case option of
                                        Option1 -> ["It is not possible", "to simplify the expression", "to cos2 y/ sin2 y", "using sin2y + cos2y = 1" ]
                                        Option2 -> ["It is not possible", "to simplify the expression", "to (cos y/ sin y)/csc y", "using sin2 y + cos2 y = 1" ]
                                        Option3 -> ["It is not possible", "to simplify the expression", "to csc y", "using sin2 y + cos2 y = 1" ]
                                        RightOption -> ["We can use the identity:", "sin2 y + cos2 y = 1 by rearranging it to", "sin2 y = 1 - cos2 y.", "We can now substitute the numerator", "with sin2y.", "Therefore, this option is correct."]
                (Question4, Step2) -> case option of
                                        Option1 -> ["It is not possible", "to simplify the expression.", "to (cos y / sin y) / sin y." ]
                                        Option2 -> ["It is not possible", "to simplify the expression.", "to cos y / (1 / sin y)." ]
                                        Option3 -> ["cot y equals cos y / sin y", "It is not possible", "to simplify to cot y." ]
                                        RightOption -> ["sin y can be factored out", "from the numerator and denominator", "and then cancelled out.", "Therefore, this option is correct"]
                (Question4, Step3) -> case option of
                                        Option1 -> ["It is not possible", "to simplify the expression.", "to cos y / sin y." ]
                                        Option2 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to sin y / csc y."]
                                        Option3 -> ["sec y equals 1 / cos y.", "It is not possible", "to simplify the expression.", "to sec y." ]
                                        RightOption -> ["We can use the identity:", "tan y = sin y / cos y.", "Therefore, this option is correct"]
                (Question5, Step1) -> case option of
                                        Option1 -> ["It is not possible", "to simplify the expression.", "to sin2 y." ]
                                        Option2 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to cos2 y/csc y." ]
                                        Option3 -> ["cot2 y can be simplified to", "cos2y / sin2 y where sin y can", "be factored out, but", "it would still not simplify to", "(cos y/sin y) + (sin y / cos y).", "There is an easier simplification." ]
                                        RightOption -> ["We can use the identity:", "cos y * sin y = sin 2y/2", "and substitute that in.", "Therefore, this option is correct"]
                (Question5, Step2) -> case option of
                                        Option1 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to csc y / (cos y / sin y)." ]
                                        Option2 -> ["tan y equals sin y / cos y.", "It is not possible", "to simplify the expression.", "to tan y / (1 / tan y)." ]
                                        Option3 -> ["It is not possible", "to simplify the expression.", "to cos y." ]
                                        RightOption -> ["We can use the identity:", "cos y * sin y = sin 2y/2 again", "and substitute that in the second term.", "Therefore, this option is correct"]
                (Question5, Step3) -> case option of
                                        Option1 -> ["cot y equals cos y / sin y", "and csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to cot y / csc y." ]
                                        Option2 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to csc y * sin y." ]
                                        Option3 -> ["It is not possible", "to simplify the expression.", "to cos y." ]
                                        RightOption -> ["Since the two terms have", "the same denominator, ", "we can add the two fractions together.", "Therefore, this option is correct"]
                otherwise -> []

hintText lst =  group (List.indexedMap (\idx line -> text line
                                                        -- |> sansserif
                                                        |> size 10
                                                        |> centered
                                                        |> filled black
                                                        |> move (0, 14-10*(Basics.toFloat idx)) ) lst)


hintCard question step = group [rect 130 130 |> filled grey |> addOutline (solid 0.3) black
                               , hintText (hintStr question step)
                               ] |> move ( 60, 0 )

choiceCard question step option = group [rect 170 130 |> filled grey |> addOutline (solid 0.3) black
                                , hintText (explanationStr question step option)
                                , text "X" |> bold |> sansserif |> size 14 |> filled black |> makeTransparent 0.75 |> move (65, 45) |> notifyTap ExitChoice
                                ] |> move ( 60, 0 )






-- messages generated by the framework (Tick) and by user interactions
-- note that we let elm figure out the type of the model by making it a type parameter, m

type Msg m
    = Tick Float GetKeyState
    | Notif Notifications
    | WrongAnswer1
    | WrongAnswer2
    | WrongAnswer3
    | RightAnswer
    | NextStep
    | NextQuestion
    | PreviousQuestion
    | ClickedHint Questions Steps
    | ExitHint
    | ClickedChoice Questions Steps Options
    | ExitChoice
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

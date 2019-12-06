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
    , choiceState = NoPopUpChoice
    , optionColourA = orange
    , optionColourB = orange
    , optionColourC = orange
    , optionColourD = orange
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
    [ rectangle 400 300 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    , resultsSection model.question model.step model.answer model.option
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) orange |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled orange |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
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
                Question1 -> "Prove that tan y / sin y = sec y"
                Question2 -> "Prove that sin y + sin y * cot^2 y = cscy"
                Question3 -> "Prove that cot y / csc y = cos y"
                Question4 -> "Prove that cot(y)+tan(y)=sec(y)*csc(y)"
                Question5 -> "Prove that (1-sin(y))(1+csc(y))=cos(y)*cot(y)"

stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


solutionStr question = case question of
                Question1 -> [ "Step 1: LHS = (sin y / cos y) * (1 / sin y)"
                             , "Step 2: LHS = 1 / cos y"
                             ]
                Question2 -> [ "Step 1: LHS = sin y * (1 + cot2 y)"
                             , "Step 2: LHS = sin y * (csc2 y)"
                             , "Step 3: LHS = (sin y / sin2 y)"
                             , "Step 4: LHS = 1 / sin y"
                             ]
                Question3 -> [ "Step 1: LHS = (cos(y)/sin(y))/csc(y)"
                             , "Step 2: LHS = (cos(y)/sin(y))/(1/sin(y))"
                             , "Step 3: LHS = cos(y)"
                             ]
                Question4 -> [ "Step 1: LHS = (cos(y)/sin(y))+(sin(y)/cos(y))",
                               "Step 2: LHS = (cos^2(y)+sin^2(y))/(cos(y)*sin(y))",
                               "Step 3: LHS = 1/(cos(y)*sin(y))"
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
                (Question1, Step1) -> [ ( "a) LHS = tan y * (sin y / cos y)", WrongAnswer1)
                                      , ( "b) LHS = tan y * (cos y / sin y)", WrongAnswer2)
                                      , ( "c) LHS = (cos y / sin y) * (1 / sin y)", WrongAnswer3)
                                      , ( "d) LHS = (sin y / cos y) * (1 / sin y)", RightAnswer)
                                      ]
                (Question1, Step2) -> [ ( "a) LHS = sin y / cos y", WrongAnswer1)
                                      , ( "b) LHS = 1 / cos y", RightAnswer)
                                      , ( "c) LHS = 1 / sin y", WrongAnswer2)
                                      , ( "d) LHS = (sin y * cos y)/(cos2 y * sin y)", WrongAnswer3)
                                      ]
                (Question1, Step3) -> [ ( "a) LHS = sec y", RightAnswer)
                                      , ( "b) LHS = tan y", WrongAnswer1)
                                      , ( "c) LHS = cos y", WrongAnswer2)
                                      , ( "d) LHS = sin y * cos y", WrongAnswer3)
                                      ]
                (Question2, Step1) -> [ ( "a) LHS = sin y + sin y * (1 / tan2 y)", WrongAnswer1)
                                      , ( "b) LHS = sin y * (1 + cot^2 y)", RightAnswer)
                                      , ( "c) LHS = sin y + (sin y / tan2 y)", WrongAnswer2)
                                      , ( "d) LHS = sin y + ( (sin y * cos2 y) / sin2 y", WrongAnswer3)
                                      ]
                (Question2, Step2) -> [ ( "a) LHS = sin y * (1 + (cos2 y / sin2 y))", WrongAnswer1)
                                      , ( "b) LHS = sin y * (1 + (sin2 y / cos2 y))", WrongAnswer2)
                                      , ( "c) LHS = sin y * (csc^2 y)", RightAnswer)
                                      , ( "d) LHS = sin y * (cos^2 y)", WrongAnswer3)
                                      ]
                (Question2, Step3) -> [ ( "a) LHS = sin y * (1 / cos2 y)", WrongAnswer1)
                                      , ( "b) LHS = sin y * (cos2 y / sin2 y)", WrongAnswer2)
                                      , ( "c) LHS = (sin2 y / cos y)", WrongAnswer3)
                                      , ( "d) LHS = (sin y / sin^2 y)", RightAnswer)
                                      ]
                (Question2, Step4) -> [ ( "a) LHS = 1 / sin y = csc y", RightAnswer)
                                      , ( "b) LHS = sin y / (1 + tan2 y)=csc y", WrongAnswer1)
                                      , ( "c) LHS = (1 + sin y) / sin y = csc y", WrongAnswer2)
                                      , ( "d) LHS = 1 / sin2 y = csc y", WrongAnswer3)
                                      ]
                (Question2, Step5) -> [ ( "a) LHS = cos y / sin2 y", WrongAnswer1)
                                      , ( "b) LHS = csc y", RightAnswer)
                                      , ( "c) LHS = tan y / (sin y * tan y)", WrongAnswer2)
                                      , ( "d) LHS = cot y", WrongAnswer3)
                                      ]
                (Question3, Step1) -> [ ( "a) LHS = cos2 y / sin2 y",  WrongAnswer1)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", RightAnswer)
                                      , ( "c) LHS = secy / (tan y * tan y)", WrongAnswer2)
                                      , ( "d) LHS = cot y", WrongAnswer3)
                                      ]
                (Question3, Step2) -> [ ( "a) LHS = cos3 y / sin3 y", WrongAnswer1)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) LHS = (cos(y)/sin(y))/(1/sin(y))", RightAnswer)
                                      , ( "d) LHS = cot y", WrongAnswer3)
                                      ]
                (Question3, Step3) -> [ ( "a) LHS = cos3 y / sin3 y", WrongAnswer1)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) LHS = cos(y)", RightAnswer)
                                      , ( "d) LHS = csc y", WrongAnswer3)
                                      ]
                (Question4, Step1) -> [ ( "a) LHS = cos2 y / sin2 y", WrongAnswer1)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) LHS = (cos(y)/sin(y))+(sin(y)/cos(y))", RightAnswer)
                                      , ( "d) LHS = cot y", WrongAnswer3)
                                      ]
                (Question4, Step2) -> [ ( "a) LHS = (cos^2(y)+sin^2(y))/(cos(y)*sin(y))", RightAnswer)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", WrongAnswer1)
                                      , ( "c) LHS = cos(y)/(1/sin(y)", WrongAnswer2)
                                      , ( "d) LHS = cot y", WrongAnswer3)
                                      ]
                (Question4, Step3) -> [ ( "a) LHS = cos y / sin y", WrongAnswer1)
                                      , ( "b) LHS = (sin(y))/csc(y)", WrongAnswer2)
                                      , ( "c) LHS = 1/(cos(y)*sin(y))", RightAnswer)
                                      , ( "d) LHS = sec y", WrongAnswer3)
                                      ]
                (Question5, Step1) -> [ ( "a) LHS =  sin2 y", WrongAnswer1)
                                      , ( "b) LHS =  cos(y)/csc(y)", WrongAnswer2)
                                      , ( "c) LHS = (cos(y)/sin(y))+(sin(y)/cos(y))", WrongAnswer3)
                                      , ( "d) LHS = (1+(1/sin(y)))(1-(sin(y)))", RightAnswer)
                                      ]
                (Question5, Step2) -> [ ( "a) LHS = (1+sin(y))*(1-sin(y))/sin(y)", RightAnswer)
                                      , ( "b) LHS = (cos(y)/sin(y))", WrongAnswer1)
                                      , ( "c) LHS = sin(y)/(1/tan(y)", WrongAnswer2)
                                      , ( "d) LHS = sin y", WrongAnswer3)
                                      ]
                (Question5, Step3) -> [ ( "a) LHS = (cos^2(y))/sin(y)", RightAnswer)
                                      , ( "b) LHS = (sin(y))/csc(y)", WrongAnswer1)
                                      , ( "c) LHS = (cos(y)*sin(y)", WrongAnswer2)
                                      , ( "d) LHS = csc y", WrongAnswer3)
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
                                                                                                                    |> notifyEnter (updateOptionColour idx lightOrange)
                                                                                                                    |> notifyLeave (updateOptionColour idx orange)
                                                                                                                    |> notifyTap (Tuple.second tuple) ) lst)


optionsSection question step optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled orange
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- helper function, this could probably be improved by putting the steps in a list
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step5) -> True
                (Question3, Step3) -> True
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
                                            else orange
                                            )
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                , text "Explaination"
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
                                    |> filled orange
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                , text "Explaination"
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
                                    |> filled orange
                                    |> move ( -100, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , text "Next"
                                    |> filled white
                                    |> move ( -112, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , rectangle 90 25
                                    |> filled orange
                                    |> move ( -5, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                , text "Explaination"
                                    |> filled white
                                    |> move ( -35, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedChoice question step option)
                                ]
                            else group []



hintStr question step = case (question, step) of
                (Question1, Step1) -> ["Express tan", "in terms of", "sine", "and", "cosine" ]
                (Question1, Step2) -> ["Reduce", "the" , "fraction", "obtained in", "Step1"]
                (Question1, Step3) -> ["Express", "Step2" , "in", "terms", "of", "sec"]
                (Question2, Step1) -> ["Take the", "comon", "denominator", "sine" ]
                (Question2, Step2) -> ["Use the equation", "cot^2 y=-1+csc^2 y" , "to simplify", "Step1"]
                (Question2, Step3) -> ["Express", "csc as", "sine and", "simplify"]
                (Question2, Step4) -> ["Reduce", "the fraction", "obtained in", "Step3"]
                (Question2, Step5) -> ["Express", "as" , "cosecant"]
                (Question3, Step1) -> ["Use", "cot(y)", "=", "cos(y)/sin(y)" ]
                (Question3, Step2) -> ["Use", "csc(y)=1/sin(y)"]
                (Question3, Step3) -> ["Reduce the", "fractions obtained", "in Step2"]
                (Question4, Step1) -> ["Use tan(y)=sin(y)/cos(y)", "and", "cot(y)=cos(y)/sin(y)"]
                (Question4, Step2) -> ["Add the", "terms obtained", "Step1" ]
                (Question4, Step3) -> ["Use", "sin^2y+cos^2y=1", "to simplify", "Step2"]
                (Question5, Step1) -> ["Use", "csc(y)=1/sin(y)", "to", "simplify" ]
                (Question5, Step2) -> ["Simplify", "the fraction", "obtained in", "Step1" ]
                (Question5, Step3) -> ["Use", "1-sin^2(y)=cos^2(y)", "to simplify Step2"]
                otherwise -> []

explainationStr question step option = case (question, step) of
                (Question1, Step1) -> case option of
                                        Option1 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "sin y / cos y" ]
                                        Option2 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "cos y / sin y" ]
                                        Option3 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y does not equal", "cos y / sin y" ]
                                        RightOption -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y can be", "written as", "sin y / cos y.", "Therefore, this option is correct"]
                (Question1, Step2) -> case option of
                                        Option1 -> ["By multiplying the fractions, ", "it is not possible to", "get this expression" ]
                                        Option2 -> ["By multiplying the fractions, ", "it is not possible to", "get this expression" ]
                                        Option3 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y does not equal", "cos y / sin y" ]
                                        RightOption -> ["By multiplying the fractions", "the sin y in the", "numerator and denominator", "can be cancelled out.", "Therefore, this option is correct"]
                (Question1, Step3) -> case option of
                                        Option1 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "sin y / cos y" ]
                                        Option2 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "cos y / sin y" ]
                                        Option3 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y does not equal", "cos y / sin y" ]
                                        RightOption -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y can be", "written as", "sin y / cos y.", "Therefore, this option is correct"]
                (Question2, Step1) -> case option of
                                        Option1 -> ["This is not ","what is obtained", "when sin is taken as", "the common denominator"  ]
                                        Option2 -> ["This is not ","what is obtained", "when sin is taken as", "the common denominator"  ]
                                        Option3 -> ["This is not ","what is obtained", "when sin is taken as", "the common denominator"  ]
                                        RightOption -> ["This is what is obtained when sin"," is taken as a denominator",  "Therefore, this option is correct"]
                (Question2, Step2) -> case option of
                                        Option1 -> ["Equation needs to be", "substitued for cotangent"]
                                        Option2 -> ["Equation needs to be", "substitued for cotangent"]
                                        Option3 -> ["Equation needs to be", "substitued for cotangent"]
                                        RightOption -> ["Equation is substitued for contangent", "Therefore, this option is correct"]
                (Question2, Step3) -> case option of
                                        Option1 -> ["Use csc x = 1/sin x" ]
                                        Option2 -> ["Use csc x = 1/sin x" ]
                                        Option3 -> ["Use csc x = 1/sin x"  ]
                                        RightOption -> ["Relationship between secant and csc","is used", "Therefore, this option is correct"]
                (Question2, Step4) -> case option of
                                        Option1 -> ["constant/constant^2 = 1/constant" ]
                                        Option2 -> ["constant/constant^2 = 1/constant" ]
                                        Option3 -> ["constant/constant^2 = 1/constant"  ]
                                        RightOption -> ["This has been reduced correctly", "Therefore, this option is correct"]
                (Question2, Step5) -> case option of
                                        Option1 -> ["Use csc x = 1/sin x" ]
                                        Option2 -> ["Use csc x = 1/sin x" ]
                                        Option3 -> ["Use csc x = 1/sin x"]
                                        RightOption -> ["csc x = 1/sin x","has been correctly used", "Therefore, this option is correct"]
                (Question3, Step1) -> case option of
                                        Option1 -> ["cot x does not equal cos2x" ]
                                        Option2 -> ["cot x should be substituted", " in with the correct values" ]
                                        Option3 -> ["The identity has", "not been used", "correctly" ]
                                        RightOption -> ["The identity has", "been used", "correctly", "Therefore, this option is correct"]
                (Question3, Step2) -> case option of
                                        Option1 -> ["csc x is equal to 1/sin x","and that should be substituted" ]
                                        Option2 -> ["csc x is equal to 1/sin x","and that should be substituted" ]
                                        Option3 -> ["csc x is equal to 1/sin x","and that should be substituted" ]
                                        RightOption -> ["The fraction has","been substituted correctly", "Therefore, this option is correct"]
                (Question3, Step3) -> case option of
                                        Option1 -> ["Fractions in the former","step have not been reduced","correctly" ]
                                        Option2 -> ["Fractions in the former","step have not been reduced","correctly"  ]
                                        Option3 -> ["Fractions in the former","step have not been reduced","correctly" ]
                                        RightOption -> ["Fractions have been reduced", "correctly", "Therefore, this option is correct"]
                (Question4, Step1) -> case option of
                                        Option1 -> ["tan y", "simplified to", "sin y / cos y" ]
                                        Option2 -> ["tan y", "simplified to", "sin y / cos y"]
                                        Option3 -> ["tan y", "simplified to", "sin y / cos y" ]
                                        RightOption -> [ "tan y and cot y", "have been simplified", "correctly", "Therefore, this option is correct"]
                (Question4, Step2) -> case option of
                                        Option1 -> ["Reduce fractions","in the former step correctly" ]
                                        Option2 -> ["Reduce fractions","in the former step correctly"  ]
                                        Option3 -> ["Reduce fractions","in the former step correctly"  ]
                                        RightOption -> ["Fractions in the former","step have been added ","and reduced correctly", "Therefore, this option is correct"]
                (Question4, Step3) -> case option of
                                        Option1 -> ["Reduce the fraction by","crossing out the like terms","in numerator and denominator" ]
                                        Option2 -> ["Reduce the fraction by","crossing out the like terms","in numerator and denominator" ]
                                        Option3 -> ["Reduce the fraction by","crossing out the like terms","in numerator and denominator"  ]
                                        RightOption -> ["Fractions have been","reduced correctly", "Therefore, this option is correct"]
                (Question5, Step1) -> case option of
                                        Option1 -> ["Substitute for csc" ]
                                        Option2 -> ["Substitute for csc"  ]
                                        Option3 -> ["Substitute for csc" ]
                                        RightOption -> ["This has been ","substituted correctly for csc", "Therefore, this option is correct"]
                (Question5, Step2) -> case option of
                                        Option1 -> ["Please reduce the fraction", " by adding first ","and then multiplying" ]
                                        Option2 -> ["Please reduce the fraction", " by adding first ","and then multiplying" ] 
                                        Option3 -> ["Please reduce the fraction", " by adding first ","and then multiplying" ] 
                                        RightOption -> ["Fractions have been ","reduced correctly", "Therefore, this option is correct"]
                (Question5, Step3) -> case option of
                                        Option1 -> ["Use the correct identity"," sin^2 x + cos^2 x = 1"," to simplify " ]
                                        Option2 -> ["Use the correct identity"," sin^2 x + cos^2 x = 1"," to simplify "]
                                        Option3 -> ["Use the correct identity"," sin^2 x + cos^2 x = 1"," to simplify "]
                                        RightOption -> ["Correct identity has","been used to simplify", "Therefore, this option is correct"]
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
                                , hintText (explainationStr question step option)
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

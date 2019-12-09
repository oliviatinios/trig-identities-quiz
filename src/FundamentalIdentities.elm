module FundamentalIdentities exposing (..)

{-

    This file contains the code for the Fundamental Identities Screen.

-}

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import String exposing (..)
import Tuple



init =
    { time = 0
    , answerState = Default
    , step = Step1
    , question = Question1
    , hintState = NoPopUp
    , explanationState = NoPopUpExplanation
    , optionColourA = orange -- colour for multiple choice option A
    , optionColourB = orange -- colour for multiple choice option B
    , optionColourC = orange -- colour for multiple choice option C
    , optionColourD = orange -- colour for multiple choice option D
    , option = 0 -- 0 represents multiple choice option A, 1 represents option B, and so on...
    , optionSelected = False -- True if a multiple choice option has been selected
    , state = None
    }


type HintState = NoPopUp | PopUp Questions Steps


type ExplanationState = NoPopUpExplanation | PopUpExplanation Questions Steps Int


type AnswerState = Default | Incorrect | Correct


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


type State
    = None
    | Hint
    | Explanation


-- Helper function
-- Takes a parameter of type Steps and returns the corresponding index
getIndexFromStep step = case step of
                Step1 -> 0
                Step2 -> 1
                Step3 -> 2
                Step4 -> 3
                Step5 -> 4


-- Takes a parameter of type Questions and returns the corresponding question title as a string
questionTitleStr question = case question of
                Question1 -> "Question 1"
                Question2 -> "Question 2"
                Question3 -> "Question 3"
                Question4 -> "Question 4"
                Question5 -> "Question 5"


-- Takes a parameter of type Questions and returns the corresponding question as a string
questionStr question = case question of
                Question1 -> "Prove that tan y / sin y = sec y"
                Question2 -> "Prove that sin y + sin y * cot^2 y = cscy"
                Question3 -> "Prove that cot y / csc y = cos y"
                Question4 -> "Prove that cot(y)+tan(y)=sec(y)*csc(y)"
                Question5 -> "Prove that (1-sin(y))(1+csc(y))=cos(y)*cot(y)"


-- Takes a parameter of type Steps and returns the corresponding question as a string
stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


-- Takes a parameter of type Questions and returns the corresponding steps as a list of strings
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


-- This function creates the partial solution that is displayed above the multiple choice options
-- Returns a Shape
solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled orange
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


-- Calls the solutionText function and passes in the current step and a list of all steps
solutionSection question step = solutionText step (solutionStr question)


-- Takes two parameters of type Questions and Steps and returns the corresponding list of multiple options
-- Each multiple choice option is a tuple containing a string with option that will be displayed to the screen
-- and an indication of whether or not the option is correct or incorrect
optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) LHS = tan y * (sin y / cos y)", Incorrect)
                                      , ( "b) LHS = tan y * (cos y / sin y)", Incorrect)
                                      , ( "c) LHS = (cos y / sin y) * (1 / sin y)", Incorrect)
                                      , ( "d) LHS = (sin y / cos y) * (1 / sin y)", Correct)
                                      ]
                (Question1, Step2) -> [ ( "a) LHS = sin y / cos y", Incorrect)
                                      , ( "b) LHS = 1 / cos y", Correct)
                                      , ( "c) LHS = 1 / sin y", Incorrect)
                                      , ( "d) LHS = (sin y * cos y)/(cos2 y * sin y)", Incorrect)
                                      ]
                (Question1, Step3) -> [ ( "a) LHS = sec y", Correct)
                                      , ( "b) LHS = tan y", Incorrect)
                                      , ( "c) LHS = cos y", Incorrect)
                                      , ( "d) LHS = sin y * cos y", Incorrect)
                                      ]
                (Question2, Step1) -> [ ( "a) LHS = sin y + sin y * (1 / tan2 y)", Incorrect)
                                      , ( "b) LHS = sin y * (1 + cot^2 y)", Correct)
                                      , ( "c) LHS = sin y + (sin y / tan2 y)", Incorrect)
                                      , ( "d) LHS = sin y + ( (sin y * cos2 y) / sin2 y", Incorrect)
                                      ]
                (Question2, Step2) -> [ ( "a) LHS = sin y * (1 + (cos2 y / sin2 y))", Incorrect)
                                      , ( "b) LHS = sin y * (1 + (sin2 y / cos2 y))", Incorrect)
                                      , ( "c) LHS = sin y * (csc^2 y)", Correct)
                                      , ( "d) LHS = sin y * (cos^2 y)", Incorrect)
                                      ]
                (Question2, Step3) -> [ ( "a) LHS = sin y * (1 / cos2 y)", Incorrect)
                                      , ( "b) LHS = sin y * (cos2 y / sin2 y)", Incorrect)
                                      , ( "c) LHS = (sin2 y / cos y)", Incorrect)
                                      , ( "d) LHS = (sin y / sin^2 y)", Correct)
                                      ]
                (Question2, Step4) -> [ ( "a) LHS = 1 / sin y = csc y", Correct)
                                      , ( "b) LHS = sin y / (1 + tan2 y)=csc y", Incorrect)
                                      , ( "c) LHS = (1 + sin y) / sin y = csc y", Incorrect)
                                      , ( "d) LHS = 1 / sin2 y = csc y", Incorrect)
                                      ]
                (Question2, Step5) -> [ ( "a) LHS = cos y / sin2 y", Incorrect)
                                      , ( "b) LHS = csc y", Correct)
                                      , ( "c) LHS = tan y / (sin y * tan y)", Incorrect)
                                      , ( "d) LHS = cot y", Incorrect)
                                      ]
                (Question3, Step1) -> [ ( "a) LHS = cos2 y / sin2 y",  Incorrect)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", Correct)
                                      , ( "c) LHS = secy / (tan y * tan y)", Incorrect)
                                      , ( "d) LHS = cot y", Incorrect)
                                      ]
                (Question3, Step2) -> [ ( "a) LHS = cos3 y / sin3 y", Incorrect)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", Incorrect)
                                      , ( "c) LHS = (cos(y)/sin(y))/(1/sin(y))", Correct)
                                      , ( "d) LHS = cot y", Incorrect)
                                      ]
                (Question3, Step3) -> [ ( "a) LHS = cos3 y / sin3 y", Incorrect)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", Incorrect)
                                      , ( "c) LHS = cos(y)", Correct)
                                      , ( "d) LHS = csc y", Incorrect)
                                      ]
                (Question4, Step1) -> [ ( "a) LHS = cos2 y / sin2 y", Incorrect)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", Incorrect)
                                      , ( "c) LHS = (cos(y)/sin(y))+(sin(y)/cos(y))", Correct)
                                      , ( "d) LHS = cot y", Incorrect)
                                      ]
                (Question4, Step2) -> [ ( "a) LHS = (cos^2(y)+sin^2(y))/(cos(y)*sin(y))", Correct)
                                      , ( "b) LHS = (cos(y)/sin(y))/csc(y)", Incorrect)
                                      , ( "c) LHS = cos(y)/(1/sin(y)", Incorrect)
                                      , ( "d) LHS = cot y", Incorrect)
                                      ]
                (Question4, Step3) -> [ ( "a) LHS = cos y / sin y", Incorrect)
                                      , ( "b) LHS = (sin(y))/csc(y)", Incorrect)
                                      , ( "c) LHS = 1/(cos(y)*sin(y))", Correct)
                                      , ( "d) LHS = sec y", Incorrect)
                                      ]
                (Question5, Step1) -> [ ( "a) LHS =  sin2 y", Incorrect)
                                      , ( "b) LHS =  cos(y)/csc(y)", Incorrect)
                                      , ( "c) LHS = (cos(y)/sin(y))+(sin(y)/cos(y))", Incorrect)
                                      , ( "d) LHS = (1+(1/sin(y)))(1-(sin(y)))", Correct)
                                      ]
                (Question5, Step2) -> [ ( "a) LHS = (1+sin(y))*(1-sin(y))/sin(y)", Correct)
                                      , ( "b) LHS = (cos(y)/sin(y))", Incorrect)
                                      , ( "c) LHS = sin(y)/(1/tan(y)", Incorrect)
                                      , ( "d) LHS = sin y", Incorrect)
                                      ]
                (Question5, Step3) -> [ ( "a) LHS = (cos^2(y))/sin(y)", Correct)
                                      , ( "b) LHS = (sin(y))/csc(y)", Incorrect)
                                      , ( "c) LHS = (cos(y)*sin(y)", Incorrect)
                                      , ( "d) LHS = csc y", Incorrect)
                                      ]
                otherwise -> []


-- This function takes an index representing a multiple choice option (A, B, C or D) and returns the corresponding colour
getOptionColour idx optionColourA optionColourB optionColourC optionColourD = case idx of
                                                                                0 -> optionColourA
                                                                                1 -> optionColourB
                                                                                2 -> optionColourC
                                                                                3 -> optionColourD
                                                                                otherwise -> optionColourA


-- This function is used to change the colour of a multiple choice option as a user hovers over it with their mouse
-- This is a signifier
updateOptionColourOnHover idx colour optionSelected = if optionSelected then
                                            ChangeOptionColour (\m -> m)
                                        else
                                            case idx of
                                                0 -> ChangeOptionColour (\m -> { m | optionColourA = colour })
                                                1 -> ChangeOptionColour (\m -> { m | optionColourB = colour })
                                                2 -> ChangeOptionColour (\m -> { m | optionColourC = colour })
                                                3 -> ChangeOptionColour (\m -> { m | optionColourD = colour })
                                                otherwise -> ChangeOptionColour (\m -> m)


-- This function is used to change the colour of a multiple choice option when it is selected by the user
-- This follows the feedback principle because it tells the user that they have succesfully selected an option
updateOptionColourOnClick idx firstColour secondColour = case idx of
                                    0 -> ChangeOptionColour (\m -> { m | optionColourA = secondColour, optionColourB = firstColour, optionColourC = firstColour, optionColourD = firstColour })
                                    1 -> ChangeOptionColour (\m -> { m | optionColourA = firstColour, optionColourB = secondColour, optionColourC = firstColour, optionColourD = firstColour})
                                    2 -> ChangeOptionColour (\m -> { m | optionColourA = firstColour, optionColourB = firstColour, optionColourC = secondColour, optionColourD = firstColour })
                                    3 -> ChangeOptionColour (\m -> { m | optionColourA = firstColour, optionColourB = firstColour, optionColourC = firstColour, optionColourD = secondColour })
                                    otherwise -> ChangeOptionColour (\m -> m)


-- This function creates the shapes for the multiple choice options so they can be displayed to the screen
optionsText lst optionSelected optionColourA optionColourB optionColourC optionColourD = group (List.indexedMap (\idx tuple -> text (Tuple.first tuple)
                                                                                                                    |> size 12
                                                                                                                    |> filled (getOptionColour idx optionColourA optionColourB optionColourC optionColourD)
                                                                                                                    |> move ( -130, 45-20*(Basics.toFloat idx))
                                                                                                                    |> notifyEnter (updateOptionColourOnHover idx lightOrange optionSelected)
                                                                                                                    |> notifyLeave (updateOptionColourOnHover idx orange optionSelected)
                                                                                                                    |> notifyTap (updateOptionColourOnClick idx orange lightOrange)
                                                                                                                    |> notifyTap (ClickedOption idx (Tuple.second tuple)) ) lst)


-- Creates the entire multiple choice section including the question
optionsSection question step optionSelected optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled orange
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionSelected optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- Helper function
-- Takes two parameters of type Questions and Steps and returns true if the step is the last step for that question
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step5) -> True
                (Question3, Step3) -> True
                (Question4, Step3) -> True
                (Question5, Step3) -> True
                otherwise -> False


-- This function creates the shapes for the results section so it can be displayed to the screen
-- The results section is displayed below the multiple choice options after the user selects an option
-- The results section contains whether or not the user has selected the correct answer, an explanation button and a next button
resultsSection question step answerState option =
                if (isLastStep question step)
                    then group [ text (if answerState == Incorrect
                                    then "Incorrect. Try again!"
                                    else if answerState == Correct
                                        then "Correct! You have solved the problem."
                                        else ""
                                    )
                                    |> size 12
                                    |> filled (if answerState == Incorrect
                                                    then red
                                                    else if answerState == Correct
                                                        then green
                                                        else white
                                                )
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                , rectangle 90 25
                                    |> filled (if answerState == Default
                                            then blank
                                            else orange
                                            )
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                , text "Explanation"
                                    |> filled (if answerState == Default
                                            then blank
                                            else white
                                            )
                                    |> move ( -115, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                ]
                    else if (answerState == Incorrect)
                        then group [
                                text "Incorrect. Try again!"
                                    |> size 12
                                    |> filled red
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                , rectangle 90 25
                                    |> filled orange
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                , text "Explanation"
                                    |> filled white
                                    |> move ( -115, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                ]
                        else if (answerState == Correct)
                            then group [
                                text "Correct!"
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
                                    |> notifyTap (ClickedExplanation question step option)
                                , text "Explanation"
                                    |> filled white
                                    |> move ( -35, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                ]
                            else group []


-- Takes two parameters of type Questions and Steps and returns the corresponding hint
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


-- Takes two parameters of type Questions and Steps and returns the corresponding explanation
explanationStr question step option = case (question, step) of
                (Question1, Step1) -> case option of
                                        0 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "sin y / cos y"]
                                        1 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "cos y / sin y"]
                                        2 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y does not equal", "cos y / sin y"]
                                        3 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y can be", "written as", "sin y / cos y.", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question1, Step2) -> case option of
                                        0 -> ["By multiplying the fractions, ", "it is not possible to", "get this expression"]
                                        1 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y does not equal", "cos y / sin y"]
                                        2 -> ["By multiplying the fractions, ", "it is not possible to", "get this expression"]
                                        3 -> ["By multiplying the fractions", "the sin y in the", "numerator and denominator", "can be cancelled out.", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question1, Step3) -> case option of
                                        0 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y can be", "written as", "sin y / cos y.", "Therefore, this option is correct"]
                                        1 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "cos y / sin y"]
                                        2 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "1 / sin y cannot be", "simplified to", "cos y / sin y"]
                                        3 -> ["The LHS can be rewritten", "as tan y * (1 / sin y).", "tan y does not equal", "cos y / sin y"]
                                        otherwise -> []
                (Question2, Step1) -> case option of
                                        0 -> ["This is not ","what is obtained", "when sin is taken as", "the common denominator"]
                                        1 -> ["This is what is obtained when sin"," is taken as a denominator",  "Therefore, this option is correct"]
                                        2 -> ["This is not ","what is obtained", "when sin is taken as", "the common denominator"]
                                        3 -> ["This is not ","what is obtained", "when sin is taken as", "the common denominator"]
                                        otherwise -> []
                (Question2, Step2) -> case option of
                                        0 -> ["Equation needs to be", "substitued for cotangent"]
                                        1 -> ["Equation needs to be", "substitued for cotangent"]
                                        2 -> ["Equation is substitued for contangent", "Therefore, this option is correct"]
                                        3 -> ["Equation needs to be", "substitued for cotangent"]
                                        otherwise -> []
                (Question2, Step3) -> case option of
                                        0 -> ["Use csc x = 1/sin x"]
                                        1 -> ["Use csc x = 1/sin x"]
                                        2 -> ["Use csc x = 1/sin x"]
                                        3 -> ["Relationship between secant and csc","is used", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question2, Step4) -> case option of
                                        0 -> ["This has been reduced correctly", "Therefore, this option is correct"]
                                        1 -> ["constant/constant^2 = 1/constant"]
                                        2 -> ["constant/constant^2 = 1/constant"]
                                        3 -> ["constant/constant^2 = 1/constant"]
                                        otherwise -> []
                (Question2, Step5) -> case option of
                                        0 -> ["Use csc x = 1/sin x"]
                                        1 -> ["csc x = 1/sin x","has been correctly used", "Therefore, this option is correct"]
                                        2 -> ["Use csc x = 1/sin x"]
                                        3 -> ["Use csc x = 1/sin x"]
                                        otherwise -> []
                (Question3, Step1) -> case option of
                                        0 -> ["cot x does not equal cos2x"]
                                        1 -> ["The identity has", "been used", "correctly", "Therefore, this option is correct"]
                                        2 -> ["cot x should be substituted", " in with the correct values"]
                                        3 -> ["The identity has", "not been used", "correctly"]
                                        otherwise -> []
                (Question3, Step2) -> case option of
                                        0 -> ["csc x is equal to 1/sin x","and that should be substituted"]
                                        1 -> ["csc x is equal to 1/sin x","and that should be substituted"]
                                        2 -> ["The fraction has","been substituted correctly", "Therefore, this option is correct"]
                                        3 -> ["csc x is equal to 1/sin x","and that should be substituted"]
                                        otherwise -> []
                (Question3, Step3) -> case option of
                                        0 -> ["Fractions in the former","step have not been reduced","correctly"]
                                        1 -> ["Fractions in the former","step have not been reduced","correctly"]
                                        2 -> ["Fractions have been reduced", "correctly", "Therefore, this option is correct"]
                                        3 -> ["Fractions in the former","step have not been reduced","correctly"]
                                        otherwise -> []
                (Question4, Step1) -> case option of
                                        0 -> ["tan y", "simplified to", "sin y / cos y"]
                                        1 -> ["tan y", "simplified to", "sin y / cos y"]
                                        2 -> [ "tan y and cot y", "have been simplified", "correctly", "Therefore, this option is correct"]
                                        3 -> ["tan y", "simplified to", "sin y / cos y"]
                                        otherwise -> []
                (Question4, Step2) -> case option of
                                        0 -> ["Fractions in the former","step have been added ","and reduced correctly", "Therefore, this option is correct"]
                                        1 -> ["Reduce fractions","in the former step correctly"]
                                        2 -> ["Reduce fractions","in the former step correctly"]
                                        3 -> ["Reduce fractions","in the former step correctly"]
                                        otherwise -> []
                (Question4, Step3) -> case option of
                                        0 -> ["Reduce the fraction by","crossing out the like terms","in numerator and denominator"]
                                        1 -> ["Reduce the fraction by","crossing out the like terms","in numerator and denominator"]
                                        2 -> ["Fractions have been","reduced correctly", "Therefore, this option is correct"]
                                        3 -> ["Reduce the fraction by","crossing out the like terms","in numerator and denominator"]
                                        otherwise -> []
                (Question5, Step1) -> case option of
                                        0 -> ["Substitute for csc"]
                                        1 -> ["Substitute for csc"]
                                        2 -> ["Substitute for csc"]
                                        3 -> ["This has been ","substituted correctly for csc", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question5, Step2) -> case option of
                                        0 -> ["Fractions have been ","reduced correctly", "Therefore, this option is correct"]
                                        1 -> ["Please reduce the fraction", " by adding first ","and then multiplying"] 
                                        2 -> ["Please reduce the fraction", " by adding first ","and then multiplying"] 
                                        3 -> ["Please reduce the fraction", " by adding first ","and then multiplying"]
                                        otherwise -> []
                (Question5, Step3) -> case option of
                                        0 -> ["Correct identity has","been used to simplify", "Therefore, this option is correct"]
                                        1 -> ["Use the correct identity"," sin^2 x + cos^2 x = 1"," to simplify "]
                                        2 -> ["Use the correct identity"," sin^2 x + cos^2 x = 1"," to simplify "]
                                        3 -> ["Use the correct identity"," sin^2 x + cos^2 x = 1"," to simplify "]
                                        otherwise -> []
                otherwise -> []


-- Creates the text shape for the hint
hintText lst =  group (List.indexedMap (\idx line -> text line
                                                        |> size 10
                                                        |> centered
                                                        |> filled black
                                                        |> move (0, 14-10*(Basics.toFloat idx)) ) lst)


-- Creates the shapes for the hint pop-up
hintCard question step = group [ rect 130 130 |> filled darkGrey |> makeTransparent 0.5 |> move (5, -5)
                               , rect 130 130 |> filled white |> addOutline (solid 0.3) black
                               , text "Hint!" |> size 16 |> filled orange |> move (-55, 45)
                               , hintText (hintStr question step)
                               ] |> move ( 60, 0 )


-- Creates the shapes for the explanation pop-up
explanationCard question step option = group [ rect 170 130 |> filled darkGrey |> makeTransparent 0.5 |> move (5, -5)
                                             , rect 170 130 |> filled white |> addOutline (solid 0.3) black
                                             , hintText (explanationStr question step option)
                                             , text "X" |> bold |> sansserif |> size 14 |> filled black |> makeTransparent 0.75 |> move (65, 45) |> notifyTap ExitExplanation
                                             ] |> move ( 60, 0 )


-- Messages generated by user interactions
type Msg m
    = Tick Float GetKeyState
    | ClickedOption Int AnswerState
    | NextStep
    | NextQuestion
    | PreviousQuestion
    | ClickedHint Questions Steps
    | ExitHint
    | ClickedExplanation Questions Steps Int
    | ExitExplanation
    | ChangeOptionColour (m -> m)


-- Model
view model =
    [ rectangle 400 312 |> filled blank |> addOutline (solid 2) orange |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled orange |> move ( 20, 120 )
    , triangle 8|> filled (rgb 230 125 50) |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled (rgb 230 125 50) |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled orange |> move ( -130, 95 )
    , resultsSection model.question model.step model.answerState model.option
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionSelected model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) orange |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled orange |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
       ]
    ++
        case model.state of
            Hint -> case model.hintState of
                        PopUp question step -> [hintCard question step]
                        otherwise -> []
            Explanation -> case model.explanationState of
                        PopUpExplanation question step option -> [explanationCard question step option]
                        otherwise -> []
            otherwise -> []


-- Change app's state based on messages
update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
            }

        ClickedOption option answerState
            -> { model
                | option = option
                , answerState = answerState
                , optionSelected = True
                }

        NextStep
            -> { model
                | answerState = Default
                , optionSelected = False
                , optionColourA = orange
                , optionColourB = orange
                , optionColourC = orange
                , optionColourD = orange
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
                | answerState = Default
                , step = Step1
                , optionSelected = False
                , optionColourA = orange
                , optionColourB = orange
                , optionColourC = orange
                , optionColourD = orange
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
                | answerState = Default
                , step = Step1
                , question =
                    case model.question of
                        Question1 -> Question5
                        Question2 -> Question1
                        Question3 -> Question2
                        Question4 -> Question3
                        Question5 -> Question4
                }

        ClickedHint question step
            -> { model
                | state = Hint
                , hintState = PopUp question step
                }

        ClickedExplanation question step option
            -> { model
                | state = Explanation
                , explanationState = PopUpExplanation question step option
                }

        ExitHint
            -> { model
                | state = None
                , hintState = NoPopUp
                }

        ExitExplanation
            -> { model
                | state = None
                , explanationState = NoPopUpExplanation
                }

        ChangeOptionColour t ->
            t model

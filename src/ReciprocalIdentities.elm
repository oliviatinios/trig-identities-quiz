module ReciprocalIdentities exposing (..)

{-

    This file contains the code for the Reciprocal Identities Screen.

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
    , optionColourA = darkBlue -- colour for multiple choice option A
    , optionColourB = darkBlue -- colour for multiple choice option B
    , optionColourC = darkBlue -- colour for multiple choice option C
    , optionColourD = darkBlue -- colour for multiple choice option D
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
                Question1 -> "Which of the following expressions is equal to (sin y - tan y)(cos y - cot y)?"
                Question2 -> "Prove that (sec^2(y) - 1)/sec^2(y) = sin^2(y)"
                Question3 -> "Prove that 2tan(y)sec(y) = 2(sin(y)/cos(y))(1/cos(y))"
                Question4 -> "Prove that 4cos^2(y) - 1 = (2cos(y))^2 - 1"
                Question5 -> "Simplify csc^2(y) - cot^2(y)"


-- Takes a parameter of type Steps and returns the corresponding question as a string
stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


-- Takes a parameter of type Questions and returns the corresponding steps as a list of strings
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


-- This function creates the partial solution that is displayed above the multiple choice options
-- Returns a Shape
solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled darkBlue
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


-- Calls the solutionText function and passes in the current step and a list of all steps
solutionSection question step = solutionText step (solutionStr question)


-- Takes two parameters of type Questions and Steps and returns the corresponding list of multiple options
-- Each multiple choice option is a tuple containing a string with option that will be displayed to the screen
-- and an indication of whether or not the option is correct or incorrect
optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) = ((sin y sin y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", Incorrect)
                                      , ( "b) = ((sin y cos y - sin y)/cos y) ((tan y sin y - cos y)/sin y)", Incorrect)
                                      , ( "c) = ((tan y cos y - sin y)/cos y) ((tan y sin y - cos y)/tan y)", Incorrect)
                                      , ( "d) = ((sin y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", Correct)
                                      ]
                (Question1, Step2) -> [ ( "a) = ((tan y cos y - sin y)/cos y) ((cos y tan y - cos y)/sin y)", Incorrect)
                                      , ( "b) = ((sin y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", Correct)
                                      , ( "c) = ((tan y cos y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", Incorrect)
                                      , ( "d) = ((sin y tan y - sin y)/cos y) ((cos y sin y - cos y)/sin y)", Incorrect)
                                      ]
                (Question1, Step3) -> [ ( "a) = (sin y * ((cos y - 1) / cos y)) (cos y * ((sin y - 1)/sin y))", Correct)
                                      , ( "b) = (tan y * ((cos y - 1) / cos y)) (cos y * ((tan y - 1)/sin y))", Incorrect)
                                      , ( "c) = (tan y * ((cos y - 1) / cos y)) (cos y * ((sin y - 1)/sin y))", Incorrect)
                                      , ( "d) = (sin y * ((cos y - 1) / cos y)) (cos y * ((tan y - 1)/sin y))", Incorrect)
                                      ]
                (Question1, Step4) -> [ ( "a) = sin y cos y * ((cos y - 1) (sin y - 1) / cos y sin y)", Correct)
                                      , ( "b)= sin y cos y * ((sin y - 1) (sin y - 1) / cos y sin y)", Incorrect)
                                      , ( "c) = sin y cos y * ((cos y - 1) (sin y - 1) / sin y sin y)", Incorrect)
                                      , ( "d) = sin y cos y * ((sin y - 1) (sin y - 1) / cos y sin y)", Incorrect)
                                      ]
                (Question1, Step5) -> [ ( "a) = (cos y - 1) (sin y - 1)", Correct)
                                      , ( "b) = (tan y - 1) (sin y - 1)", Incorrect)
                                      , ( "c) = (cos y - 1) (tan y - 1)", Incorrect)
                                      , ( "d) = (sin y - 1) (sin y - 1)", Incorrect)
                                      ]
                (Question2, Step1) -> [ ( "a) RHS = ((cos^2(y) + 1) - 1)/sec^2(y)", Incorrect)
                                      , ( "b) RHS = ((tan^2(y) + 1) - 1)/sec^2(y))", Correct)
                                      , ( "c) RHS = ((cos^2(y) + 1) - 1)/sin^2(y)", Incorrect)
                                      , ( "d) RHS = ((tan^2(y) + 1) - 1)/sin^2(y)", Incorrect)
                                      ]
                (Question2, Step2) -> [ ( "a) RHS = cos^2(y) / sec^2(y)", Incorrect)
                                      , ( "b) RHS = cos^2(y) / tan^2(y)", Incorrect)
                                      , ( "c) RHS = tan^2(y) / sec^2(y)", Correct)
                                      , ( "d) RHS = sec^2(y) / tan^2(y)", Incorrect)
                                      ]
                (Question2, Step3) -> [ ( "a) RHS = sec^2(y) * (1/tan^2(y))", Incorrect)
                                      , ( "b) RHS = sec^2(y) * (2/tan^2(y))", Incorrect)
                                      , ( "c) RHS = tan^2(y) * (2/sec^2(y))", Incorrect)
                                      , ( "d) RHS = tan^2(y) * (/sec^2(y))", Correct)
                                      ]
                (Question2, Step4) -> [ ( "a) RHS = tan^2(y) cos^2(y) ", Correct)
                                      , ( "b) RHS = cos^2(y) tan^2(y) ", Incorrect)
                                      , ( "c) RHS = cos^2(2y) tan^2(y) ", Incorrect)
                                      , ( "d) RHS = tan^2(2y) cos^2(y) ", Incorrect)
                                      ]
                (Question2, Step5) -> [ ( "a) RHS = (cos^2(y) / cos^2(y))(cos^2(y)) = sin^2(y)", Incorrect)
                                      , ( "b) RHS = (sin^2(y) / cos^2(y))(cos^2(y)) = sin^2(y)", Correct)
                                      , ( "c) RHS = (cos^2(y) / cos^2(y))(tan^2(y)) = sin^2(y)", Incorrect)
                                      , ( "d) RHS = (sin^2(y) / cos^2(y))(tan^2(y)) = sin^2(y)", Incorrect)
                                      ]
                (Question3, Step1) -> [ ( "a) RHS = 4sin(y)/cos^2(y)", Incorrect)
                                      , ( "b) RHS = 2sin(y)/cos^2(y)", Correct)
                                      , ( "c) RHS = 4sin(y)/cos^4(y)", Incorrect)
                                      , ( "d) RHS = 2sin(y)/cos^4(y)", Incorrect)
                                      ]
                (Question3, Step2) -> [ ( "a) RHS = sin(y)/(1-cot^2(y)", Incorrect)
                                      , ( "b) RHS = tan(y)/(1-cot^2(y)", Incorrect)
                                      , ( "c) RHS = sin(y)/(1-sin^2(y)", Correct)
                                      , ( "d) RHS = tan(y)/(1-sin^2(y)", Incorrect)
                                      ]
                (Question4, Step1) -> [ ( "a) RHS = (2cos(y) - 1)(2(tan(y)) + 1)", Incorrect)
                                      , ( "b) RHS = (2sin(y) - 1)(2(tan(y)) + 1)", Incorrect)
                                      , ( "c) RHS = (2cos(y) - 1)(2(cos(y)) + 1)", Correct)
                                      , ( "d) RHS = (2cos(y) - 1)(2(cos(y)) + 1)", Incorrect)
                                      ]
                (Question5, Step1) -> [ ( "a) = 1 + csc^2(y) - cot^2(y)", Incorrect)
                                      , ( "b) = 1 + cot^2(y) - csc^2(y)", Incorrect)
                                      , ( "c) = 1 + csc^2(y) - csc^2(y)", Incorrect)
                                      , ( "d) = 1 + cot^2(y) - cot^2(y)", Correct)
                                      ]
                (Question5, Step2) -> [ ( "a) = 1", Correct)
                                      , ( "b) = cosy", Incorrect)
                                      , ( "c) = siny/tany", Incorrect)
                                      , ( "d) = tany/siny", Incorrect)
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
                                                                                                                    |> notifyEnter (updateOptionColourOnHover idx lightBlue optionSelected)
                                                                                                                    |> notifyLeave (updateOptionColourOnHover idx darkBlue optionSelected)
                                                                                                                    |> notifyTap (updateOptionColourOnClick idx darkBlue lightBlue)
                                                                                                                    |> notifyTap (ClickedOption idx (Tuple.second tuple)) ) lst)


-- Creates the entire multiple choice section including the question
optionsSection question step optionSelected optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled darkBlue
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionSelected optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- Helper function
-- Takes two parameters of type Questions and Steps and returns true if the step is the last step for that question
isLastStep question step = case (question, step) of
                (Question1, Step5) -> True
                (Question2, Step5) -> True
                (Question3, Step2) -> True
                (Question4, Step1) -> True
                (Question5, Step2) -> True
                otherwise -> False


-- This function creates the shapes for the results section so it can be displayed to the screen
-- The results section is displayed below the multiple choice options after the user selects an option
-- The results section contains whether or not the user has selected the correct answer, an explanation button and a next button
resultsSection question step answerState option =
                if (isLastStep question step)
                    then group [ text (if answerState == Incorrect
                                    then "Incorrect"
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
                                            else darkBlue
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
                                text "Incorrect"
                                    |> size 12
                                    |> filled red
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                , rectangle 90 25
                                    |> filled darkBlue
                                    |> move ( -85, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                , text "Explanation"
                                    |> filled white
                                    |> move ( -115, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                ]
                        else if (answerState == Correct)
                            then group [
                                text "Correct"
                                    |> size 12
                                    |> filled green
                                    |> move ( -130, -40 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                 , rectangle 60 25
                                    |> filled darkBlue
                                    |> move ( -100, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , text "Next"
                                    |> filled white
                                    |> move ( -112, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , rectangle 90 25
                                    |> filled darkBlue
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


-- Takes two parameters of type Questions and Steps and returns the corresponding explanation
explanationStr question step option = case (question, step) of
                (Question1, Step1) -> case option of
                                        0 -> ["You cannot derive", "sin y sin y." ]
                                        1 -> ["You cannot derive", "cos y sin y."]
                                        2 -> ["You cannot derive", "tan y cos y" ]
                                        3 -> ["You can derive", "this equation.", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question1, Step2) -> case option of
                                        0 -> ["You cannot derive", "tan y cos y" ]
                                        1 -> ["You can derive", "sin y cos y.", "Therefore, this option is correct."]
                                        2 -> ["You cannot derive", "tan y cos y" ]
                                        3 -> ["You cannot derive", "sin y tan y." ]
                                        otherwise -> []
                (Question1, Step3) -> case option of
                                        0 -> ["You can derive this equtaion.", "Therefore, this option is correct"]
                                        1 -> ["You cannot derive", "tan y * ((cos y - 1) / cosy)" ]
                                        2 -> ["You cannot derive", "tan y * ((cos y - 1) / cosy)" ]
                                        3 -> ["You cannot derive", "tan y * (cos y * ((tan y -1)/sin y)"  ]
                                        otherwise -> []
                (Question1, Step4) -> case option of
                                        0 -> ["You can derive this equtaion.", "Therefore, this option is correct"]
                                        1 -> ["You cannot derive", "((siny - 1)(siny-1) / cosy sin y)" ]
                                        2 -> ["You cannot derive", "the sin y sin y", "towards the end of this equation" ]
                                        3 -> ["You cannot derive", "((siny - 1)(siny-1) / cosy sin y)"  ]
                                        otherwise -> []
                (Question1, Step5) -> case option of
                                        0 -> ["You can derive this equtaion.", "Therefore, this option is correct"]
                                        1 -> ["You cannot derive", "(tany - 1){siny - 1)" ]
                                        2 -> ["You cannot derive", "(cosy - 1){tany - 1)" ]
                                        3 -> ["You cannot derive", "(tany - 1)(siny - 1)"  ]
                                        otherwise -> []
                (Question2, Step1) -> case option of
                                        0 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        1 -> ["You can derive this.", "Therefore, this option is correct"]
                                        2 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        3 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        otherwise -> []
                (Question2, Step2) -> case option of
                                        0 -> ["You cannot derive this.", "To get the answerState,", " you only need to perform basic arithmetic!"]
                                        1 -> ["You cannot derive this.", "To get the answerState,", " you only need to perform basic arithmetic!" ]
                                        2 -> ["Yes, 1-1 = 0 so they cancel out.", "Therefore, this option is correct"]
                                        3 -> ["You cannot derive this.", "To get the answerState,", " you only need to perform basic arithmetic!"]
                                        otherwise -> []
                (Question2, Step3) -> case option of
                                        0 -> ["You cannot derive this.","Try rewriting the equation to represent the", "latter part of this equation as", "a fraction" ]
                                        1 -> ["You cannot derive this.","Try rewriting the equation to represent the", "latter part of this equation as", "a fraction" ]
                                        2 -> ["You cannot derive this.","Try rewriting the equation to represent the", "latter part of this equation as", "a fraction" ]
                                        3 -> ["You can rewrite the denomitor", "as 1/sec^2(y)", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question2, Step4) -> case option of
                                        0 -> ["You can derive this.", "Therefore, this option is correct"]
                                        1 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        2 -> ["You cannot derive this.", "Think of one of the fundamental trig identities."]
                                        3 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        otherwise -> []
                (Question2, Step5) -> case option of
                                        0 -> ["You cannot derive this.", "Think of one of the recipricoal identities." ]
                                        1 -> ["You can derive this.", "Therefore, this option is correct"]
                                        2 -> ["You cannot derive this.", "Think of one of the recipricoal identities."]
                                        3 -> ["You cannot derive this.", "Think of one of the recipricoal identities." ]
                                        otherwise -> []
                (Question3, Step1) -> case option of
                                        0 -> ["No this cannot be correct", "because you can't get 4" ]
                                        1 -> ["Yes, the cos y can be multiplied to cos^2(y).", "Therefore, this option is correct"]
                                        2 -> ["No this cannot be correct", "because you can't get 4"  ]
                                        3 -> ["No this cannot be correct", "because you can't getcos^4(y)"  ]
                                        otherwise -> []
                (Question3, Step2) -> case option of
                                        0 -> ["You cannot derive this.", "Try substituting in 1 - sin^2y." ]
                                        1 -> ["You cannot derive this.", "Try substituting in 1 - sin^2y."  ]
                                        2 -> ["Yes, you can derive this.", "Therefore, this option is correct"]
                                        3 -> ["You cannot derive this.", "Try substituting in 1 - sin^2y."  ]
                                        otherwise -> []
                (Question4, Step1) -> case option of
                                        0 -> ["You cannot derive this.", "Try using the difference of squares." ]
                                        1 -> ["You cannot derive this.", "Try using the difference of squares."  ]
                                        2 -> ["Yes, you can derive this.", "Therefore, this option is correct"]
                                        3 -> ["You cannot derive this.", "Try using the difference of squares."]
                                        otherwise -> []
                (Question5, Step1) -> case option of
                                        0 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        1 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        2 -> ["You cannot derive this.", "Think of one of the fundamental trig identities." ]
                                        3 -> ["Yes, csc^2(y) can be rewritten", "as 1+cot^2(y)", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question5, Step2) -> case option of
                                        0 -> ["Yes, the cot^2y substract each other.", "Therefore, this option is correct"]
                                        1 -> ["You cannot derive this.", "There are terms which you subtract in the expression." ]
                                        2 -> ["You cannot derive this.", "There are terms which you subtract in the expression." ]
                                        3 -> ["You cannot derive this.", "There are terms which you subtract in the expression." ]
                                        otherwise -> []
                otherwise -> []


-- Creates the text shape for the hint
hintText lst =  group (List.indexedMap (\idx line -> text line
                                                        -- |> sansserif
                                                        |> size 10
                                                        |> centered
                                                        |> filled black
                                                        |> move (0, 14-10*(Basics.toFloat idx)) ) lst)


-- Creates the shapes for the hint pop-up
hintCard question step = group [rect 130 130 |> filled grey |> addOutline (solid 0.3) black
                               , hintText (hintStr question step)
                               ] |> move ( 60, 0 )


-- Creates the shapes for the explanation pop-up
explanationCard question step option = group [rect 170 130 |> filled grey |> addOutline (solid 0.3) black
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
    [ rectangle 400 300 |> filled blank |> addOutline (solid 2) darkBlue |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled darkBlue |> move ( 20, 120 )
    , triangle 8|> filled darkBlue |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled darkBlue |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled darkBlue |> move ( -130, 95 )
    , resultsSection model.question model.step model.answerState model.option
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionSelected model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) darkBlue |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled darkBlue |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
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
                , optionColourA = darkBlue
                , optionColourB = darkBlue
                , optionColourC = darkBlue
                , optionColourD = darkBlue
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
                , optionColourA = darkBlue
                , optionColourB = darkBlue
                , optionColourC = darkBlue
                , optionColourD = darkBlue
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
            -> {model
                | state = Hint
                , hintState = PopUp question step
                }

        ClickedExplanation question step option
            -> {model
                | state = Explanation
                , explanationState = PopUpExplanation question step option
                }

        ExitHint
            -> {model
                | state = None
                , hintState = NoPopUp
                }

        ExitExplanation
            -> {model
                | state = None
                , explanationState = NoPopUpExplanation
                }

        ChangeOptionColour t ->
            t model










































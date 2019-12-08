module PythagoreanIdentities exposing (..)

{-

    This file contains the code for the Pythagorean Identities Screen.

-}

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import String exposing (..)
import Tuple



init =
    { time = 0
    , notify = NotifyTap
    , answerState = Default
    , step = Step1
    , question = Question1
    , hintState = NoPopUp
    , explanationState = NoPopUpExplanation
    , optionColourA = purple -- colour for multiple choice option A
    , optionColourB = purple -- colour for multiple choice option B
    , optionColourC = purple -- colour for multiple choice option C
    , optionColourD = purple -- colour for multiple choice option D
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
                Question1 -> "Simplify sin(y)*cos^2*(y)-sin(y)"
                Question2 -> "Simplify csc^2(y)-cot(y)-3"
                Question3 -> "Simplify 1 / (1-cos^2(y))"
                Question4 -> "Simplify (1-cos^2(y))/(sin(y)*cos(y))"
                Question5 -> "Simplify cot(2y)*cos(y)*sin(y)+cos(y)*sin(y)"


-- Takes a parameter of type Steps and returns the corresponding question as a string
stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


-- Takes a parameter of type Questions and returns the corresponding steps as a list of strings
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


-- This function creates the partial solution that is displayed above the multiple choice options
-- Returns a Shape
solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled purple
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


-- Calls the solutionText function and passes in the current step and a list of all steps
solutionSection question step = solutionText step (solutionStr question)


-- Takes two parameters of type Questions and Steps and returns the corresponding list of multiple options
-- Each multiple choice option is a tuple containing a string with option that will be displayed to the screen
-- and an indication of whether or not the option is correct or incorrect
optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) tan y * (sin y / cos y)", Incorrect)
                                      , ( "b) tan y * (cos y / sin y)", Incorrect)
                                      , ( "c) (cos y / sin y) * (1 / sin y)", Incorrect)
                                      , ( "d) sin y(cos^2(y)-1)", Correct)
                                      ]
                (Question1, Step2) -> [ ( "a) csc y / cos y", Incorrect)
                                      , ( "b) sin y(-sin^2(y))", Correct)
                                      , ( "c) 2 / sin y", Incorrect)
                                      , ( "d) (cos2 y * sin y)", Incorrect)
                                      ]
                (Question1, Step3) -> [ ( "a) -sin^3(y)", Correct)
                                      , ( "b) tan y", Incorrect)
                                      , ( "c) cos y", Incorrect)
                                      , ( "d) sin y", Incorrect)
                                      ]
                (Question2, Step1) -> [ ( "a) (1 / tan2 y)", Incorrect)
                                      , ( "b) 1+cot^2(y)-cot(y)-3", Correct)
                                      , ( "c) cos y + (sin y / tan2 y)", Incorrect)
                                      , ( "d) (sin y * cos2 y) / sin2 y", Incorrect)
                                      ]
                (Question2, Step2) -> [ ( "a) (1 + (cos2 y / sin2 y))", Incorrect)
                                      , ( "b) (1 + (sin2 y / cos2 y))", Incorrect)
                                      , ( "c) cot^2(y)-cot(y)-2", Correct)
                                      , ( "d) cot y * (cot2 y)", Incorrect)
                                      ]
                (Question2, Step3) -> [ ( "a) cot y * (1 / cos2 y)", Incorrect)
                                      , ( "b) sin y * (cos2 y / cot2 y)", Incorrect)
                                      , ( "c) (sin2 y / cot y+1)", Incorrect)
                                      , ( "d) (cot(y)-2)*(cot(y)+1)", Correct)
                                      ]
                (Question3, Step1) -> [ ( "a) cos2 y / sin2 y", Incorrect)
                                      , ( "b) 1/sin^2(y)", Correct)
                                      , ( "c) secy / (tan y * tan y)", Incorrect)
                                      , ( "d) sin y", Incorrect)
                                      ]
                (Question3, Step2) -> [ ( "a) cos3 y / sin3 y", Incorrect)
                                      , ( "b) (cos(y)/sin(y))/csc(y)", Incorrect)
                                      , ( "c) csc^2(y)", Correct)
                                      , ( "d) sin y", Incorrect)
                                      ]
                (Question4, Step1) -> [ ( "a) cos2 y / sin2 y", Incorrect)
                                      , ( "b) (cos(y)/sin(y))/csc(y)", Incorrect)
                                      , ( "c) sin^2(y)/(cos(y)sin(y))", Correct)
                                      , ( "d) csc y", Incorrect)
                                      ]
                (Question4, Step2) -> [ ( "a) sin(y)/cos(y)", Correct)
                                      , ( "b) (cos(y)/sin(y))/sin(y)", Incorrect)
                                      , ( "c) cos(y)/(1/cos(y)", Incorrect)
                                      , ( "d) cot y", Incorrect)
                                      ]
                (Question4, Step3) -> [ ( "a) cos y / sin y", Incorrect)
                                      , ( "b) (sin(y))/csc(y)", Incorrect)
                                      , ( "c) tan(y)", Correct)
                                      , ( "d) sec y", Incorrect)
                                      ]
                (Question5, Step1) -> [ ( "a) sin2 y", Incorrect)
                                      , ( "b) cos2(y)/csc(y)", Incorrect)
                                      , ( "c) (cos(y)/sin(y))+(sin(y)/cos(y))", Incorrect)
                                      , ( "d) (sin(2y)/2)+(cos(y)*cot(2*y)*sin(y))", Correct)
                                      ]
                (Question5, Step2) -> [ ( "a) (sin(2y)/2)+(sin(2y)*cot(2y))/2", Correct)
                                      , ( "b) csc y/(cos(y)/sin(y))", Incorrect)
                                      , ( "c) tan(y)/(1/tan(y)", Incorrect)
                                      , ( "d) cos y", Incorrect)
                                      ]
                (Question5, Step3) -> [ ( "a) (sin(2y)+sin(2y)cot(2y))/2", Correct)
                                      , ( "b) (cot(y))/csc(y)", Incorrect)
                                      , ( "c) (csc(y)*sin(y)", Incorrect)
                                      , ( "d) csc y", Incorrect)
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
                                                                                                                    |> notifyEnter (updateOptionColourOnHover idx lightPurple optionSelected)
                                                                                                                    |> notifyLeave (updateOptionColourOnHover idx purple optionSelected)
                                                                                                                    |> notifyTap (updateOptionColourOnClick idx purple lightPurple)
                                                                                                                    |> notifyTap (ClickedOption idx (Tuple.second tuple)) ) lst)


-- Creates the entire multiple choice section including the question
optionsSection question step optionSelected optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled purple
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionSelected optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- Helper function
-- Takes two parameters of type Questions and Steps and returns true if the step is the last step for that question
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step3) -> True
                (Question3, Step2) -> True
                (Question4, Step3) -> True
                (Question5, Step3) -> True
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
                                            else purple
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
                                    |> filled purple
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
                                    |> notifyTap (ClickedExplanation question step option)
                                , text "Explanation"
                                    |> filled white
                                    |> move ( -35, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap (ClickedExplanation question step option)
                                ]
                            else group []


-- Takes two parameters of type Questions and Steps and returns the corresponding hint
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


-- Takes two parameters of type Questions and Steps and returns the corresponding explanation
explanationStr question step option = case (question, step) of
                (Question1, Step1) -> case option of
                                        0 -> ["tan y equals sin y/cos y. ", "Multiplying tan y by sin y / cos y", "does not lead back", "to the original expression." ]
                                        1 -> ["tan y equals sin y/cos y. ", "Multiplying tan y by cos y / sin y", "does not lead back", "to the original expression." ]
                                        2 -> ["It is not possible", "to simplify to this expression.", "You can double check by", "expanding this expression and checking", "if it equals the original expression." ]
                                        3 -> ["sin y is common in both", "the terms around the subtraction.", "We can factor out sin y. ", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question1, Step2) -> case option of
                                        0 -> ["csy equals 1/siny, ", "This expression would", "expand to 1/(siny * cosy).", "This cannot be derived back to Step 1." ]
                                        1 -> ["After substituting", "1 as sin2y + cos2y,", "the expression becomes", "siny(cos2y - sin2y - cos2y).", "The cos2y terms cancel out.", "Therefore, this option is correct"]
                                        2 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        3 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        otherwise -> []
                (Question1, Step3) -> case option of
                                        0 -> ["Multiplying siny with -sin2y", "equals -sin3y.", "Therefore, this option is correct"]
                                        1 -> ["It is not possible", "to get cos y", "from Step 2." ]
                                        2 -> ["It is not possible", "to get sin y", "from Step 2." ]
                                        3 -> ["It is not possible", "to get cos y", "from Step 2." ]
                                        otherwise -> []
                (Question2, Step1) -> case option of
                                        0 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        1 -> ["We can use the", "1 + cot2y = csc2y identity", "and subsitute it in as csc2y.", "Therefore, this option is correct"]
                                        2 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        3 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        otherwise -> []
                (Question2, Step2) -> case option of
                                        0 -> ["Leaving the constant 1 as is,", "cot2y - coty - 3 cannot", "be simplified to", "cos2y/sin2y." ]
                                        1 -> ["Leaving the constant 1 as is,", "cot2y - coty - 3 cannot", "be simplified to", "sin2y/cos2y." ]
                                        2 -> ["We can simplify further by", "evaluating the constants.", "1 - 3 equals -2.", "Therefore, this option is correct"]
                                        3 -> ["It is not possible", "to simplify to this expression.", "You can double check by", "expanding this expression and checking", "if it equals Step 1.", "cot y * (cot2 y) equals cot3 y", "cot3 y cannot be expanded", "to the expression in Step 1" ]
                                        otherwise -> []
                (Question2, Step3) -> case option of
                                        0 -> ["It is not possible", "to simplify to this expression.", "You can double check by", "expanding this expression and checking", "if it equals Step 2.", "cot y * (1/cos2 y) equals cot y / cos2 y.", "This cannot be expanded", "to the expression in Step 2" ]
                                        1 -> ["It is not possible to get this expression.", "You can double check by", "expanding this expression and checking", "if it equals Step 2.", "sin y * (cos2 y/cot2 y) equals", "(sin y * cos2 y) / cot2 y.", "This cannot be expanded", "to the expression in Step 2" ]
                                        2 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        3 -> ["We can express the expression", "in terms of factors.", "Using the quadratic formula, ", "we get (cot y - 2)(cot y + 1).", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question3, Step1) -> case option of
                                        0 -> ["There are no identities", "that can be used to", "get this expression" ]
                                        1 -> ["We can use the identity:", "sin2y + cos2y = 1 by rearranging it to", "sin2y = 1 - cos2y.", "We can now substitute the denominator", "with sin2y.", "Therefore, this option is correct."]
                                        2 -> ["It is not possible", "to simplify the expression", "to secy / (tan y * tan y)" ]
                                        3 -> ["It is not possible", "to simplify the expression", "to sin y" ]
                                        otherwise -> []
                (Question3, Step2) -> case option of
                                        0 -> ["It is not possible", "to simplify the expression", "to cos3 y/ sin3 y using 1/siny = cscy" ]
                                        1 -> ["It is not possible", "to simplify the expression", "to (cosy/siny)/ csc y using 1/siny = cscy" ]
                                        2 -> ["Using 1/siny = cscy", "the expression simplifies to csc^2y.", "Therefore, this option is correct"]
                                        3 -> ["It is not possible", "to simplify the expression", "to sin y using 1/siny = cscy" ]
                                        otherwise -> []
                (Question4, Step1) -> case option of
                                        0 -> ["It is not possible", "to simplify the expression", "to cos2 y/ sin2 y", "using sin2y + cos2y = 1" ]
                                        1 -> ["It is not possible", "to simplify the expression", "to (cos y/ sin y)/csc y", "using sin2 y + cos2 y = 1" ]
                                        3 -> ["We can use the identity:", "sin2 y + cos2 y = 1 by rearranging it to", "sin2 y = 1 - cos2 y.", "We can now substitute the numerator", "with sin2y.", "Therefore, this option is correct."]
                                        2 -> ["It is not possible", "to simplify the expression", "to csc y", "using sin2 y + cos2 y = 1" ]
                                        otherwise -> []
                (Question4, Step2) -> case option of
                                        0 -> ["sin y can be factored out", "from the numerator and denominator", "and then cancelled out.", "Therefore, this option is correct"]
                                        1 -> ["It is not possible", "to simplify the expression.", "to (cos y / sin y) / sin y." ]
                                        2 -> ["It is not possible", "to simplify the expression.", "to cos y / (1 / sin y)." ]
                                        3 -> ["cot y equals cos y / sin y", "It is not possible", "to simplify to cot y." ]
                                        otherwise -> []
                (Question4, Step3) -> case option of
                                        0 -> ["It is not possible", "to simplify the expression.", "to cos y / sin y." ]
                                        1 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to sin y / csc y."]
                                        2 -> ["We can use the identity:", "tan y = sin y / cos y.", "Therefore, this option is correct"]
                                        3 -> ["sec y equals 1 / cos y.", "It is not possible", "to simplify the expression.", "to sec y." ]
                                        otherwise -> []
                (Question5, Step1) -> case option of
                                        0 -> ["It is not possible", "to simplify the expression.", "to sin2 y." ]
                                        1 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to cos2 y/csc y." ]
                                        2 -> ["cot2 y can be simplified to", "cos2y / sin2 y where sin y can", "be factored out, but", "it would still not simplify to", "(cos y/sin y) + (sin y / cos y).", "There is an easier simplification." ]
                                        3 -> ["We can use the identity:", "cos y * sin y = sin 2y/2", "and substitute that in.", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question5, Step2) -> case option of
                                        0 -> ["We can use the identity:", "cos y * sin y = sin 2y/2 again", "and substitute that in the second term.", "Therefore, this option is correct"]
                                        1 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to csc y / (cos y / sin y)." ]
                                        2 -> ["tan y equals sin y / cos y.", "It is not possible", "to simplify the expression.", "to tan y / (1 / tan y)." ]
                                        3 -> ["It is not possible", "to simplify the expression.", "to cos y." ]
                                        otherwise -> []
                (Question5, Step3) -> case option of
                                        0 -> ["Since the two terms have", "the same denominator, ", "we can add the two fractions together.", "Therefore, this option is correct"]
                                        1 -> ["cot y equals cos y / sin y", "and csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to cot y / csc y." ]
                                        2 -> ["csc y equals 1 / sin y.", "It is not possible", "to simplify the expression.", "to csc y * sin y." ]
                                        3 -> ["It is not possible", "to simplify the expression.", "to cos y." ]
                                        otherwise -> []
                otherwise -> []


-- Creates the text shape for the hint
hintText lst =  group (List.indexedMap (\idx line -> text line
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
    | Notif Notifications
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
    [ rectangle 400 300 |> filled blank |> addOutline (solid 2) purple |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled purple |> move ( 20, 120 )
    , triangle 8|> filled purple |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled purple |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled purple |> move ( -130, 95 )
    , resultsSection model.question model.step model.answerState model.option
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionSelected model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) purple |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled purple |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
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

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            { model | notify = notif }

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
                , optionColourA = purple
                , optionColourB = purple
                , optionColourC = purple
                , optionColourD = purple
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
                , optionColourA = purple
                , optionColourB = purple
                , optionColourC = purple
                , optionColourD = purple
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

--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }


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

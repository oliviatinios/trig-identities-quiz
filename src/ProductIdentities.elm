module ProductIdentities exposing (..)

{-

    This file contains the code for the Product Identities Screen.

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
    , optionColourA = darkGreen -- colour for multiple choice option A
    , optionColourB = darkGreen -- colour for multiple choice option B
    , optionColourC = darkGreen -- colour for multiple choice option C
    , optionColourD = darkGreen -- colour for multiple choice option D
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
                Question1 -> "Write the following product of cosines as a sum: 2cos(7x/2)cos(3x/2)"
                Question2 -> "Simplify the following: sin(4y)cos(2y)"
                Question3 -> "Write cos(3y)cos(5y) as a sum or difference."
                Question4 -> "Write the difference of sines expression as a product: sin(4y) - sin(2y)"
                Question5 -> "Prove that csc^2(y) - 2 = cos(2y) / sin^2(y)"


-- Takes a parameter of type Steps and returns the corresponding question as a string
stepStr step = case step of
                Step1 -> "What is the first step?"
                Step2 -> "What is the second step?"
                Step3 -> "What is the third step?"
                Step4 -> "What is the fourth step?"
                Step5 -> "What is the fifth step?"


-- Takes a parameter of type Questions and returns the corresponding steps as a list of strings
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
                Question5 -> [ "Step 1: RHS = (1 - 2sin^2(y)) / sin^2(y)",
                               "Step 2: RHS = 1/sin^2(y) - 2sin^2(y) / sin^2(y)",
                               "Step 3: RHS = csc^2(y) - 2"
                             ]


-- This function creates the partial solution that is displayed above the multiple choice options
-- Returns a Shape
solutionText step lst = group (List.indexedMap (\idx line -> text line
                                                            |> size 12
                                                            |> filled darkGreen
                                                            |> move ( -130, 75-20*(Basics.toFloat idx)) ) (List.take (getIndexFromStep step) lst))


-- Calls the solutionText function and passes in the current step and a list of all steps
solutionSection question step = solutionText step (solutionStr question)


-- Takes two parameters of type Questions and Steps and returns the corresponding list of multiple options
-- Each multiple choice option is a tuple containing a string with option that will be displayed to the screen
-- and an indication of whether or not the option is correct or incorrect
optionsStr question step = case (question, step) of
                (Question1, Step1) -> [ ( "a) = (2*(1/2)) [ cos(8x/2 - 3x/2) + cos(7x/2 + 3x/2)]", Incorrect)
                                      , ( "b) = (2*(1/2)) [ cos(9x/2 - 3x/2) + cos(6x/2 + x/2)]", Incorrect)
                                      , ( "c) = (2*(1/2)) [ cos(7x/2 - 4x/2) + cos(7x/2 + 3x/2)]", Incorrect)
                                      , ( "d) = (2*(1/2)) [ cos(7x/2 - 3x/2) + cos(7x/2 + 3x/2)]", Correct)
                                      ]
                (Question1, Step2) -> [ ( "a) = [cos(3x/2) + cos(10x/2)]", Incorrect)
                                      , ( "b) = [cos(8x/2) + cos(10x/2)]", Incorrect)
                                      , ( "c) = [cos(4x/2) + cos(10x/2)]", Correct)
                                      , ( "d) = [cos(7x/2) + cos(10x/2)]", Incorrect)
                                      ]
                (Question1, Step3) -> [ ( "a) = cos2x + cos5x", Correct)
                                      , ( "b) = cos3x + cos7x", Incorrect)
                                      , ( "c) = cos5x + cos5x", Incorrect)
                                      , ( "d) = cos2x + cos6x", Incorrect)
                                      ]
                (Question2, Step1) -> [ ( "a) = 1/2 [sin(5y + 2y) + sin(4y - 2y)]", Incorrect)
                                      , ( "b) = 1/2 [sin(4y + 2y) + sin(4y - 2y)]", Correct)
                                      , ( "c) = 1/2 [sin(5y + 2y) + sin(2y - 2y)]", Incorrect)
                                      , ( "d) = 1/2 [sin(4y + 2y) + sin(2y - 2y)]", Incorrect)
                                      ]
                (Question2, Step2) -> [ ( "a) = 1/2 [sin(4y) + sin(2y)]", Incorrect)
                                      , ( "b) = 1/2 [sin(2y) + sin(2y)]", Incorrect)
                                      , ( "c) = 1/2 [sin(6y) + sin(2y)]", Correct)
                                      , ( "d) = 1/2 [sin(5y) + sin(2y)]", Incorrect)
                                      ]
                (Question3, Step1) -> [ ( "a) = 1/2 [ cos(5y - 5y) + cos(3y + 5y)]", Incorrect)
                                      , ( "b) = 1/2 [ cos(3y - 5y) + cos(3y + 5y)]", Correct)
                                      , ( "c) = 1/2 [ cos(9y - 5y) + cos(3y + 5y)]", Incorrect)
                                      , ( "d) = 1/2 [ cos(2y - 5y) + cos(8y + 5y)]", Incorrect)
                                      ]
                (Question3, Step2) -> [ ( "a) = 1/3 [cos(2y) + cos(8y)]", Incorrect)
                                      , ( "b) = 1/3 [cos(8y) + cos(8y)]", Incorrect)
                                      , ( "c) = 1/2 [cos(2y) + cos(8y)]", Correct)
                                      , ( "d) = 1/2 [cos(8y) + cos(8y)]", Incorrect)
                                      ]
                (Question4, Step1) -> [ ( "a) = 6sin((4y - 2y) / 2) cos((4y + 2y) / 2)", Incorrect)
                                      , ( "b) = 6sin((4y - 2y) / 2) cos((7y + 2y) / 2)", Incorrect)
                                      , ( "c) = 2sin((4y - 2y) / 2) cos((4y + 2y) / 2)", Correct)
                                      , ( "d) = 2sin((4y - 2y) / 2) cos((7y + 2y) / 2)", Incorrect)
                                      ]
                (Question4, Step2) -> [ ( "a) = 2sin(2y/2)cos(6y/2)", Correct)
                                      , ( "b) = sin(2y/3)cos(6y/2)", Incorrect)
                                      , ( "c) = 2sin(2y/3)cos(6y/2)", Incorrect)
                                      , ( "d) = 2sin(2y/2)cos(8y/2)", Incorrect)
                                      ]
                (Question4, Step3) -> [ ( "a) = 6sinycos3y", Incorrect)
                                      , ( "b) = 2sinycos7y", Incorrect)
                                      , ( "c) = 2sinycos3y", Correct)
                                      , ( "d) = 2sinycos7y", Incorrect)
                                      ]
                (Question5, Step1) -> [ ( "a) RHS = (1 - 3sin^3(y)) / sin^2(y)", Incorrect)
                                      , ( "b) RHS = (1 - 2sin^2(y)) / sin^2(2y)", Incorrect)
                                      , ( "c) RHS = (1 - 3sin^2(y)) / sin^2(2y)", Incorrect)
                                      , ( "d) RHS = (1 - 2sin^2(y)) / sin^2(y)", Correct)
                                      ]
                (Question5, Step2) -> [ ( "a) RHS = 1/sin^2(y) - 2sin^2(y) / sin^2(y)", Correct)
                                      , ( "b) RHS = 2/sin^2(y) - 2sin^2(y) / sin^2(y)", Incorrect)
                                      , ( "c) RHS = 2/sin^2(y) - 1sin^2(y) / sin^2(y)", Incorrect)
                                      , ( "d) RHS = 1/sin^2(y) - 1sin^2(y) / sin^2(y)", Incorrect)
                                      ]
                (Question5, Step3) -> [ ( "a) RHS = csc^2(y) - 2", Correct)
                                      , ( "b) RHS = cot^2(y) - 2", Incorrect)
                                      , ( "c) RHS = csc^2(y) - 1", Incorrect)
                                      , ( "d) RHS = cot^2(y) - 1", Incorrect)
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
                                                                                                                    |> notifyEnter (updateOptionColourOnHover idx lightGreen optionSelected)
                                                                                                                    |> notifyLeave (updateOptionColourOnHover idx darkGreen optionSelected)
                                                                                                                    |> notifyTap (updateOptionColourOnClick idx darkGreen lightGreen)
                                                                                                                    |> notifyTap (ClickedOption idx (Tuple.second tuple)) ) lst)


-- Creates the entire multiple choice section including the question
optionsSection question step optionSelected optionColourA optionColourB optionColourC optionColourD = group [ text (stepStr step)
                                                                                                    |> size 12
                                                                                                    |> filled darkGreen
                                                                                                    |> move ( -130, 65 )
                                                                                                , optionsText (optionsStr question step) optionSelected optionColourA optionColourB optionColourC optionColourD
                                                                                                ] |> move ( 0, -20*(Basics.toFloat(getIndexFromStep step)) )


-- Helper function
-- Takes two parameters of type Questions and Steps and returns true if the step is the last step for that question
isLastStep question step = case (question, step) of
                (Question1, Step3) -> True
                (Question2, Step2) -> True
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
                                            else darkGreen
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
                                    |> filled darkGreen
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
                                    |> filled darkGreen
                                    |> move ( -100, -60 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , text "Next"
                                    |> filled white
                                    |> move ( -112, -63 - 20*(Basics.toFloat(getIndexFromStep step)) )
                                    |> notifyTap NextStep
                                , rectangle 90 25
                                    |> filled darkGreen
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
                (Question1, Step1) -> ["Try using the", "product of", "cosines" ]
                (Question1, Step2) -> ["Perform", "simple subtractions" ]
                (Question1, Step3) -> ["Simplify" ]
                (Question2, Step1) -> ["Use the", "product", "of sines" ]
                (Question2, Step2) -> ["Simplify"]
                (Question3, Step1) -> ["Use the", "product", "of cosines" ]
                (Question3, Step2) -> ["Simplify"]
                (Question4, Step1) -> ["Use the", "difference", "of sines" ]
                (Question4, Step2) -> ["Perform", "simple additions", "and", "subtractions"]
                (Question4, Step3) -> ["Simplify"]
                (Question5, Step1) -> ["Use a", "double", "angle", "formula" ]
                (Question5, Step2) -> ["Can you try", "splitting", "the terms?" ]
                (Question5, Step3) -> ["Use a", "reciprocal", "formula"]
                otherwise -> []


-- Takes two parameters of type Questions and Steps and returns the corresponding explanation
explanationStr question step option = case (question, step) of
                (Question1, Step1) -> case option of
                                        0 -> ["We cannot derive", "cos(8x/2)" ]
                                        1 -> ["We cannot derive", "cos(9x/2)" ]
                                        2 -> ["We cannot derive", "cos(7x/2-4x/2)" ]
                                        3 -> ["We can derive this equation using", "the product of cosines.", "cos(a)cos(b) = (1/2)*[cos(a-b)+cos(a+b)]", "by substituting a=7x/2 and b = 3x/2", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question1, Step2) -> case option of
                                        0 -> ["We cannot derive", "cos(3x/2)" ]
                                        1 -> ["We cannot derive", "cos(8x/2)" ]
                                        2 -> ["We can derive this equation since", "7x/2 - 3x/2 = 4x/2 and", "7x/2 + 3x/2 = 10x/2.", "Therefore, this option is correct"]
                                        3 -> ["We cannot derive", "cos(7x/2)" ]
                                        otherwise -> []
                (Question1, Step3) -> case option of
                                        0 -> ["We can derive this equation since", "4/2 = 2 and 10/2 = 5.", "Therefore, this option is correct"]
                                        1 -> ["We cannot derive", "cos(3x) + cos(7x)" ]
                                        2 -> ["We cannot derive", "cos(5x) + cos(5x)" ]
                                        3 -> ["We cannot derive", "cos(2x) + cos(6x)" ]
                                        otherwise -> []
                (Question2, Step1) -> case option of
                                        0 -> ["We cannot derive", "sin(5y + 2y)" ]
                                        1 -> ["We can derive this equation using", "the product of sine and cosine.", "sin(a)cos(b) = (1/2)*[sin(a+b)+sin(a-b)]", "by substituting a = 4y and b=2y", "Therefore, this option is correct"]
                                        2 -> ["We cannot derive", "sin(5y + 2y)" ]
                                        3 -> ["We cannot derive", "sin(2y - 2y)"]
                                        otherwise -> []
                (Question2, Step2) -> case option of
                                        0 -> ["We cannot derive", "sin(4y + 2y)" ]
                                        1 -> ["We cannot derive", "sin(2y + 2y)" ]
                                        2 -> ["We can derive this equation since", "4y+2y = 6y and", "4y-2y = 2y", "Therefore, this option is correct"]
                                        3 -> ["We cannot derive", "sin(5y - 2y)"]
                                        otherwise -> []
                (Question3, Step1) -> case option of
                                        0 -> ["We cannot derive", "cos(5y - 5y)" ]
                                        1 -> ["We can derive this equation using", "the product of cosines,", "cos(a)cos(b) = (1/2)*[cos(a-b) + cos(a+b)]", "by substituting a = 3y and b = 5y", "Therefore, this option is correct"]
                                        2 -> ["We cannot derive", "cos(9y - 5y)" ]
                                        3 -> ["We cannot derive", "cos(2y - 5y)" ]
                                        otherwise -> []
                (Question3, Step2) -> case option of
                                        0 -> ["We cannot derive", "1/3[cos(2y + 8y)]" ]
                                        1 -> ["We cannot derive", "1/3[cos(8y + 8y)]" ]
                                        2 -> ["3y - 5y = 2y and ", "3y + 5y = 8y", "Therefore, this option is correct"]
                                        3 -> ["We cannot derive", "1/3[cos(8y + 8y)]" ]
                                        otherwise -> []
                (Question4, Step1) -> case option of
                                        0 -> ["We cannot derive", "the 6 coefficient" ]
                                        1 -> ["We cannot derive", "the 6 coefficient"]
                                        2 -> ["We can derive this equation using", "the difference of sines","sin(a)-sin(b) = 2sin((a-b)/2)cos((a+b)/2)", "by substituting a = 4y and b = 2y", "Therefore, this option is correct"]
                                        3 -> ["The 2 cofficient is correct", "however we cannot derive cos(4y+2y)" ]
                                        otherwise -> []
                (Question4, Step2) -> case option of
                                        0 -> ["Yes this is correct. 4-2=2", "and 4+2=6", "Therefore, this option is correct"]
                                        1 -> ["This is not correct.", "Try to perform simple arithmetics." ]
                                        2 -> ["This is not correct.", "Try to perform simple arithmetics." ]
                                        3 -> ["This is not correct.", "Try to perform simple arithmetics."  ]
                                        otherwise -> []
                (Question4, Step3) -> case option of
                                        0 -> ["This is not correct.", "Try to perform simple arithmetics." ]
                                        1 -> ["This is not correct.", "Try to perform simple arithmetics." ]
                                        2 -> ["2y/2 = y", "and 6y/2 = 3y", "Therefore, this option is correct" ]
                                        3 -> ["This is not correct.", "Try to perform simple arithmetics."  ]
                                        otherwise -> []
                (Question5, Step1) -> case option of
                                        0 -> ["We cannot derive this.", "Try to apply a double angle formula" ]
                                        1 -> ["We cannot derive this.", "Try to apply a double angle formula" ]
                                        2 -> ["We cannot derive this.", "Try to apply a double angle formula" ]
                                        3 -> ["Yes, we can derive this since", "cos2y = 1 - 2sin^2(y).", "Therefore, this option is correct"]
                                        otherwise -> []
                (Question5, Step2) -> case option of
                                        0 -> ["We can derive this by", "expanding the equation through", "applying 1/sin^2(y) to each part", "Therefore, this option is correct"]
                                        1 -> ["We cannot derive this.", "How can you rearrange the equation?" ]
                                        2 -> ["We cannot derive this.", "How can you rearrange the equation?" ]
                                        3 -> ["We cannot derive this.", "How can you rearrange the equation?" ]
                                        otherwise -> []
                (Question5, Step3) -> case option of
                                        0 -> ["We can derive this since the sin^2(y)", "is cancelled out and ", "1/sin^2(y) = csc^2(y)", "Therefore, this option is correct"]
                                        1 -> ["We cannot derive this.", "Try to apply a reciprocal formula" ]
                                        2 -> ["We cannot derive this.", "Try to apply a reciprocal formula" ]
                                        3 -> ["We cannot derive this.", "Try to apply a reciprocal formula" ]
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
hintCard question step = group [ rect 130 130 |> filled darkGrey |> makeTransparent 0.5 |> move (5, -5)
                               , rect 130 130 |> filled white |> addOutline (solid 0.3) black
                               , text "Hint!" |> size 16 |> filled darkGreen |> move (-55, 45)
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
    [ rectangle 400 312 |> filled blank |> addOutline (solid 2) darkGreen |> move ( 55, 0 )
    , text (questionTitleStr model.question) |> size 16 |> bold |> filled darkGreen |> move ( 20, 120 )
    , triangle 8|> filled darkGreen |> move ( 150, 125 ) |> notifyTap NextQuestion
    , triangle 8|> filled darkGreen |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap PreviousQuestion
    , text (questionStr model.question) |> size 12 |> bold |> filled darkGreen |> move ( -130, 95 )
    , resultsSection model.question model.step model.answerState model.option
    ]
    ++ [solutionSection model.question model.step]
    ++ [optionsSection model.question model.step model.optionSelected model.optionColourA model.optionColourB model.optionColourC model.optionColourD]
    ++ [ group
            [ circle 12 |> filled blank |> addOutline (solid 3) darkGreen |> makeTransparent 0.75 |> move ( 234, 125 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint
            , text "?" |> bold |> sansserif |> size 20 |> filled darkGreen |> makeTransparent 0.75 |> move ( 228, 118 ) |> notifyEnter (ClickedHint model.question model.step) |> notifyLeave ExitHint ]
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
                , optionColourA = darkGreen
                , optionColourB = darkGreen
                , optionColourC = darkGreen
                , optionColourD = darkGreen
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
                , optionColourA = darkGreen
                , optionColourB = darkGreen
                , optionColourC = darkGreen
                , optionColourD = darkGreen
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






















































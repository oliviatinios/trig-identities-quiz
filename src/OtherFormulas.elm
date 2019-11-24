module OtherFormulas exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import String exposing (..)



init =
    { time = 0
    , notify = NotifyTap
    , answer = Default
    , step = Step1
    , question = Question1
    }

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

        GoToStep2
            -> { model
                | answer = Default
                , step = Step2
                }

        GoToStep3
            -> { model
                | answer = Default
                , step = Step3
                }

        GoToStep4
            -> { model
                | answer = Default
                , step = Step4
                }

        GoToStep5
            -> { model
                | answer = Default
                , step = Step5
                }

        GoToQuestion1
            -> { model
                | answer = Default
                , step = Step1
                , question = Question1
                }

        GoToQuestion2
            -> { model
                | answer = Default
                , step = Step1
                , question = Question2
                }



--         -- ran out of room for notifications, but left them here for a possible future improvement
--         Notif notif ->
--             { model | notify = notif }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads

view model =
    (case model.question of
        Question1 ->
            (case model.step of
                Step1 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 1" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> move ( 150, 125 ) |> notifyTap GoToQuestion2
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q1" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]

                Step2 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 1" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> move ( 150, 125 ) |> notifyTap GoToQuestion2
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q1" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]

                Step3 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 1" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> move ( 150, 125 ) |> notifyTap GoToQuestion2
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q1" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]
                Step4 -> []
                Step5 -> []
            )

        Question2 ->
            (case model.step of
                Step1 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q2" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]

                Step2 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q2" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]

                Step3 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q2" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]
                Step4 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q2" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]
                Step5 ->
                    [ rectangle 400 290 |> filled blank |> addOutline (solid 2) darkRed |> move ( 55, 0 )
                    , text "Question 2" |> size 16 |> bold |> filled darkRed |> move ( 20, 120 )
                    , triangle 8|> filled darkRed |> rotate (degrees -60) |> move ( -50, 125 ) |> notifyTap GoToQuestion1
                    , text "Other formulas Q2" |> size 12 |> bold |> filled darkRed |> move ( -130, 95 )
                    ]
            )
    )
-- messages generated by the framework (Tick) and by user interactions
-- note that we let elm figure out the type of the model by making it a type parameter, m


type Msg m
    = Tick Float GetKeyState
    | Notif Notifications
    | WrongAnswer
    | RightAnswer
    | GoToStep2
    | GoToStep3
    | GoToStep4
    | GoToStep5
    | GoToQuestion1
    | GoToQuestion2


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




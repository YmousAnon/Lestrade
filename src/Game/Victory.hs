module Game.Victory
(
    SolutionState(UnSolved,Correct,Wrong),
    (-|-),

    Solvable,
    (|-|),
) where


    data SolutionState = UnSolved | Wrong | Correct
        deriving (Show)

    (-|-) :: SolutionState -> SolutionState -> SolutionState
    UnSolved -|- _        = UnSolved
    _        -|- UnSolved = UnSolved
    Wrong    -|- _        = Wrong
    _        -|- Wrong    = Wrong
    Correct  -|- Correct  = Correct



    class Solvable a where
        (|-|) :: a -> a -> SolutionState



    data Correct = Grid
                   -- { tiles ::
                   -- ,
                   -- }

    --Correct
                -- { board    :: Board
                -- , solution :: Board
                -- , vhb      :: HintBoard
                -- , hhb      :: HintBoard
                -- , area     :: Area
                -- }

    --let

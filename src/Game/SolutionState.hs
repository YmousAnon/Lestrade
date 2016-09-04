module Game.SolutionState
(
    SolutionState(UnSolved,Correct,Wrong),
    (-|-),

    Solvable,
    (|-|),
) where

    import Control.Monad

    import Game.Board.Value


    data SolutionState = UnSolved | Wrong | Correct
        deriving Eq

    (-|-) :: SolutionState -> SolutionState -> SolutionState
    UnSolved -|- _        = UnSolved
    _        -|- UnSolved = UnSolved
    Wrong    -|- _        = Wrong
    _        -|- Wrong    = Wrong
    Correct  -|- Correct  = Correct



    class Solvable a where
        (|-|) :: a -> a -> SolutionState

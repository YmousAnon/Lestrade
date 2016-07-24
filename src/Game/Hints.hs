module Game.Hints
(
    genHint,
) where

    import Control.Monad.Trans.State

    import Game.Board
    import Game.Hints.Vertical

    import System.Random


    genHint :: Board -> State ([(Int,Int)],StdGen) (IO VHint)
    genHint s = do (rcs,g ) <- get
                   let (rci,g' ) = randomR (0,length rcs-1) g
                       (h  ,g'') = runState (genVHint (rcs !! rci) s) g'
                   put (rcs,g'')
                   return $ h

        where
            order :: Int -> State StdGen [Int]
            order n = do i  <- state $ randomR (0,n-1)
                         is <- order (n-1)
                         return (i:is)

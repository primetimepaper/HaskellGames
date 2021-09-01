module Part3 where
import Part1 
import Part2

{-
Initially all of a player's pieces are at the `Start` position,
waiting to enter the board.  The first move is by the red player.
-}
-- ! All 14 pieces are at the Start position
emptyPlacing :: Placing
emptyPlacing = ([Start, Start, Start, Start, Start, Start, Start], [Start, Start, Start, Start, Start, Start, Start])

initGS :: GameState
initGS = GameState emptyPlacing Red

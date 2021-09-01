module Part7 where
import Part1
import Part2
import Part3
import Part4
import Part5
import Part6

{-
Given a list of dice roll/position pairs representing moves, implement
a function, `winner`, that returns the winner, if there is one, of
that sequence of moves.
-}

winnerTest :: Bool
winnerTest = 
  (winner [(4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (1, Start), (4, OnBoard Sq12), (0, OnBoard Sq_1)] == Nothing) &&
  (winner ( take 42 (cycle  [(4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (1, Start), (4, OnBoard Sq12), (0, OnBoard Sq_1)])) == Nothing) && -- ! returns Just Red
  (winner ( take 42 (cycle  [(4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (0, Start), (4, OnBoard Sq12), (0, Start)])) == Just Red) &&
  (winner  [(0, Start), (4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (0, Start), (4, OnBoard Sq12)] == Nothing) &&
  (winner ( take 42 (cycle  [(0, Start), (4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (0, Start), (4, OnBoard Sq12)])) == Just Green)

-- ! Assuming Red goes first, then Green, then Red, and so on
winner :: [(Int, Position)] -> Maybe Player
winner = winner' initGS
winner' :: GameState -> [(Int, Position)] -> Maybe Player
winner' gs [] = gameOver gs
winner' gs@(GameState place play) (x:xs) = if (length . filter (== Home)) (fst place) /= 7 && (length . filter (== Home)) (snd place) /= 7 then winner' (move gs x) xs else gameOver gs

{-
If you wish, you can load the file `PlayRGU.hs` into `ghci`.  It
imports your file, `Formative2.hs` and provides a function `play` that
allows you to have an interactive match.  You must provide your own dice.

**WARNING** the interface does no error checking, and will crash or
  otherwise behave badly, if you enter an unexpected value.
-}

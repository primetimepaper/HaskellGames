module Part5 where
import Part1
import Part2
import Part3
import Part4

{-
1. If there are valid moves with the current dice roll:
  1. The current player chooses one.
  2. The player's token is moved from the chosen position to the new position.
  3. If the new position is a shared square, and it is occupied by the
     other player then the other player's piece returns to the start.
  4. If the new position is "beyond" `Home`, it is taken to be
     `Home`.  (For example, a piece on Square 13 may be moved to home
     with a roll of 2, 3 or 4.)
  5. The next player is the opponent, unless the new position is a
     rosette, in which case the current player has another roll.
2. If the dice roll has no valid moves, the next player is the opponent.


The function which, given a game state and a dice
roll/position pair, returns the new game state.
-}
move :: GameState -> (Int, Position) -> GameState
move gs@(GameState place Red) (i, pos) = if movable place Red pos i 
   then if pos `elem` updatePlacing (fst place) pos i && pos `elem` snd place && pos `notElem` [OnBoard Sq_1,OnBoard Sq_2,OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq13,OnBoard Sq14]
      then GameState (updatePlacing (fst place) pos i, updatePlacing (snd place) pos (-1)) (if rosette (move' pos i) then Red else Green)
      else GameState (updatePlacing (fst place) pos i, snd place) (if rosette (move' pos i) then Red else Green)
   else GameState place Green
move gs@(GameState place Green) (i, pos) = if movable place Green pos i 
   then if pos `elem` fst place && pos `elem` updatePlacing (snd place) pos i  && pos `notElem` [OnBoard Sq_1,OnBoard Sq_2,OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq13,OnBoard Sq14]
      then GameState (updatePlacing (fst place) pos (-1), updatePlacing (snd place) pos i) (if rosette (move' pos i) then Green else Red)
      else GameState (fst place, updatePlacing (snd place) pos i) (if rosette (move' pos i) then Green else Red)
   else GameState place Red

rosette :: Position -> Bool 
rosette pos = pos `elem` [OnBoard Sq_4, OnBoard Sq_8, OnBoard Sq14]
updatePlacing :: [Position] -> Position -> Int -> [Position]
updatePlacing [] _ _ = []
updatePlacing x@(x1:xs) pos i | x1 == pos && i == -1 = Start : xs
                              | x1 == pos && pos == OnBoard Sq14 = Home : xs
                              | x1 == pos = move' pos i : xs
                              | otherwise = x1 : updatePlacing xs pos i
module Part6 where
import Part1
import Part2
import Part3 
import Part4
import Part5

{-
The game is over when one player gets all its tokens to "home".

We will model this by a function that, given a game state, returns
* `Nothing` when neither player has won, and
* `Just p` when player `p` has won (note: draws are not possible).
-}

gameOverTest :: Bool
gameOverTest = 
  (gameOver initGS == Nothing) &&
  (gameOver allRedHomeGS == Just Red) &&
  (gameOver allGreenHomeGS == Just Green) &&
  (gameOver allOnBoardGS == Nothing)


gameOver :: GameState -> Maybe Player
gameOver (GameState place play) | (length . filter (== Home)) (fst place) == 7 = Just Red
                                | (length . filter (== Home)) (snd place) == 7 = Just Green 
                                | otherwise = Nothing

{-
For the purposes of `gameOverTest`, you will have to define `allGreenHomeGS :: GameState` to reflect all player `Red` pieces (tokens) at `Home` and `allRedHomeGS :: GameState`
when all player `Green` pieces are at `Home`.
-}
-- ! POTENTIAL PROBLEM AREA FOR PART7
allRedHomeGS, allGreenHomeGS :: GameState 
allRedHomeGS = GameState ([Home, Home, Home, Home, Home, Home, Home], [Home, Home, Home, Home, Home, Home, OnBoard Sq14]) Red
allGreenHomeGS = GameState ([Home, Home, Home, Home, Home, Home, OnBoard Sq14], [Home, Home, Home, Home, Home, Home, Home]) Green
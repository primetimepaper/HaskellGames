module Part3 where
{-
To insert an element into a list at a given position. If the given position is larger than the length of
the list then insertion should be at the position found by taking the
input modulo the number of insertion positions.

-}
insertModTest :: Bool
insertModTest =    insertMod    0 "hello" 'x' == "xhello"
                && insertMod    5 "hello" 'x' == "hellox"
                && insertMod    3 [0..4]  9   == [0, 1, 2, 9, 3, 4]
                && insertMod   24 "hello" 'x' == "xhello"
                && insertMod  131 "hello" 'x' == "hellox"
                && insertMod 1011 [0..4]  9   == [0, 1 ,2, 9, 3 ,4]


insertMod :: Int -> [a] -> a -> [a]
insertMod i x y = if i > length x then insertMod (i `mod` (1 + length x)) x y else take i x ++ [y] ++ drop i x
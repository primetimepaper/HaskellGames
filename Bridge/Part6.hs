module Part6 where
import Pack -- defines card, pack, and hand types and printing facilities for values of those types

{-
You may use your answers to Q2i, Q2ii and Q2v for `shuffleDeal`.
If you chose to do so, uncomment the following line
-}
import Part1(pack)
import Part2(psrn)
import Part5(shuffle)
{-
Implement a function, `deal`, to distribute a list as evenly as
possible into four ordered sublists.
-}
dealTest :: Bool
dealTest =    deal [0..9]         == ([0,4,8],[1,5,9],[2,6],[3,7])
           && deal "hello world!" == ("hor"," el","dlw","!lo")

insertOrd :: Ord a => a -> [a] -> [a]
insertOrd w = foldr f [w]
  where
    f x ys@(z:zs) | x > w     = z:x:zs
                  | otherwise = x:ys
      
deal :: Ord a => [a] -> ([a], [a], [a], [a])
deal x = handout x ([],[],[],[]) where
  handout [] hands = hands 
  handout (a:as) (one, two, three, four) | length four < length three = handout as (one, two, three, insertOrd a four)
                                         | length three < length two = handout as (one, two, insertOrd a three, four)
                                         | length two < length one = handout as (one, insertOrd a two, three, four)
                                         | otherwise = handout as (insertOrd a one, two, three, four)

{-
Using your function `deal`, implement a function, `shuffleDeal` that,
given a seed, and a next number function, shuffles and deals a full
pack of cards for a game of bridge.

When pretty printed using the function `pp` defined in module `Pack`,
your function should print:
```haskell
pp (shuffleDeal id 0) ~~>
N:  ♣5, ♣9, ♣K, ♦4, ♦8, ♦Q, ♥3, ♥7, ♥J, ♠2, ♠6, ♠10, ♠A
E:  ♣4, ♣8, ♣Q, ♦3, ♦7, ♦J, ♥2, ♥6, ♥10, ♥A, ♠5, ♠9, ♠K
S:  ♣3, ♣7, ♣J, ♦2, ♦6, ♦10, ♦A, ♥5, ♥9, ♥K, ♠4, ♠8, ♠Q
W:  ♣2, ♣6, ♣10, ♣A, ♦5, ♦9, ♦K, ♥4, ♥8, ♥Q, ♠3, ♠7, ♠J

pp (shuffleDeal psrn 1234567890) ~~>
N:  ♣3, ♣7, ♦4, ♦7, ♦8, ♦Q, ♦K, ♥8, ♥10, ♠3, ♠6, ♠9, ♠Q
E:  ♣4, ♣5, ♣6, ♣9, ♦6, ♦9, ♥2, ♥6, ♥Q, ♠8, ♠10, ♠J, ♠K
S:  ♣Q, ♣K, ♦2, ♦10, ♦J, ♦A, ♥3, ♥5, ♥J, ♥K, ♥A, ♠4, ♠A
W:  ♣2, ♣8, ♣10, ♣J, ♣A, ♦3, ♦5, ♥4, ♥7, ♥9, ♠2, ♠5, ♠7

pp (shuffleDeal psrn 2468013579) ~~>
N:  ♣4, ♦8, ♦10, ♦K, ♦A, ♥5, ♥8, ♥9, ♥J, ♠2, ♠4, ♠7, ♠8
E:  ♣2, ♣6, ♣J, ♣Q, ♦6, ♦J, ♥3, ♥6, ♥7, ♥A, ♠6, ♠10, ♠J
S:  ♣3, ♣5, ♣8, ♣9, ♣K, ♦2, ♦3, ♦4, ♦9, ♥2, ♥K, ♠3, ♠Q
W:  ♣7, ♣10, ♣A, ♦5, ♦7, ♦Q, ♥4, ♥10, ♥Q, ♠5, ♠9, ♠K, ♠A
```
-}
shuffleDeal  :: (Int -> Int)                -- function to update integer
                -> Int                      -- seed
                -> (Hand, Hand, Hand, Hand) -- result
shuffleDeal f n = deal (shuffle f n pack)
module Part7 where
import Pack
{-
A game of bridge is played either with
* one suit being designated the _trump_ suit, or
* no suit being designated the trump suit.

Give a suitable data type to represent the current trump suit, or that
there is no trump suit. Define the special value `noTrumpSuit::Trumps`
which represents there being no trump suit, and the function
`theTrumpSuit :: Suit -> Trumps`, so that `theTrumpSuit s` is a value
that represents `s` being the trump suit.
-}
noTrumpSuit :: Trumps
theTrumpSuit :: Suit -> Trumps

--type Trumps = () -- TREAT AS UNDEFINED TYPE; you may replace `type` by `newtype` or `data`

-- ! Cannot solve if Trumps is undefined. The parameter for trickWinner is simply Trumps, and not a function declaring the Trump from which we can extract the Suit
type Trumps = [Suit]
noTrumpSuit = []
theTrumpSuit s = [s]

{-
In each round one player leads, and then the other players follow the
lead, in rotation.  The following rules are used to decide the winner
of the round, or _trick_:
* if there is no trump suit, or there is a trump suit but no cards of
  that suit have been exposed, then the highest card of the same suit
  as the led card (the first card exposed) wins.
* if there is a trump suit, and cards of that suit have been exposed,
  then the highest card of the trump suit wins.
* The type `Card` is declared as deriving `Ord`, in a way such that
  the maximum card in a list matches the Bridge notion of highest card
  in a trick.

Implement a function, `trickWinner`, that takes:
* a trump suit, or no trumps,
* a led card, and
* a list of three following cards,

and determines the winning card of the four.

-}
trickWinnerTest :: Bool
trickWinnerTest =    twh noTrumpSuit             == Card Spades Ace
                  && twh (theTrumpSuit Diamonds) == Card Spades Ace
                  && twh (theTrumpSuit Hearts)   == Card Hearts Four
  where
    twh t = trickWinner t (Card Spades Two) [Card Hearts Four, Card Spades Ace, Card Spades King]

trickWinner :: Trumps -> Card -> [Card] -> Card
trickWinner t led@(Card s v) follow = last (isort (filter (isSuit s)[x | x <- led:follow]) ++ if null t then [] else isort (filter (isSuit (head t))[x | x <- led:follow]))

isSuit :: Suit -> Card -> Bool
isSuit s1 (Card s v) = s == s1

insert :: Ord a => a -> [a] -> [a]
insert x = insx
  where
    insx [] = [x]
    insx ys@(z:zs) | x <= z    = x : ys
                   | otherwise = z : insx zs
isort :: Ord a => [a] -> [a]
isort = foldr insert []
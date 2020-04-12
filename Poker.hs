--Seyon Rajagopal CPS506 Haskell Assignment
module Poker where

  import Data.List (sortBy)
  import Data.Char (ord)


  createDeck = [(value, suit) | suit <- ['C', 'D', 'H', 'S'], value <- [14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]]
  createDeck2 = [(value, suit) | suit <- ['C', 'D', 'H', 'S'], value <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]]
  
  -- sort the cards
  sortCards (c1, s1) (c2, s2)
    | c1 < c2 = LT
    | c1 > c2 = GT
    | c1 == c2 = compare s1 s2

  --function that checks hand by pattern matching
  evalHand ((x1, y1):(x2, y2):(x3, y3):(x4, y4):(x5, y5):rest) 
    | x1 == 10 && x2 == 11 && x3 == 12 && x4 ==13 && x5 == 14 && y1 ==y2 && y2 == y3 && y3 == y4 && y4 == y5 = (10,ord y1,0,0,0,0,0) -- Royal FLush
    | x1 == 2 && x2 == 3 && x3 == 4 && x4 == 5 && x5 == 14 && y1 == y2 && y2 == y3 && y3 == y4 && y4 == y5 = (9,5,ord y1,0,0,0,0) -- Straight Flush
    | x1+1 == x2 && x2+1 == x3 && x3+1 == x4 && x4+1 == x5 && y1 == y2 && y2 == y3 && y3 == y4 && y4 == y5 = (9,x5,ord y1,0,0,0,0)
    | x1 == x2 && x2 == x3 && x3 == x4 = (8,x1,x5,ord y4,0,0,0) -- Four of a kind
    | x2 == x3 && x3 == x4 && x4 == x5 = (8,x2,x1,ord y5,0,0,0)
    | x1 == x2 && x2 == x3 && x4 == x5 = (7,x1,x4,ord y3,0,0,0) -- Full House
    | x3 == x4 && x4 == x5 && x1 == x2 = (7,x5,x2,ord y5,0,0,0) 
    | y1 == y2 && y2 == y3 && y3 == y4 && y4 == y5 = (6,x5,x4,x3,x2,x1,ord y5) -- Flush
    | x1+1 == x2 && x2+1 == x3 && x3+1 == x4 && x4+1 == x5 = (5,x5,ord y5,0,0,0,0) -- Straight
    | x1 == 2 && x2 == 3 && x3 == 4 && x4 == 5 && x5 == 14 = (5,5,ord y5,0,0,0,0)
    | x1 == x2 && x2 == x3 = (4,x1,x4,x5,ord y3,0,0) -- Three of a kind
    | x2 == x3 && x3 == x4 = (4,x2,x5,x1,ord y4,0,0)
    | x3 == x4 && x4 == x5 = (4,x3,x2,x1,ord y5,0,0)
    | x1 == x2 && x3 == x4 = (3,x3,x1,x5,ord y4,0,0) -- Two Pair
    | x1 == x2 && x4 == x5 = (3,x4,x1,x3,ord y5,0,0)
    | x2 == x3 && x4 == x5 = (3,x4,x2,x1,ord y5,0,0)
    | x1 == x2 = (2,x1,x5,x4,x3,ord y2,0) -- Pair
    | x2 == x3 = (2,x2,x5,x4,x1,ord y3,0)
    | x3 == x4 = (2,x3,x5,x2,x1,ord y4,0)
    | x4 == x5 = (2,x4,x3,x2,x1,ord y5,0)
    | otherwise = (1,x5,x4,x3,x2,x1,ord y5) -- High Card

  --determine winner
  getwinner hand1 hand2 aceHand1 aceHand2= if(evalHand hand1 >= evalHand hand2) 
                      then aceHand1
                      else aceHand2

  deal cards =
        -- split hands into 2
    let hand1 = [snd x | x <- (zip [0..] cards), even(fst x)]
        hand2 = [snd x | x <- (zip [0..] cards), odd(fst x)]
        
        --convert and sort hands with 1 represented as 14 and put at end
        convertedHand1 = [createDeck!!((hand1!!0)-1)] ++ [createDeck!!((hand1!!1)-1)] ++ [createDeck!!((hand1!!2)-1)] ++ [createDeck!!((hand1!!3)-1)] ++ [createDeck!!((hand1!!4)-1)] 
        convertedHand2 = [createDeck!!((hand2!!0)-1)] ++ [createDeck!!((hand2!!1)-1)] ++ [createDeck!!((hand2!!2)-1)] ++ [createDeck!!((hand2!!3)-1)] ++ [createDeck!!((hand2!!4)-1)]
        sortedHand1 = sortBy sortCards convertedHand1
        sortedHand2 = sortBy sortCards convertedHand2

        --convert and sort hands with 1 at beginning
        aceConvertedHand1 = [createDeck2!!((hand1!!0)-1)] ++ [createDeck2!!((hand1!!1)-1)] ++ [createDeck2!!((hand1!!2)-1)] ++ [createDeck2!!((hand1!!3)-1)] ++ [createDeck2!!((hand1!!4)-1)] 
        aceConvertedHand2 = [createDeck2!!((hand2!!0)-1)] ++ [createDeck2!!((hand2!!1)-1)] ++ [createDeck2!!((hand2!!2)-1)] ++ [createDeck2!!((hand2!!3)-1)] ++ [createDeck2!!((hand2!!4)-1)]
        aceSortedHand1 = sortBy sortCards aceConvertedHand1
        aceSortedHand2 = sortBy sortCards aceConvertedHand2

        --determine winner and return array of strings
        winner = getwinner sortedHand1 sortedHand2 aceSortedHand1 aceSortedHand2
        winner1 = winner >>= \(x,y) -> [show(x)++y:[]]
        
    in (winner1)

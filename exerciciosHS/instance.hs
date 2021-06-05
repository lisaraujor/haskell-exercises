data Part = AM | PM
     deriving (Eq, Show)
data TIME = Local Int Int Part
          | Total Int Int

totalMinutos :: TIME -> Int
totalMinutos (Total _ b) = b
totalMinutos (Local _ b _) = b

instance Eq TIME where
   (==) (Local a b c) (Local d e f) = a == d && b == e && c == f
   (==) (Total a b) (Total d e) = a == d && b == e
   (==) (Local a b c) (Total d e) = (partToInt c a) == d && b == e
   (==) (Total a b) (Local d e f) = (partToInt f d) == a && b == e

partToInt :: Part -> Int -> Int
partToInt AM a = a
partToInt PM a = (12+a) `mod` 24

instance Show TIME where
    show (Local a b AM) = show a ++ ":" ++ show b ++ " am"
    show (Local a b PM) = show a ++ ":" ++ show b ++ " pm"
    show (Total a b) = show a ++ "h" ++ show b ++ "m"

instance Enum TIME where
	(succ) (Local 11 59 AM) = Local 12 00 PM
	(succ) (Local 11 59 PM) = Local 12 00 AM
	(succ) (Local 12 59 AM) = Local 1 00 AM
	(succ) (Local 12 59 PM) = Local 1 00 PM
	(succ) (Local h 59 p) = Local (h+1) 00 p
	(succ) (Local h m p) = Local h (m+1) p
	(succ) (Total 23 59) = Total 00 00
	(succ) (Total h 59) = Total (h+1) 00
	(succ) (Total h m) = Total h (m+1)
	(pred) (Local 1 00 p) = Local 12 59 p
	(pred) (Local 12 00 AM) = Local 11 59 PM
	(pred) (Local 12 00 PM) = Local 11 59 AM
	(pred) (Local h 00 p) = Local (h-1) 59 p
	(pred) (Local h m p) = Local h (m-1) p
	(pred) (Total 00 00) = Total 23 59
	(pred) (Total h 00) = Total (h-1) 59
	(pred) (Total h m) = Total h (m-1)
	{-(fromEnum) (Local h m PM) = ((h+12)*60) + m
    (fromEnum) (Local h m AM) = (h*60) + m
    (fromEnum) (Total h m) = (h*60) + m
    (toEnum) h = Total (h `div` 60) (h `mod` 60)-}

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq,Ord,Enum,Bounded)

data Rank = Numeric Int | Jack | Queen | King | Ace deriving(Eq,Ord)

data Card = Card {rank::Rank, suit::Suit} deriving(Eq, Ord)

{-instance Show Card where
	show (Card rank suit) =  show rank suit-}

instance Enum Rank where
   (succ) (Numeric 10) = Jack
   (succ) (Numeric a) = Numeric (succ a)
   (succ) Jack = Queen
   (succ) Queen = King
   (succ) King = Ace
   (succ) Ace = Ace
   (pred) (Numeric 2) = Numeric 2
   (pred) (Numeric a) = Numeric (pred a)
   (pred) Jack = Numeric 10
   (pred) Queen = Jack
   (pred) King = Queen
   (pred) Ace = King
   (fromEnum) (Numeric a) = a
   (fromEnum) Jack = 11
   (fromEnum) Queen = 12
   (fromEnum) King = 13
   (fromEnum) Ace = 14
   (toEnum) 11 = Jack
   (toEnum) 12 = Queen
   (toEnum) 13 = King
   (toEnum) 14 = Ace
   (toEnum) a = (Numeric a)

instance Enum Card where
   (succ) (Card Ace Clubs) = Card Ace Clubs
   (succ) (Card r Clubs) = Card (succ r) Spades
   (succ) (Card r s) = Card r (succ s)
   (pred) (Card (Numeric 2) Spades) = Card (Numeric 2) Spades

   (pred) (Card r Spades) = Card (pred r) Clubs
   (pred) (Card r s) = Card r (pred s)
   (fromEnum) (Card r s) = ((fromEnum r)-2)*4 + (fromEnum s)
   (toEnum) a = Card (toEnum ((div a 4)+2)) (toEnum (mod a 4))
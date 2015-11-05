module Main where

import Control.Applicative
import Control.Arrow
import Data.List (splitAt, find)
import Data.Monoid ((<>))
import System.Random(randomRIO)
import Data.Function (on, (&))

-- | Define the main data structure
data Suit = Spade | Heart | Club | Diamond deriving (Eq, Ord, Enum, Show)
data Pip  = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
data Card = Card
          { _suit :: Suit
          , _pip  :: Pip
          } deriving (Eq, Ord, Show)

-- | Make Card enumerable pour facilment construct
instance Enum Card where
  toEnum   = (`div` 13) &&& (`mod` 13) >>> toEnum *** toEnum >>> uncurry Card
  fromEnum = _suit &&& _pip >>> fromEnum *** fromEnum  >>> \(x, y) -> 13 * x + y

-- | Just make a shabby non-type-safe data structure considering the balence of use and cost
type Deck = [Card]
data EOBoard = EOBoard
             { _foundations :: [Deck]
             , _columns     :: [Deck]
             , _reserves    :: [Deck]
             } deriving (Eq, Ord, Show)

-- | Tricky method to get all the constructor of a enum
pack :: Deck
pack = Card <$> enumFrom Spade <*> enumFrom Ace

-- | Trival def, useless, but this can be odd when reach Ace or King
pCard, sCard :: Card -> Card
pCard = succ
sCard = pred

-- | Trival def for judge
isAce, isKing :: Card -> Bool
isAce  = _pip >>> (==) Ace
isKing = _pip >>> (==) King

-- | The un-rec single-step shuffle
moveRamdonElemtes :: ([a], [a]) -> IO ([a], [a])
moveRamdonElemtes (ss, as@(_ : _)) =
  takeN <$> randomRIO (0,length as - 1) <*> pure as
    where takeN n = splitAt n >>> (\(xs, (y : ys)) -> ((y : ss), xs <> ys))
moveRamdonElemtes (ss, []) = pure (ss, [])

-- | shuffle a deck by the global RandomGEN
shuffle :: [a] -> IO [a]
shuffle as = fst <$> shuffle' ([], as)
  where
    shuffle' (ss, as@(_ : _)) = moveRamdonElemtes (ss,as) >>= shuffle'
    shuffle' (ss, []        ) = pure (ss,[])

-- | split a list into given parts, generic splitAt
multiSplit :: [Int] -> [a] -> [[a]]
multiSplit (s:ss) = splitAt s >>> second (multiSplit ss) >>> uncurry (:)
multiSplit []     = pure

-- | Generate a EOBoard whose state is the start of the 8-off
eODeal :: IO EOBoard
eODeal =
  let packSplit = (multiSplit $ replicate 8 6 <> replicate 3 1) >>> splitAt 8 >>> uncurry (EOBoard $ replicate 4 [])
  in  packSplit <$> shuffle pack

-- | Detect if a deck in the fundations can be auto-added by the given deck in the columns
overlayDetect :: Deck -> Deck -> Bool
overlayDetect (c : cs) (xc:xcs) = if isKing c then False else (_suit c) == (_suit xc) && (succ $ _pip c) == (_pip xc)
overlayDetect []       (xc:xcs) = isAce xc
overlayDetect _        []       = False

-- | Replace the nth elem of the list with given one
replaceWith :: [a] -> Int -> a -> [a]
replaceWith xs i y = (take i xs) ++ (y : (drop (i+1) xs))

-- | a.k.a. autoplay, i.e.,automaticly doing the possible columns-to-foundations move
toFoundations :: EOBoard -> EOBoard
toFoundations e =
  let
    (ef,(ec,er)) = _foundations &&& _columns &&& _reserves $ e
    possibleMoves = curry (uncurry (overlayDetect `on` snd) &&& id) <$> zip [0..] ef <*> zip [0..] ec
  in
    case snd <$> find fst possibleMoves of
      Just ((i, d1), (j, (hd2 : d2))) -> toFoundations $ EOBoard (replaceWith ef i (hd2:d1)) (replaceWith ec j d2) er
      otherwise -> e

main :: IO ()
main =  toFoundations <$> eODeal >>= print

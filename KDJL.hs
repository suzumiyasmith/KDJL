module Main where
import Data.List (splitAt, find)
import System.Random(randomRIO)
import Data.Function (on)

-- | Define the main data structure
data Suit = Spade | Heart | Club | Diamond deriving (Eq, Ord, Enum, Show)
data Pip  = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
data Card = Card
          { _suit :: Suit
          , _pip  :: Pip
          } deriving (Eq, Ord, Show)

-- | Make Card enumerable pour facilment construct
instance Enum Card where
  toEnum  c = Card (toEnum (div c 13)) (toEnum (mod c 13))
  fromEnum c = (fromEnum (_suit c)) * 13 + (fromEnum (_pip c))

-- | Just make a shabby non-type-safe data structure considering the balence of use and cost
type Deck = [Card]
data EOBoard = EOBoard
             { _foundations :: [Deck]
             , _columns     :: [Deck]
             , _reserves    :: [Deck]
             } deriving (Eq, Ord, Show)

-- | Tricky method to get all the constructor of a enum
pack :: Deck
pack = [Card s p | s <- (enumFrom Spade) , p <- (enumFrom Ace)]

-- | Trival def, useless, but this can be odd when reach Ace or King
pCard, sCard :: Card -> Card
pCard = succ
sCard = pred

-- | Trival def for judge
isAce, isKing :: Card -> Bool
isAce  = (== Ace) . _pip
isKing = (== King) . _pip

-- | The un-rec single-step shuffle
moveRamdonElemtes :: ([a], [a]) -> IO ([a], [a])
moveRamdonElemtes (ss, as@(_ : _)) =
  fmap (\x -> takeN x as) $ randomRIO (0,length as - 1)
    where takeN n = (\(xs, (y : ys)) -> ((y : ss), xs ++ ys)) . (splitAt n)
moveRamdonElemtes (ss, []) = return (ss, [])

-- | shuffle a deck by the global RandomGEN
shuffle :: [a] -> IO [a]
shuffle as = fmap fst $ shuffle' ([], as)
  where
    shuffle' (ss, as@(_ : _)) = moveRamdonElemtes (ss,as) >>= shuffle'
    shuffle' (ss, []        ) = return (ss,[])

-- | split a list into given parts, generic splitAt
multiSplit :: [Int] -> [a] -> [[a]]
multiSplit (s:ss) as = (fst $ splitAt s as) : (multiSplit ss (snd $ splitAt s as))
multiSplit []  as   = [as]

-- | Generate a EOBoard whose state is the start of the 8-off
eODeal :: IO EOBoard
eODeal =
  let packSplit = (uncurry (EOBoard $ replicate 4 [])) . (splitAt 8) . (multiSplit $ replicate 8 6 ++ replicate 3 1) 
  in  fmap packSplit $ shuffle pack

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
    (ef, ec, er) = (_foundations e, _columns e, _reserves e)
    possibleMoves = [((overlayDetect `on` snd) x1 x2, (x1, x2)) | x1 <- (zip [0..] ef), x2 <- (zip [0..] ec)]
  in
    case fmap snd $ find fst possibleMoves of
      Just ((i, d1), (j, (hd2 : d2))) -> toFoundations $ EOBoard (replaceWith ef i (hd2:d1)) (replaceWith ec j d2) er
      otherwise -> e

-- | just for simple testing
main :: IO ()
main = (fmap toFoundations eODeal) >>= print

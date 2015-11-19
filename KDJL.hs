module Main where
import Data.List (splitAt, find)
import System.Random(randomRIO)
import Data.Function (on)

-- | Define the main data structure
-- 下面这三个data定义了数据结构：Suit 花色 ； Pip 数字 ； Card 卡片
data Suit = Spade | Heart | Club | Diamond deriving (Eq, Ord, Enum, Show)
data Pip  = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
data Card = Card
          { _suit :: Suit
          , _pip  :: Pip
          } deriving (Eq, Ord, Show)

-- | Make Card enumerable pour facilment construct
-- 下面这个instance实现了Card类型的Enum(可数)类型类（就是ＯＯＰ里的接口或是抽象类）的实例
instance Enum Card where
  toEnum  c = Card (toEnum (div c 13)) (toEnum (mod c 13))
  fromEnum c = (fromEnum (_suit c)) * 13 + (fromEnum (_pip c))

-- | Just make a shabby non-type-safe data structure considering the balence of use and cost
-- 下面这ｄａｔａ是Ｄｅｃｋ（卡组）的定义，就是简单定义成卡片的列表
type Deck = [Card]
-- 定义ＥＯＢｏａｒｄ、即８－ｏｆｆ　Ｂｏａｒｄ、空当接龙整体面板、由ｆｏｕｎｄａｔｉｏｎ、ｃｏｌｕｍｎ、ｒｅｓｅｒｖｅ三部分组成
data EOBoard = EOBoard
             { _foundations :: [Deck]
             , _columns     :: [Deck]
             , _reserves    :: [Deck]
             } deriving (Eq, Ord, Show)

-- | Tricky method to get all the constructor of a enum
--　利用ｃａｒｄ实现的Ｅｎｕｍ类型类的函数定义一套完整卡组（仅除大小王、共５２张）
pack :: Deck
pack = [Card s p | s <- (enumFrom Spade) , p <- (enumFrom Ace)]

-- | Trival def, useless, but this can be odd when reach Ace or King
-- 两个函数、分别可以给出一张卡的下一张卡、或是其前一张牌、ｐ代表　ｐｒｅｖｉｏｕｓ、前、ｓ代表ｓｕｃｃｅｓｓ、后继
pCard, sCard :: Card -> Card
pCard = succ
sCard = pred

-- | Trival def for judge
-- 分别判断一张牌是否是　幺尖Ａ　、是否是　老凯Ｋ
isAce, isKing :: Card -> Bool
isAce  = (== Ace) . _pip
isKing = (== King) . _pip

-- | The un-rec single-step shuffle
-- 接受两个列表、然后随机将后者的一张卡放进前者中
moveRamdonElemtes :: ([a], [a]) -> IO ([a], [a])
moveRamdonElemtes (ss, as@(_ : _)) =
  fmap (\x -> takeN x as) $ randomRIO (0,length as - 1)
    where takeN n = (\(xs, (y : ys)) -> ((y : ss), xs ++ ys)) . (splitAt n)
moveRamdonElemtes (ss, []) = return (ss, [])

-- | shuffle a deck by the global RandomGEN
-- 利用刚才定义的那个函数、进行洗牌、其实就是把一个牌组里牌随机抽出后依顺序组成一个新的牌组
shuffle :: [a] -> IO [a]
shuffle as = fmap fst $ shuffle' ([], as)
  where
    shuffle' (ss, as@(_ : _)) = moveRamdonElemtes (ss,as) >>= shuffle'
    shuffle' (ss, []        ) = return (ss,[])

-- | split a list into given parts, generic splitAt
-- 将一个数组按照由第一个参数给定的格式进行分割
multiSplit :: [Int] -> [a] -> [[a]]
multiSplit (s:ss) as = (fst $ splitAt s as) : (multiSplit ss (snd $ splitAt s as))
multiSplit []  as   = [as]

-- | Generate a EOBoard whose state is the start of the 8-off
--　随机产生一个符合要求ＥＯＢｏａｒｄ
eODeal :: IO EOBoard
eODeal =
  let packSplit = (uncurry (EOBoard $ replicate 4 [])) . (splitAt 8) . (multiSplit $ replicate 8 6 ++ replicate 3 1) 
  in  fmap packSplit $ shuffle pack

-- | Detect if a deck in the fundations can be auto-added by the given deck in the columns
-- 检测两卡组间是否可以有ａｕｔｏｐｌａｙ的可能
overlayDetect :: Deck -> Deck -> Bool
overlayDetect (c : cs) (xc:xcs) = if isKing c then False else (_suit c) == (_suit xc) && (succ $ _pip c) == (_pip xc)
overlayDetect []       (xc:xcs) = isAce xc
overlayDetect _        []       = False

-- | Replace the nth elem of the list with given one
-- 将一个列表的第ｎ个位置替换成所给的元素
replaceWith :: [a] -> Int -> a -> [a]
replaceWith xs i y = (take i xs) ++ (y : (drop (i+1) xs))

-- | a.k.a. autoplay, i.e.,automaticly doing the possible columns-to-foundations move
-- 即ａｕｔｏｐｌａｙ、自动进行所有可能的移动、即自动将符合条件的ｃｏｌｕｎｍ里的卡移动到ｆｏｕｄａｔｉｏｎ里、之后若还有可以移动的、便再移动
toFoundations :: EOBoard -> EOBoard
toFoundations e =
  let
    (ef, ec, er) = (_foundations e, _columns e, _reserves e)
    possibleMoves = [((overlayDetect `on` snd) x1 x2, (x1, x2)) | x1 <- (zip [0..] ef), x2 <- (zip [0..] ec)]
  in
    case fmap snd $ find fst possibleMoves of
      Just ((i, d1), (j, (hd2 : d2))) -> toFoundations $ EOBoard (replaceWith ef i (hd2:d1)) (replaceWith ec j d2) er
      otherwise -> e
-- 主函数、同时也是一个小测试、将一个随机生成的ＥＯＢｏａｒｄ进行ａｕｔｏｐｌａｙ后输出到终端
-- | just for simple testing
main :: IO ()
main = (fmap toFoundations eODeal) >>= print

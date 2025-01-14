module Y2024.Day21
  ( tasks
  ) where

import           Data.Monoid

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Text   as Text

import           AOC
import           Grid
import           Memo
import           Path
import           Utils

type Code = [WithAction Int]

waP :: Parser Char (WithAction Int)
waP = (WButton <$> digitP) &| (WAction <$ requireP 'A')

codeP :: Parser Text Code
codeP = charactersP &** waP

parser :: Parser Text [Code]
parser = linesP &** codeP

data WithAction button
  = WButton button
  | WAction
  deriving (Ord, Eq, Show, Generic)

instance Enum button => Enum (WithAction button) where
  toEnum 0 = WAction
  toEnum n = WButton $ toEnum $ pred n
  fromEnum WAction     = 0
  fromEnum (WButton b) = succ $ fromEnum b

instance Bounded button => Bounded (WithAction button) where
  minBound = WAction
  maxBound = WButton maxBound

instance GridItem a => GridItem (WithAction a) where
  showInGrid (WButton a) = showInGrid a
  showInGrid WAction     = 'A'

waToMaybe :: WithAction a -> Maybe a
waToMaybe (WButton a) = Just a
waToMaybe WAction     = Nothing

waShow :: GridItem a => [WithAction a] -> Text
waShow = Text.pack . map showInGrid

instance Hashable button => Hashable (WithAction button)

type Keypad button = Map Position2 (WithAction button)

numeric :: Keypad Int
numeric =
  Map.fromList
    [ (Position2 0 0, WButton 7)
    , (Position2 1 0, WButton 8)
    , (Position2 2 0, WButton 9)
    , (Position2 0 1, WButton 4)
    , (Position2 1 1, WButton 5)
    , (Position2 2 1, WButton 6)
    , (Position2 0 2, WButton 1)
    , (Position2 1 2, WButton 2)
    , (Position2 2 2, WButton 3)
    , (Position2 1 3, WButton 0)
    , (Position2 2 3, WAction)
    ]

type Action = WithAction Direction4

directional :: Keypad Direction4
directional =
  Map.fromList
    [ (Position2 1 0, WButton N)
    , (Position2 2 0, WAction)
    , (Position2 0 1, WButton W)
    , (Position2 1 1, WButton S)
    , (Position2 2 1, WButton E)
    ]

type ListLen r = (Sum Int, [r])

llength :: ListLen r -> Int
llength = getSum . fst

type MoveAndPress a r = WithAction a -> WithAction a -> ListLen r

humanAction :: MoveAndPress a (WithAction a)
humanAction _ r = (Sum 1, [r])

robotAction ::
     forall a r. (Eq a, Show a, Ord r, Hashable r, Show r)
  => MoveAndPress Direction4 r
  -> Keypad a
  -> MoveAndPress a r
robotAction mv kp aFrom aTo =
  mconcat $ zipWithTail mv $ (++ [WAction]) $ map snd $ start : path
  where
    path =
      fromJustE "robotAction a*"
        $ aStarGoal moves cost (manhattanDistance pTo . fst) start
    pFrom = mapFindValueE "robotAction aFrom" (== aFrom) kp
    pTo = mapFindValueE "robotAction aTo" (== aTo) kp
    start = (pFrom, WAction)
    moves (p, _) = do
      d <- allDir4
      let p' = walk d p
      guard $ Map.member p' kp
      pure (p', WButton d)
    cost (p1, b1) (p2, b2)
      | p2 == pTo = llength $ m <> mLast
      | otherwise = llength m
      where
        m = mv b1 b2
        mLast = mv b2 WAction

enterCode :: MoveAndPress a r -> [WithAction a] -> ListLen r
enterCode mv code = mconcat $ map (uncurry mv) $ zipTail $ WAction : code

traceAction :: (Show n, Show a) => n -> MoveAndPress a r -> MoveAndPress a r
traceAction n a f t = traceShow (n, f, t, llength r) r
  where
    r = a f t

superRemotePresses :: Int -> Code -> ListLen (WithAction Direction4)
superRemotePresses n = enterCode $ robotAction (action n) numeric
  where
    action 0 = humanAction
    action n =
      robotAction (mapMemo2 $ traceAction n $ action $ pred n) directional

numericValue :: Code -> Int
numericValue =
  read
    . mapMaybe
        (\case
           WButton n -> Just $ head $ show n
           _ -> Nothing)

part :: Int -> [Code] -> Int
part n = sum . map go
  where
    go code = traceShow ("code", waShow code, "nv", nv, "l", l) nv * l
      where
        nv = numericValue code
        l = llength $ superRemotePresses n code

part1 :: [Code] -> Int
part1 = part 2

part2 :: [Code] -> Int
part2 = part 25

tasks =
  Tasks
    2024
    21
    (CodeBlock 3)
    parser
    [ Assert "level 1 keypad" "<A^A^^>AvvvA" -- different from the example but same length
        $ waShow
        $ snd
        $ enterCode (robotAction humanAction numeric)
        $ justParse codeP "029A"
    , task part1 126384 & taskPart 1
    , taskBlind part2 & taskPart 2
    ]

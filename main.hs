import Data.List

main :: IO ()
main = do
    let
        board = [] :: [Point]
        ant = Ant { antDirection = ToLeft, antPoint = (0, 0) }
        playResult = play ant board 20000

    printMap $ boardToStrings playResult

play :: Ant -> [Point] -> Int -> [Point]
play _   board 0 = board
play ant board step = 
    play antMoved boardUpdated (step - 1)
    where
        color = cellColorOf board (antPoint ant)
        antRotated = rotateAnt color ant
        boardUpdated = updateBoardByAnt board antRotated
        antMoved = moveForwardAnt antRotated
-- printMap
-- リストの各要素をプリントする。
printMap :: (Show elem_t) => [elem_t] -> IO ()
printMap [] = return ()
printMap (x:xs) = do
    print $ x
    printMap xs

-- boardToStrings
-- (x,y)のリスト（Blackの座標のみ）を画面表示用の文字列に変換する。
boardToStrings :: [Point] -> [String]
boardToStrings board = toFullBoardWithSymbol boardWithColor
    where
        boardFull = boardToFullBoard board
        boardWithColor = toFullBoardWithColor board boardFull

-- toFullBoardWithSymbol
-- 各行のセル色並びを画面表示用のシンボル列に変換する。
toFullBoardWithSymbol :: [[CellColor]] -> [String]
toFullBoardWithSymbol [] = []
toFullBoardWithSymbol boardWithColor = map colorsToSymbols boardWithColor
    where
        colorsToSymbols = \colors -> map cellColorToSymbol colors

-- toFullBoardWithColor
-- (x,y)のリスト（Black/Whiteの座標）の各座標をセル色に変換する。
toFullBoardWithColor :: [Point] -> [Point] -> [[CellColor]]
toFullBoardWithColor [] _ = []
toFullBoardWithColor _ [] = []
toFullBoardWithColor board boardFull = map pointsToColors boardGroupByY
    where
        boardGroupByY = groupByY boardFull
        pointsToColors = \points -> map pointToColor points
        pointToColor = \point -> cellColorOf board point

-- toFullBoardWithColor
-- リスト（Blackの座標のみ）を、AにWhiteの座標を補完したリストに変換する。
-- ex) リスト[(-1,1),(1,-1)]は、リスト[(x, y) | x <- [-1,0,1], y <- [-1,0,1]]に変換される。
boardToFullBoard :: [Point] -> [Point]
boardToFullBoard [] = []
boardToFullBoard board = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
    where
        xs = map (\(x, y) -> x) board
        ys = map (\(x, y) -> y) board
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys

-- groupByY
-- [(x,y)](x,y)のyでグループ化した座標グループのリストを作成する。
-- ex) [(1,1),(1,2),(2,1),(2,2)]は、[[(1,1),(2,1)],[(1,2),(2,2)]]に変換される。
groupByY :: [Point] -> [[Point]]
groupByY board = map yToPoint ys
    where
        ys = nub $ map (\(_, y) -> y) board
        yToPoint = \y -> filter (\(_x, _y) -> _y == y) board

-- rotateAnt
-- セルの色に応じて、アリを左90°または右90°に方向転換する。
rotateAnt :: CellColor -> Ant -> Ant
rotateAnt White (Ant ToUp    point) = Ant { antDirection = ToRight, antPoint = point }
rotateAnt White (Ant ToRight point) = Ant { antDirection = ToDown,  antPoint = point }
rotateAnt White (Ant ToDown  point) = Ant { antDirection = ToLeft,  antPoint = point }
rotateAnt White (Ant ToLeft  point) = Ant { antDirection = ToUp,    antPoint = point }
rotateAnt Black (Ant ToUp    point) = Ant { antDirection = ToLeft,  antPoint = point }
rotateAnt Black (Ant ToRight point) = Ant { antDirection = ToUp,    antPoint = point }
rotateAnt Black (Ant ToDown  point) = Ant { antDirection = ToRight, antPoint = point }
rotateAnt Black (Ant ToLeft  point) = Ant { antDirection = ToDown,  antPoint = point }

-- moveForwardAnt
-- アリが向いている方向に、アリを1マス前進させる。
moveForwardAnt :: Ant -> Ant
moveForwardAnt (Ant ToUp    (x, y)) = Ant { antDirection = ToUp,    antPoint = (x,     y - 1) }
moveForwardAnt (Ant ToRight (x, y)) = Ant { antDirection = ToRight, antPoint = (x + 1, y)     }
moveForwardAnt (Ant ToDown  (x, y)) = Ant { antDirection = ToDown,  antPoint = (x,     y + 1) }
moveForwardAnt (Ant ToLeft  (x, y)) = Ant { antDirection = ToLeft,  antPoint = (x - 1, y)     }

-- moveForwardAnt
-- 現在のセル色を反転後のセル色を返す。
reverseCell :: CellColor -> CellColor
reverseCell White = Black
reverseCell Black = White

-- updateBoardByAnt
-- アリの現在地のマスの背景色を反転させた座標リストを返す
updateBoardByAnt :: [Point] -> Ant -> [Point]
updateBoardByAnt board ant = updateBoardByPoint board (antPoint ant)

-- updateBoardByPoint
-- 指定座標のマスの背景色を反転させた座標リストを返す
updateBoardByPoint :: [Point] -> Point -> [Point]
updateBoardByPoint board point
    | elem point board = removeItem point board
    | otherwise = addItem point board

-- cellColorToSymbol
-- セルの背景色に対応する画面表示用のシンボルを返す。
cellColorToSymbol :: CellColor -> Char
cellColorToSymbol White = '_'
cellColorToSymbol Black = 'X'

-- cellColorOf
-- 指定座標のマスの色を取得する。
cellColorOf :: [Point] -> Point -> CellColor
cellColorOf [] _ = White
cellColorOf board point
    | elem point board = Black
    | otherwise = White

-- addItem
-- リスト末尾に要素を追加したリストを返す
addItem :: elem_t -> [elem_t] -> [elem_t]
addItem x ys = x:ys

-- removeItem
-- 指定の要素と一致する要素を削除したリストを返す。
-- 一致要素が複数存在する場合、もっともインデックスの小さい要素のみを削除したリストを返す。
removeItem :: (Eq elem_t) => elem_t -> [elem_t] -> [elem_t]
removeItem _ []     = []
removeItem x (y:ys)
    | x == y = ys
    | otherwise = y: removeItem x ys

type Point = (Int, Int)

data Ant = Ant {
    antDirection :: Direction,
    antPoint :: Point
}

data Direction = ToUp | ToDown | ToRight | ToLeft
data CellColor = White | Black

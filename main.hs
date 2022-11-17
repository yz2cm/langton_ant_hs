main :: IO ()
main = do
    let
        blackPoints = [] :: [Point]
        ant = Ant { antDirection = ToLeft, antPoint = (0, 0) }
        playResult = play ant blackPoints 20000

    mapM_ print $ blackPointsToStrings playResult

play :: Ant -> [Point] -> Int -> [Point]
play _   blackPoints 0 = blackPoints
play ant blackPoints step = 
    play antMoved blackPointsUpdated (step - 1)
    where
        color = toCellColor blackPoints (antPoint ant)
        antRotated = rotateAnt color ant
        blackPointsUpdated = updateBlackPointsByAnt blackPoints antRotated
        antMoved = moveForwardAnt antRotated

-- splitByLen
-- リストを指定の長さ単位で分割する。
-- ex) splitByLen 3 "abcdefgh" => ["abc", "def", "gh"]
splitByLen :: Int -> [elem_t] -> [[elem_t]]
splitByLen _ [] = []
splitByLen length xs = chunk:(splitByLen length remains)
    where
        chunk = take length xs
        remains = drop length xs

-- blackPointsToStrings
-- Blackの座標リストを画面表示用の文字列に変換する。
blackPointsToStrings :: [Point] -> [String]
blackPointsToStrings blackPoints = lines
    where
        lines = splitByLen width symbols :: [String]
        symbols = map toSymbol colors :: [Char]
        colors = map (toCellColor blackPoints) matrix :: [CellColor]
        matrix = buildMatrix blackPoints :: [Point]
        width = maxX - minX + 1
        minX = minimum xs
        maxX = maximum xs
        xs = map (\(x, y) -> x) blackPoints

-- buildMatrix
-- Blackの座標を包含する最小の四角形のマトリクスを返す。
buildMatrix :: [Point] -> [Point]
buildMatrix [] = []
buildMatrix blackPoints = buildMatrix' (minX, maxX) (minY, maxY)
    where
        minX = minimum $ map (\(x, _) -> x) blackPoints
        maxX = maximum $ map (\(x, _) -> x) blackPoints
        minY = minimum $ map (\(_, y) -> y) blackPoints
        maxY = maximum $ map (\(_, y) -> y) blackPoints

buildMatrix' :: (Int, Int) -> (Int, Int) -> [Point]
buildMatrix' (minX, maxX) (minY, maxY) = [(x, y) | y <- [minY..maxY], x <- [minX..maxX]]

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

-- reverseCell
-- 現在のセル色を反転後のセル色を返す。
reverseCell :: CellColor -> CellColor
reverseCell White = Black
reverseCell Black = White

-- updateBlackPointsByAnt
-- アリの現在地のマスの背景色を反転させた座標リストを返す。
updateBlackPointsByAnt :: [Point] -> Ant -> [Point]
updateBlackPointsByAnt blackPoints ant = updateBlackPointsByPoint blackPoints (antPoint ant)

-- updateBlackPointsByPoint
-- 指定座標のマスの背景色を反転させた座標リストを返す。
updateBlackPointsByPoint :: [Point] -> Point -> [Point]
updateBlackPointsByPoint blackPoints point
    | elem point blackPoints = removeItem point blackPoints
    | otherwise = addItem point blackPoints

-- toSymbol
-- セルの背景色に対応する画面表示用のシンボルを返す。
toSymbol :: CellColor -> Char
toSymbol White = '_'
toSymbol Black = 'X'

-- toCellColor
-- 指定座標のマスの色を取得する。
toCellColor :: [Point] -> Point -> CellColor
toCellColor [] _ = White
toCellColor blackPoints point
    | elem point blackPoints = Black
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

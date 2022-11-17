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
        matrix = buildMatrix blackPoints :: [Point]
        colors = map (toCellColor blackPoints) matrix :: [CellColor]
        symbols = map toSymbol colors :: [Char]
        lines = splitByLen width symbols :: [String]
        width = maxX - minX + 1
        minX = minimum xs
        maxX = maximum xs
        xs = map (\(x, y) -> x) blackPoints

-- buildMatrix
-- Blackの座標を包含する最小面積の四角形のマトリクスを返す。
buildMatrix :: [Point] -> [Point]
buildMatrix [] = []
buildMatrix blackPoints = buildMatrix' (minX, maxX) (minY, maxY)
    where
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        xs = map (\(x, _) -> x) blackPoints
        ys = map (\(_, y) -> y) blackPoints
        buildMatrix' :: (Int, Int) -> (Int, Int) -> [Point]
        buildMatrix' (minX, maxX) (minY, maxY) = [(x, y) | y <- [minY..maxY], x <- [minX..maxX]]

-- rotateAnt
-- セルの色に応じて、アリを左90°または右90°に方向転換する。
rotateAnt :: CellColor -> Ant -> Ant
rotateAnt color (Ant direction point) =
    Ant {
        antDirection = (rotate color direction),
        antPoint = point
    }
    where
        rotate :: CellColor -> Direction -> Direction
        rotate White ToUp    = ToRight
        rotate White ToRight = ToDown
        rotate White ToDown  = ToLeft
        rotate White ToLeft  = ToUp
        rotate Black ToUp    = ToLeft
        rotate Black ToRight = ToUp
        rotate Black ToDown  = ToRight
        rotate Black ToLeft  = ToDown

-- moveForwardAnt
-- アリが向いている方向に、アリを1マス前進させる。
moveForwardAnt :: Ant -> Ant
moveForwardAnt (Ant direction  point) =
    Ant {
        antDirection = direction,
        antPoint = (moveForward direction point)
    }
    where
        moveForward :: Direction -> Point -> Point
        moveForward ToUp    (x, y) = (x,   y-1)
        moveForward ToRight (x, y) = (x+1, y)
        moveForward ToDown  (x, y) = (x,   y+1)
        moveForward ToLeft  (x, y) = (x-1, y)

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

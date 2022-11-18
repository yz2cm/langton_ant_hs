import Data.List (delete)

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
        symbols = map cellToSymbol matrix :: [Char]
        lines = splitByLen width symbols :: [String]
        cellToSymbol = toSymbol . toCellColor blackPoints
        width = maxX - minX + 1
        minX = minimum xs
        maxX = maximum xs
        xs = map (\(x, y) -> x) blackPoints

-- buildMatrix
-- Blackの座標を包含する最小面積の四角形のマトリクスを返す。
buildMatrix :: [Point] -> [Point]
buildMatrix [] = []
buildMatrix blackPoints = [(x, y) | y <- [minY..maxY], x <- [minX..maxX]]
    where
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        xs = map (\(x, _) -> x) blackPoints
        ys = map (\(_, y) -> y) blackPoints

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

-- updateBlackPointsByAnt
-- アリの現在地のマスの背景色を反転させた座標リストを返す。
updateBlackPointsByAnt :: [Point] -> Ant -> [Point]
updateBlackPointsByAnt blackPoints ant = updateBlackPointsByPoint blackPoints (antPoint ant)

-- updateBlackPointsByPoint
-- 指定座標のマスの背景色を反転させた座標リストを返す。
-- 「マスの背景色を反転させる」とは、実装上は、Blackの座標リストから当該座標を削除するか、リストに追加することを示す。
updateBlackPointsByPoint :: [Point] -> Point -> [Point]
updateBlackPointsByPoint blackPoints point
    | elem point blackPoints = Data.List.delete point blackPoints
    | otherwise = point:blackPoints

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

type Point = (Int, Int)

data Ant = Ant {
    antDirection :: Direction,
    antPoint :: Point
}

data Direction = ToUp | ToDown | ToRight | ToLeft
data CellColor = White | Black

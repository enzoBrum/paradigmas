module Main (main) where
import Dlx
import Data.List (intercalate, sort)
import System.IO
import System.Environment
import Debug.Trace
import Data.Map qualified as Map
data Cell = Cell {
    i :: Int,
    j :: Int,
    value :: Int
} deriving (Eq, Show)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Show)

type MapDirection = Map.Map Int [Direction]


readLines :: Handle -> IO ([[Int]])
readLines file = do
    eof <- hIsEOF file
    if eof then return ([])
    else do
        line <- hGetLine file
        let lineList = [map (\n -> read n ::Int) $ words line]
        (readLines file) >>= (\lines -> return (lineList ++ lines))


lookRight :: [[Int]] -> Int -> Int -> Int -> Int -> Maybe(Direction)
lookRight matrix i j size region_size
    | (mod (j+1) region_size) == 0 = Nothing
    | j == size - 1 = Nothing
    | (matrix!!i!!j) > (matrix!!i!!(j+1)) = Just RIGHT
    | otherwise = Just LEFT

lookDown :: [[Int]] -> Int -> Int -> Int -> Int -> Maybe(Direction)
lookDown matrix i j size region_size
    | (mod (i+1) region_size) == 0 = Nothing
    | i == size - 1 = Nothing
    | (matrix!!i!!j) > (matrix!!(i+1)!!j) = Just DOWN
    | otherwise = Just UP

getIndex1D :: Int -> Int -> Int -> Int
getIndex1D size i j = i*size + j


readComparisons :: [[Int]] -> Int -> Int -> MapDirection
readComparisons matrix i j 
    | i >= size = Map.empty :: MapDirection
    | j >= size = readComparisons matrix (i+1) 0
    | otherwise = do
        let compMap = Map.empty :: MapDirection

        let mp = case (lookRight matrix i j size region_size, lookDown matrix i j size region_size) of
               
                (Just rightDir, Just downDir) -> Map.insert (getIndex1D size i j) [rightDir, downDir] compMap
                (Just rightDir, _)            -> Map.insert (getIndex1D size i j) [rightDir] compMap
                (_, Just downDir)             -> Map.insert (getIndex1D size i j) [downDir] compMap
                (_,_)                         -> Map.insert (getIndex1D size i j) [] compMap
        
        Map.union mp (readComparisons matrix i (j+1))
    where 
        size = length matrix 
        region_size = floor ( sqrt $ fromIntegral (length matrix))
    

-- Cada célula do tabuleiro possui n linhas na matriz binária, onde n é o tamanho de uma região.
-- Cada linha da matriz binária possui n*n colunas representando o número de uma célula do tabuleiro.
{-
Ex:
    para um tabuleiro de 4 células no qual cada célula pode ter 2 valores

    1000
    1000
    0100
    0100
    0010
    0010
    0001
    0001
-}
constraintCell :: Int -> Int -> (Int, Cell)
constraintCell n i = do
    let one = div i n
    let cell_i = div one n
    let cell_j = mod one n
    let cell_value = (mod i n) + 1

    (one, Cell cell_i cell_j cell_value)


-- Cada grupo possui n casas dedicada a ela, representando os valores que podem estar dentro do grupo
{-
Ex:
    Se cada grupo pode ter 4 valores e temos dois grupos, cada um com quatro células

    1000'0000  (0,0)
    0100'0000
    0010'0000
    0001'0000
    1000'0000  (0,1)
    0100'0000
    0010'0000
    0001'0000
    0000'1000  (0,2)
    0000'0100
    0000'0010
    0000'0001
    0000'1000  (0,2)
    0000'0100
    0000'0010
    0000'0001
    1000'0000  (1,0)
    0100'0000
    0010'0000
    0001'0000
    1000'0000  (1,1)
    0100'0000
    0010'0000
    0001'0000
    0000'1000  (1,2)
    0000'0100
    0000'0010
    0000'0001
    0000'1000  (1,3)
    0000'0100
    0000'0010
    0000'0001
-}
{-
0 1 2 3     --> 0 --> 0
4 5 6 7     --> 0 
8 9 10 11   --> 1 --> 1
12 13 14 15 --> 1
16 17 18 19 --> 0 --> 2
20 21 22 23 --> 0
24 25 26 27 --> 1 --> 3
28 29 30 31 --> 1
32 33 34 35 --> 2 --> 4
36 37 38 39 --> 2
-}
constraintGroup :: Int -> Int -> Int
constraintGroup n i = do
    let one = div i n
    let cell_i = div one n
    let cell_j = mod one n
    
    let region_size = floor (sqrt $ fromIntegral n)

    let region_num = (div cell_i region_size) + (div cell_j region_size)
    
    let k = mod i n

    let offset = n*n

    offset+ (region_num)*n + k

-- Cada linha possui n casas dedicada a ela, representando os valores que podem estar dentro da linha
{-
Ex:
    Se cada linha pode ter 4 valores e temos duas linhas, cada uma com duas células

    1000'0000  (0,0)
    0100'0000
    0010'0000
    0001'0000
    1000'0000  (0,1)
    0100'0000
    0010'0000
    0001'0000
    0000'1000  (1,0)
    0000'0100
    0000'0010
    0000'0001
    0000'1000  (1,1)
    0000'0100
    0000'0010
    0000'0001
-}
constraintRow :: Int -> Int -> Int
constraintRow n i = offset + k
    where 
        offset = n*n*2 + (div i (n*n)) * n
        k = mod i n

-- Cada coluna possui n casas dedicada a ela, representando os valores que podem estar dentro da dela
{-
Ex:
    Se cada coluna pode ter 4 valores e temos duas colunas, cada uma com duas células

    1000'0000 (0,0)
    0100'0000
    0010'0000
    0001'0000
    0000'1000 (1,0)
    0000'0100
    0000'0010
    0000'0001
    1000'0000 (1,0)
    0100'0000
    0010'0000
    0001'0000
    0000'1000 (1,1)
    0000'0100
    0000'0010
    0000'0001
-}

constraintColumn :: Int -> Int -> Int
constraintColumn n i = offset + k
    where 
        offset = n*n*3 + (mod (div i n) n) * n
        k = mod i n


createListOfRow :: [Int] -> Int -> Int -> [Int]
createListOfRow [] j num_cols 
    | j == num_cols = []
    | otherwise = 0 : (createListOfRow  [] (j+1) num_cols)
createListOfRow (one:ones_list) j num_cols 
    | j == num_cols = []
    | j == one = 1 : (createListOfRow  ones_list (j+1) num_cols)
    | otherwise = 0 : (createListOfRow (one:ones_list) (j+1) num_cols)

createRow :: [[Int]] -> MapDirection -> Int -> Int -> ([Int], Cell)
createRow matrix mp num_cols i = do
    let n = length matrix
    let (numCell, cell) = constraintCell n i

    let numGroup = constraintGroup n i

    let numRow = constraintRow n i

    let numCol = constraintColumn n i


    let ones_list =  sort [numCell, numGroup, numRow, numCol]
    (createListOfRow ones_list 0 num_cols, cell)

createRows :: [[Int]] -> MapDirection -> Int -> Int -> Int -> ([[Int]], [Cell])
createRows matrix mp num_rows num_cols i
    | i == num_rows = ([], [])
    | otherwise = do
        let (row, cell) = createRow matrix mp num_cols i
        let (rows, cells) = createRows matrix mp num_rows num_cols (i+1)
        (row : rows, cell : cells)

createBinaryMatrix :: [[Int]] -> MapDirection -> [[Int]]
createBinaryMatrix matrix mp = do
    let n = length matrix
    let m = length (head matrix)
    let area_size = floor ( sqrt $ fromIntegral (length matrix)) * floor ( sqrt $ fromIntegral (length matrix))
    let sum_areas = n*n
    let num_areas = floor (fromIntegral sum_areas / fromIntegral area_size)
    let square_areas = area_size*area_size*num_areas

    -- num_rows --> area_size*n*m --> cada célula pode ter area_size valores
    let num_rows = area_size*n*m

    -- constraint 1 (cell) --> n*m --> número de células na matriz
    -- constaint 2 (row) --> n*area_size --> n colunas com tamanho de area_size
    -- constraint 3 (col) --> n*area_size --> n colunas com tamanho de area_size
    -- constraint 4 (group) --> num_areas*area_size
    -- constraint 5 (comparação horizontal) --> square_areas --> area_size valores para cada célula
    -- constraint 5 (comparação vertical) --> square_areas --> area_size valores para cada célula
    let num_cols = n*m + n*area_size + n*area_size + num_areas*area_size + 2*square_areas
    
    let (rows, cells) = createRows matrix mp num_rows num_cols 0
    rows

readSudoku :: String -> IO ([[Int]])
readSudoku filename = do
    file <- openFile filename ReadMode
    matrix <- readLines file
    let dMap = readComparisons matrix 0 0
    let cells = createBinaryMatrix matrix dMap
    return (cells)



    

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "Usage: sudoku <input-file>"
        return ()
    else do
        l <- readSudoku (args!!0)
        putStrLn (unlines $ map (intercalate "" . map show) (l))
        return ()
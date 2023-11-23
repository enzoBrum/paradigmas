module Main (main) where
import Dlx
import Data.List (intercalate, sort, find)
import System.IO
import System.Environment
import Debug.Trace
import Data.Map qualified as Map

-- Representa uma célula presente na matriz tabuleiro
data Cell = Cell {
    i :: Int,
    j :: Int,
    value :: Int
} deriving (Eq, Show)


-- Direção da comparação
-- A < B --> LEFT
-- A > B --> RIGHT
-- A ^ B --> UP
-- A v B --> DOWN
-- Em todos esses exemplos, A está sendo comparado com B e 
-- Direction representa a direção da flecha
data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Show)

-- Map no qual as direções estão armazenadas. O índice é dado
-- pela fórmula i*n + j, de formo que cada célula no tabuleiro
-- possui um índice único
type MapDirection = Map.Map Int [Direction]


-- Lê o conteúdo de um arquivo em uma matriz de inteiros
readLines :: Handle -> IO ([[Int]])
readLines file = do
    eof <- hIsEOF file
    if eof then return ([])
    else do
        line <- hGetLine file
        let lineList = [map (\n -> read n ::Int) $ words line]
        (readLines file) >>= (\lines -> return (lineList ++ lines))


-- Dado uma célula (i, j), verifica se a célula (i, j+1) está
-- na mesma região que a primeira. Se sim, faz uma comparação entre elas
-- para definir a direção de comparação da célula (i,j). 
lookRight :: [[Int]] -> Int -> Int -> Int -> Int -> Maybe(Direction)
lookRight matrix i j size region_size
    | (mod (j+1) region_size) == 0 = Nothing
    | j == size - 1 = Nothing
    | (matrix!!i!!j) > (matrix!!i!!(j+1)) = Just RIGHT
    | otherwise = Just LEFT

-- Dado uma célula (i, j), verifica se a célula (i+1, j) está
-- na mesma região que a primeira. Se sim, faz uma comparação entre elas
-- para definir a direção de comparação da célula (i,j). 
lookDown :: [[Int]] -> Int -> Int -> Int -> Int -> Maybe(Direction)
lookDown matrix i j size region_size
    | (mod (i+1) region_size) == 0 = Nothing
    | i == size - 1 = Nothing
    | (matrix!!i!!j) > (matrix!!(i+1)!!j) = Just DOWN
    | otherwise = Just UP


getIndex1D :: Int -> Int -> Int -> Int
getIndex1D size i j = i*size + j


-- Recebe a matriz solução do sudoku e gera um mapa de 
-- direções de comparação.
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
                -- (_,_)                         -> Map.insert (getIndex1D size i j) [] compMap
                (_,_)                         -> compMap
        
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
constraintGroup :: Int -> Int -> Int
constraintGroup n i = do
    let one = div i n
    let cell_i = div one n
    let cell_j = mod one n

    let region_line_size = floor (sqrt $ fromIntegral n)
    let region_num = (div cell_i region_line_size)*region_line_size + (div cell_j region_line_size)
    
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


getOnesFromDirection :: Int -> Int -> Int -> Direction -> [Int]
getOnesFromDirection n k offset d
    | d == LEFT || d == UP = [offset..(offset + k)]
    | otherwise = [(offset + k)..end_offset]
        where 
            end_offset = offset + n - 1


-- Cada célula pode ser menor ou maior que a célula à direita que está na mesma região.main
-- SeDigamos que uma célula x seja menor que x + 1, o valor de x será = 0010'1110,
-- Senão, 0010'0011

{-
3 2

0010'0011 = 3
0000'0100 = 2

2 3
0100'1100 = 2
0000'0010 = 3
-}
constraintHorizontalComparison :: Int -> Int -> MapDirection -> [Int]
constraintHorizontalComparison n i mp = do
    let one = div i n
    let cell_i = div one n
    let cell_j = mod one n
    
    let offset = n*n*4 + (div i n)*n
    let k = mod i n
    let next_offset = offset + n
    case Map.lookup (cell_i*n + cell_j) mp of
            Just dirList -> case find (\x -> x == LEFT || x == RIGHT) dirList of
                Just direction -> (offset + k) : (getOnesFromDirection n k next_offset direction)
                Nothing -> [offset+k]
            Nothing -> [offset+k]

-- Cada célula pode ser menor ou maior que a célula abaixo que está na mesma região
-- Digamos que uma célula y seja menor que y + 1, o valor de x será = 0010'1110,
-- Senão, 0010'0011

{-
3 2
4 1

0010'0000'1110'0000 = 3
0000'0100'0000'0111 = 2
0000'0000'0001'0000 = 4
0000'0000'0000'1000 = 1

-}
constraintVerticalComparison :: Int -> Int -> MapDirection -> [Int]
constraintVerticalComparison n i mp = do
    let one = div i n
    let cell_i = div one n
    let cell_j = mod one n
    
    let offset = n*n*4 + n*n*n + (div i n)*n
    let k = mod i n
    let next_offset = offset + n*n
    case Map.lookup (cell_i*n + cell_j) mp of
            Just dirList -> case find (\x -> x == UP || x == DOWN) dirList of
                Just direction -> (offset + k) : (getOnesFromDirection n k next_offset direction)
                Nothing -> [offset+k]
            Nothing -> [offset+k]


-- Dado uma lista de posições onde o 1 deve estar, 
-- cria uma lista com 1's nessas posições e 0's no
-- restante
createListOfRow :: [Int] -> Int -> Int -> [Int]
createListOfRow [] j num_cols 
    | j == num_cols = []
    | otherwise = 0 : (createListOfRow  [] (j+1) num_cols)
createListOfRow (one:ones_list) j num_cols 
    | j == num_cols = []
    | j == one = 1 : (createListOfRow  ones_list (j+1) num_cols)
    | otherwise = 0 : (createListOfRow (one:ones_list) (j+1) num_cols)

-- cria uma linha da matriz binária
createRow :: [[Int]] -> MapDirection -> Int -> Int -> ([Int], Cell)
createRow matrix mp num_cols i = do
    let n = length matrix
    let (numCell, cell) = constraintCell n i

    let numGroup = constraintGroup n i

    let numRow = constraintRow n i

    let numCol = constraintColumn n i

    let numComparisonHorizontal = constraintHorizontalComparison n i mp
    let numComparisonVertical = constraintVerticalComparison n i mp


    let ones_list =  sort ([numCell, numGroup, numRow, numCol] ++ numComparisonHorizontal ++ numComparisonVertical)
    (createListOfRow ones_list 0 num_cols, cell)


-- cria linhas secundárias da matriz binária. Isso é necessário
-- devido ao fato de que as constraints de comparação nem sempre
-- preencherão todas as colunas dedicadas a elas com 1.
{-
2 3

0100'1100 = 2
0000'0010 = 3

Várias colunas ficaram sem 1. Como o Dancing Links precisa que
haja um único 1 em cada coluna, adicionamos linhas com 1 em cada
coluna destinada as constraints de comparação.

O exemplo acima é transformado nisso:

01001100
00000010
10000000
01000000
00100000
00010000
00001000
00000100
00000010
00000001

Devido à constraint da Célula, essas linhas extras não afetarão a solução encontrada.
-}
createSecondaryRows :: Int -> Int -> Int -> [[Int]]
createSecondaryRows n num_cols j
    | one == num_cols = []
    | otherwise = (createListOfRow [one] 0 num_cols) : (createSecondaryRows n num_cols (j+1))
        where
            one = n*n*2 + j 

-- Cria as linhas presentes na matriz binária
createRows :: [[Int]] -> MapDirection -> Int -> Int -> Int -> ([[Int]], [Cell])
createRows matrix mp num_rows num_cols i
    | i == num_rows = ([], [])
    | otherwise = do
        let (row, cell) = createRow matrix mp num_cols i
        let (rows, cells) = createRows matrix mp num_rows num_cols (i+1)
        ((row : rows), cell : cells)

createBinaryMatrix :: [[Int]] -> MapDirection -> ([[Int]], [Cell])
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
    (rows ++ (createSecondaryRows n num_cols 0), cells)

readSudoku :: String -> IO ([[Int]], [[Int]], [Cell])
readSudoku filename = do
    file <- openFile filename ReadMode
    matrix <- readLines file
    let dMap = readComparisons matrix 0 0
    let (bin_matrix, cells) = (createBinaryMatrix matrix dMap)
    return (matrix, bin_matrix, cells)


partitionList :: Int -> [Int] -> [[Int]]
partitionList _ [] = []
partitionList n xs
    | n > 0 = take n xs : partitionList n (drop n xs)
    | otherwise = []

createSolutionMatrix :: [Int] -> Int -> [Cell] -> [[Int]]
createSolutionMatrix solutions n cells = partitionList n flatMatrix
    where 
        flatMatrix = [(value x) | (x, index) <- zip cells [0..], elem index solutions]

findSolution :: [[Int]] -> [[Int]] -> [Cell] -> Maybe([[Int]])
findSolution matrix bin_matrix cells = do
    let secondary = [False | _ <- [1..(length (head bin_matrix))]]
    
    case dancingLinks bin_matrix secondary of
        Just solutions -> return $ createSolutionMatrix solutions (length matrix) cells
        Nothing -> Nothing
    

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "Usage: sudoku <input-file>"
        return ()
    else do
        (matrix, bin_matrix, cells) <- readSudoku (args!!0)

        case findSolution matrix bin_matrix cells of
            Just ans -> do        
                putStrLn "Solução: "
                putStrLn (unlines $ map (intercalate " " . map show) (ans))
                putStrLn ("Comparação com input: " ++ (show $ matrix == ans))
                return ()
            Nothing -> do
                putStrLn "Sem solução!"
                return ()
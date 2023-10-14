module Main (main) where
import Control.Monad.ST
-- import qualified Data.Array as Array
import  Data.Array.ST
import Data.Array
import Data.STRef
import Control.Monad
import Debug.Trace
-- Oh Functional God, forgive me for what I've done! I've ate the forbidden fruit! I've ate the imperative fruit!

data Node s = Node { 
    left :: STRef s (Node s), 
    right :: STRef s (Node s),
    up :: STRef s (Node s), 
    down :: STRef s (Node s), 
    header :: STRef s (Node s),
    numRow :: Int,
    idNode :: Int
}

instance Eq (Node s) where
    (==) node1 node2 = (idNode node1 == idNode node2 && numRow node1 == numRow node2)


newNode :: Int -> Int -> ST s (Node s)
newNode row col = do
  left <- newSTRef (error "Left reference is uninitialized")
  return $ Node left left left left left row col


initColumns :: Node s -> Node s -> Int -> [Bool] -> ST s [Node s]
initColumns root nd m secondary 
    | m == length secondary = return []
    | otherwise = do
        node <- newNode (-1) m
        traceM (show ((numRow nd), (idNode nd)))
        traceM (show ((numRow node), (idNode node)))

        if not $ secondary!!m then do
            writeSTRef (left node) nd
            writeSTRef (right node) root
            writeSTRef (up node) node
            writeSTRef (down node) node
            writeSTRef (right nd) node
            writeSTRef (left root) node
        else do
            writeSTRef (left node) node
            writeSTRef (right node) node
            writeSTRef (up node) node
            writeSTRef (down node) node

        otherColumns <-  initColumns root node (m+1) secondary
        return (node : otherColumns)


initRow :: Array Int (Node s) -> Array (Int, Int) Int -> Int -> Int -> Node s -> Node s ->  Bool -> ST s ()
initRow columns matrix i j first node first_found = do
    let (n, m) = snd $ bounds matrix

    if j >= m then
        return ()
    else do
        if (matrix ! (i,j)) == 1 then do
            let col = columns ! j
            last_col <- readSTRef (up col)
            
            if not first_found then do
                writeSTRef (left node) node
                writeSTRef (right node) node
                writeSTRef (header node) col
                writeSTRef (down last_col) node
                writeSTRef (up node) last_col
                writeSTRef (up col) node
                initRow columns matrix i (j+1) first node True
            else do
                new <- newNode i j
                writeSTRef (right node) new
                writeSTRef (left new)  node
                writeSTRef (left first) new
                writeSTRef (right new) first
                writeSTRef (header new) col
                writeSTRef (down last_col) new
                writeSTRef (up new) last_col
                writeSTRef (up col) new
                initRow columns matrix i (j+1) first new True
        else do
            return ()


initRows :: Array Int (Node s) -> Array (Int, Int) Int -> Int -> ST s ()
initRows columns matrix i = do
    let (n, m) = snd $ bounds matrix

    if i >= m then
        return ()
    else 
        do
            first <- newNode i (-1)
            initRow columns matrix i 0 first first False
            initRows columns matrix (i+1)


convertToArray :: [Node s] -> ST s( Array Int (Node s) )
convertToArray columns = return (listArray (0, length columns - 1) columns)

convertToMatrix ::  [[Int]] -> Array (Int, Int) Int
convertToMatrix matrix = 
    let n = length matrix
        m = length (head matrix)
    in
        listArray ( (0,0), (n-1, m-1) ) (concat matrix)

initDancingLinks :: Array (Int, Int) Int -> [Bool] -> ST s (Node s)
initDancingLinks matrix secondary = do
    let n = length matrix
    -- let m = length secondary

    root <- newNode (-1) (-1)
    columns <- initColumns root root 0 secondary
    
    arrColumns <- convertToArray columns

    -- initRows arrColumns matrix 0

    col <- readSTRef (right root)
    traceM ("AA " ++ show ((numRow col), (idNode col)))
    d <- readSTRef (down col)
    traceM ("AB " ++ show ((numRow d), (idNode d)))

    return root



iterateNodes :: Node s -> Node s -> (Node s -> ST s(Node s)) -> (Node s -> ST s()) -> ST s()
iterateNodes from to step func  | from == to = return ()
                           | otherwise = do
                            func from
                            newFrom <- step from
                            iterateNodes newFrom to step func

cover :: Node s -> ST s()
cover col = do
    leftNode <- readSTRef (left col)
    rightNode <- readSTRef (right col)

    writeSTRef (left rightNode) leftNode
    writeSTRef (right leftNode) rightNode

    first <- readSTRef (down col)
    let stepDown nd = readSTRef (down nd)
    let stepRight nd = readSTRef (right nd)

    iterateNodes first col stepDown (\nd -> do
        firstRight <- readSTRef (right nd)
        
        iterateNodes firstRight nd stepRight (\nd2 -> do
            ndDown <- readSTRef (down nd2)
            ndUp <- readSTRef (up nd2)

            writeSTRef (up ndDown) ndUp
            writeSTRef (down ndUp) ndDown
            )
        )

uncover :: Node s -> ST s()
uncover col = do
    first <- readSTRef (up col)
    let stepUp nd = readSTRef (up nd)
    let stepLeft nd = readSTRef (left nd)

    iterateNodes first col stepUp (\nd -> do
        firstLeft <- readSTRef (left nd)
        
        iterateNodes firstLeft nd stepLeft (\nd2 -> do
            ndDown <- readSTRef (down nd2)
            ndUp <- readSTRef (up nd2)

            writeSTRef (up ndDown) nd2
            writeSTRef (down ndUp) nd2
            )
        )

    leftNode <- readSTRef (left col)
    rightNode <- readSTRef (right col)

    writeSTRef (left rightNode) col
    writeSTRef (right leftNode) col


searchCol :: Node s -> Node s -> Node s -> ST s([Int], Bool)
searchCol root col curr 
    | curr == col = do
        traceM "Biz"
        uncover col
        return ([], False)
    | otherwise = do
        traceM "foo"
        rightNode <- readSTRef (right curr)
        iterateNodes rightNode curr (readSTRef . right) (\nd -> readSTRef (header nd) >>= \nd2 -> cover nd2)
        
        (solutions, found) <- search root

        if found then
            return ( numRow curr : solutions, True )
        else do
            leftNode <- readSTRef (left curr)
            iterateNodes leftNode curr ( readSTRef . left ) (\nd -> readSTRef (header nd) >>= \nd2 -> uncover nd2)

            newCurr <- readSTRef (down curr)
            searchCol root newCurr col
                            
    

search :: Node s -> ST s ([Int], Bool)
search root = do
    col <- readSTRef (right root)
    traceM (show ((numRow col), (idNode col)))
    traceM (show ((numRow root), (idNode root)))
    if root == col then return ([], True)
    else do
        cover col
        d <- readSTRef (down col)
        traceM (show ((numRow d), (idNode d)))
        searchCol root col =<< readSTRef (down col)


dancingLinks :: [[Int]] -> [Bool] -> [[Int]]
dancingLinks matrix secondary = runST $ do
    let arrMatrix = convertToMatrix matrix
    root <- initDancingLinks arrMatrix secondary
    return [[1]]
    -- (solutions, found) <- search root
    -- traceM (show found)
    -- return [matrix!!row | row <- solutions]

main :: IO ()
main = do
    let matrix = [
            [1, 0, 1, 0],
            [0, 1, 0, 0],
            [0, 0, 0, 1],
            [1, 1, 1, 0]
            ]
    
    let secondary = [False | i <- head matrix]

    print $ dancingLinks matrix secondary

import Control.Monad.ST
-- import qualified Data.Array as Array
import  Data.Array.ST
import Data.Array ()
import Data.STRef
import Debug.Trace


data Node s = Node { 
    left :: STRef s (Node s), 
    right :: STRef s (Node s),
    value :: Int
    -- up :: STRef s (Node s), 
    -- down :: STRef s (Node s), 
    -- header :: STRef s (Node s),
    -- id :: Int,
    -- numRow :: Int
}


newList :: Int -> ST s (Node s)
newList v = do
  left <- newSTRef (error "Left reference is uninitialized")
  right <- newSTRef (error "Left reference is uninitialized")
  return $ Node left right v


addToList :: Int -> Node s -> ST s(Node s)
addToList val nd = do
  node <- newList val
  
  lft <- readSTRef $ left nd

  writeSTRef (left nd) node
  
  return nd


debug :: Node s -> ST s()
debug nd = do
  traceM ("id "++ show (value nd))

  l <- readSTRef (left nd)
  traceM ("left " ++show (value l))
  r <- readSTRef (right nd)
  traceM ("right " ++ show (value r))

testNode :: [Int]
testNode = runST $ do
  l <- newList 0
  r <- newList 1
  u <- newList 2
  d <- newList 3

  writeSTRef (left l) r
  writeSTRef (right l) u

  debug l

  writeSTRef (right u) d
  writeSTRef (right u) l

  abc <- readSTRef (right l)
  abcd <- readSTRef (right abc)

  traceM (show (value abcd))
    

  

  return [ 0 ]
  

main :: IO()
main = do
  print  testNode



-- -- Function to create a mutable unboxed array and update it
-- modifyArray :: STArray s Int Node -> ST s ()
-- modifyArray arr = do
--   writeArray arr 0 
--   writeArray arr 1 20
--   writeArray arr 2 30

-- -- Function to run the computation and return the immutable unboxed array
-- runMutableArray :: Array Int Node
-- runMutableArray = runSTArray $ do
--   arr <- newArray (0, 2) (Node 0 0) :: ST s (STArray s Int Node)  -- Create a mutable unboxed array with initial values 0
--   modifyArray arr           -- Modify the array
--   return arr                -- Convert the mutable array to an immutable one

-- main :: IO ()
-- main = do
--   let resultArray = runMutableArray
--   let val = resultArray ! 0
--   print val
--   putStrLn $ "Result: " ++ show resultArray
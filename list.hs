import Control.Monad.ST
-- import qualified Data.Array as Array
import  Data.Array.ST
import Data.Array ()
import Data.STRef



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


testNode :: [Int]
testNode = runST $ do
  ref <- newList 10
  addToList 20 ref

  left_nd <- readSTRef $ left ref
  addToList 40 left_nd
  addToList 30 ref

  left_nd2 <- readSTRef $ left ref
  
  writeSTRef (right ref) left_nd

  right_nd <- readSTRef $ right ref

  return [ value ref, value left_nd, value right_nd, value left_nd2 ]
  

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
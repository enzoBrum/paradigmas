partitionList :: Int -> [a] -> [[a]]
partitionList _ [] = []
partitionList n xs
    | n > 0 = take n xs : partitionList n (drop n xs)
    | otherwise = []

main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5, 6]
    let groupSize = 2
    let partitioned = partitionList groupSize myList
    print partitioned  -- Output: [[1,2],[3,4],[5,6]]
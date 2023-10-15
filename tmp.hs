import qualified Data.Map as Map

main :: IO ()
main = do
    let originalMap = Map.fromList [(1, [10, 20, 30]), (2, [40, 50, 60])]

    let keyToAddTo = 1
    let elementToAdd = 40

    let modifiedMap = case Map.lookup keyToAddTo originalMap of
            Just lit -> Map.insert keyToAddTo (elementToAdd : lit) originalMap
            Nothing   -> originalMap

    -- Print the modified map
    putStrLn "Modified Map:"
    print modifiedMap
module MethodTable where

import qualified Data.Map as Map
import qualified Data.List as List

-- Types to manage the classes and methods
type MethodTable = Map.Map String String
type ClassTable = Map.Map String MethodTable


-- Function to add a Class, if the name already exists or if
-- there are duplicate methods returns Nothing
addClass :: ClassTable -> String -> [String] -> Maybe ClassTable
addClass classTable name fs = 
    case (Map.member name classTable) || (hasDuplicates fs) of
        True ->
            Nothing
        False -> 
            Just $ Map.insert name methodTable classTable
    where
        methodTable = createMethodTable Map.empty name fs

-- Function to add a subclass, ame as previous one but if the 
-- superclass doesnt exist it also returns nothing
addSubclass :: ClassTable -> String -> String -> [String] -> Maybe ClassTable
addSubclass classTable name superName fs = 
    case (Map.member name classTable) || (not $ Map.member superName classTable) || (hasDuplicates fs) of
        True ->
            Nothing
        False ->
            Just $ Map.insert name methodTable classTable
    where
        methodTable = createMethodTable superNameTable name fs
        superNameTable = classTable Map.! superName

-- Function to add methods in the method table
createMethodTable :: MethodTable -> String -> [String] -> MethodTable
createMethodTable methodTable name []     = methodTable
createMethodTable methodTable name (f:fs) = createMethodTable newMT name fs
    where newMT = Map.insert f name methodTable

-- Function to describe the methods of a class, returns nothing if
-- the class doesnt exist
describe :: ClassTable -> String -> Maybe [String]
describe classTable name = 
    case Map.member name classTable of
        False ->
            Nothing
        True ->
            Just $ map (\(x,y) -> x ++ " -> " ++ y ++ " :: " ++ x) methodTable
    where
        methodTable = Map.toList $ classTable Map.! name

-- Function to check for duplicates
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (List.nub xs) /= length xs
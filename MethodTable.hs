module MethodTable where

import qualified Data.Map as Map
import qualified Data.List as List

type MethodTable = Map.Map String String

type ClassTable = Map.Map String MethodTable


addClass :: ClassTable -> String -> [String] -> Maybe ClassTable
addClass classTable name fs = 
    case Map.member name classTable of
        True ->
            Nothing
        False -> 
            Just $ Map.insert name methodTable classTable
    where
        methodTable = createMethodTable Map.empty name fs

addSubclass :: ClassTable -> String -> String -> [String] -> Maybe ClassTable
addSubclass classTable name superName fs = 
    case (Map.member name classTable) || (not $ Map.member superName classTable) of
        True ->
            Nothing
        False ->
            Just $ Map.insert name methodTable classTable
    where
        methodTable = createMethodTable superNameTable name fs
        superNameTable = classTable Map.! superName

createMethodTable :: MethodTable -> String -> [String] -> MethodTable
createMethodTable methodTable name []     = methodTable
createMethodTable methodTable name (f:fs) = createMethodTable newMT name fs
    where newMT = Map.insert f name methodTable

describe :: ClassTable -> String -> Maybe [String]
describe classTable name = 
    case Map.member name classTable of
        False ->
            Nothing
        True ->
            Just $ map (\(x,y) -> x ++ " -> " ++ y ++ " :: " ++ x) methodTable
    where
        methodTable = Map.toList $ classTable Map.! name
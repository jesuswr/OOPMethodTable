module Main where

import System.Directory
import Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import MethodTable

-- Main function
main :: IO()
main = do
    runMainLoop Map.empty

-- Function to interact with the user
runMainLoop :: ClassTable -> IO()
runMainLoop classTable = do
    putStrLn "\nQUE DESA HACER?"
    tmp <- getLine
    let inp = words tmp
    case inp of
        "CLASS":tail -> 
            case tail of
                name:":":super:functions -> 
                    case addSubclass classTable name super functions of
                        Nothing -> do
                            putStrLn "ERROR CON LOS NOMBRES DADOS"
                            runMainLoop classTable
                        Just newClassTable ->
                            runMainLoop newClassTable
                name:functions -> 
                    case addClass classTable name functions of
                        Nothing -> do
                            putStrLn "ERROR CON LOS NOMBRES DADOS"
                            runMainLoop classTable
                        Just newClassTable ->
                            runMainLoop newClassTable

        "DESCRIBIR":name:[] -> do
            case describe classTable name of
                Nothing -> do
                    putStrLn "EL NOMBRE DADO NO EXISTE"
                    runMainLoop classTable
                Just fs -> do
                    putStrLn $ unlines fs
                    runMainLoop classTable

        "SALIR":tail -> 
            return()
        _ -> do
            putStrLn "COMANDO DESCONOCIDO"
            runMainLoop classTable

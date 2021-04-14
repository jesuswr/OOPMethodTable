module Main where

import Test.HUnit
import MethodTable
import qualified Data.Map as Map
import qualified Data.List as List

main = do runTestTT tests

tests = TestList [
            test1,
            test2,
            test3,
            test4,
            test5,
            test6
        ]



-- Tablas a usar
basicTable :: ClassTable
basicTable = table
    where
        Just table = addClass Map.empty "A" ["f", "g"]
basicTable2 :: ClassTable
basicTable2 = table
    where
        Just table = addClass basicTable "B" ["f", "g"]
basicTable3 :: ClassTable
basicTable3 = table
    where
        Just table = addSubclass basicTable "SubA" "A" ["g", "h"]


-- Primero algunas pruebas de errores
-- No insertar una clase que ya esta
test1 = 
    TestCase (assertEqual "ERROR TEST 1" Nothing (addClass basicTable "A" []))
-- No usar una super clase que no existe
test2 = 
    TestCase (assertEqual "ERROR TEST 2" Nothing (addSubclass basicTable "C" "B" []))
-- No insertas una subclase que ya existe
test3 = 
    TestCase (assertEqual "ERROR TEST 3" Nothing (addSubclass basicTable2 "A" "B" []))
-- No describir una clase que no existe
test4 =
    TestCase (assertEqual "ERROR TEST 4" Nothing (describe basicTable "B"))


-- Ahora otras pruebas
-- A deberia tener A::f y A::g
test5 =
    TestCase (assertEqual "ERROR TEST 5" (Just ["f -> A :: f", "g -> A :: g"]) (describe basicTable "A"))
-- SubA deberia tener A::f, SubA::g y SubA::h
test6 =
    TestCase (assertEqual "ERROR TEST 6" (Just ["f -> A :: f", "g -> SubA :: g", "h -> SubA :: h"]) (describe basicTable3 "SubA"))
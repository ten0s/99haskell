module FifteenPuzzleTest where

import Test.HUnit
import Control.Monad

import FifteenPuzzle

runTests = runTestTT $ 
           TestList [ "buildBoard" ~: testBuildBoard
                    , "isBoardSolvable" ~: testIsBoardSolvable
                    ]
  
testBuildBoard = TestList [ "Fail #1" ~:
                            Left InvalidBoardSide ~=? buildBoard 1 []
                          , "Fail #2" ~:  
                            Left InvalidFieldLength ~=? buildBoard 2 [1,2,3,4,0]
                          , "Fail #3" ~:  
                            Left MissingEmptyTile ~=? buildBoard 2 [1,2,3,4]
                          ]  
           
testIsBoardSolvable = TestList [ "Solvable #1" ~: 
                                 Right True ~=? (buildBoard 3 [1,2,3,4,5,6,7,8,0] >>= return.(isBoardSolvable 3))
                               , "Solvable #2" ~:  
                                 Right True ~=? (buildBoard 3 [0,8,7,6,5,4,3,2,1] >>= return.(isBoardSolvable 3))
                               , "Solvable #3" ~:  
                                 Right True ~=? (buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0] >>= return.(isBoardSolvable 4))
                               , "Solvable #4" ~:  
                                 Right True ~=? (buildBoard 4 [4,2,1,14,13,11,3,6,9,0,10,5,8,7,15,12] >>= return.(isBoardSolvable 4))
                               ,  "Solvable #5" ~:
                                 Right True ~=? (buildBoard 4  [1,2,3,4,5,6,7,8,9,10,11,12,0,13,14,15]>>= return.(isBoardSolvable 4))
                               , "Not solvable #1" ~:
                                 Right False ~=? (buildBoard 3 [1,2,3,4,5,6,8,7,0] >>= return.(isBoardSolvable 3))
                               , "Not solvable #2" ~:
                                 Right False ~=? (buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,15,14,0] >>= return.(isBoardSolvable 4))
                               ]

                      
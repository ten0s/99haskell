module TowerOfHanoi (
    moveTower
) where

import ListAux (replaceAt)

--
-- Tower of Hanoi Puzzle
--
-- Example: moveTower 0 2 ([1,2,3,4,5], [], [])
--

moveTower :: Int -> Int -> ([a],[a],[a]) -> ([a],[a],[a])
moveTower from to (a,b,c)
    | 0 <= from && from <= 3 && 0 <= to && to <= 3 && from /= to
      = let state = [a,b,c]
            [a',b',c'] = move (length $ state !! from) from to state
        in (a',b',c')
    | otherwise = error "moveTower: invalid index"

move :: Int -> Int -> Int -> [[a]] -> [[a]]
move 0 _ _ state = state

move 1 from to state = let (disk, state') = takeTopFrom from state
                       in putTopTo disk to state'
                         
move count from to state = 
    let state'   = move (count - 1) from (sparePeg from to) state
        state''  = move 1 from to state'
        state''' = move (count - 1) (sparePeg from to) to state''
    in state'''

sparePeg p1 p2 = (0 + 1 + 2) - p1 - p2

takeTopFrom :: Int -> [[a]] -> (a, [[a]])
takeTopFrom index state = let stack = state !! index
                              car = head stack
                              cdr = tail stack
                          in (car, replaceAt cdr state index)

putTopTo :: a -> Int -> [[a]] -> [[a]]
putTopTo elem index state = let stack = state !! index
                                stack' = elem : stack
                            in replaceAt stack' state index
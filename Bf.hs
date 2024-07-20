import Data.Char (ord, chr)
import Control.Monad (when)
import System.Environment (getArgs)

data Memory = Memory{ cells :: [Int]
                    , ip :: Int
                    , mp :: Int
                    }

changeAt :: [a] -> Int -> a -> [a]
changeAt xs n x | n == 0 = x : tail xs
                | otherwise = head xs : changeAt (tail xs) (n - 1) x

pop :: [a] -> (a, [a])
pop xs = (last xs, init xs)

getLoops :: [(Char, Int)] -> [Int] -> [(Int, Int)]
getLoops [] _ = []
getLoops ((instr, i): xs) stack | instr == '[' = getLoops xs (stack ++ [i])
                                | instr == ']' = let (openi, newStack) = pop stack in (i, openi) : (openi, i) : getLoops xs newStack

getLoop :: Int -> [(Int, Int)] -> Int
getLoop n [] = error "Key not in list"
getLoop n ((k,v):rest) | n == v = k
                       | otherwise = getLoop n rest

bfRun :: String -> IO()
bfRun str = bfRunHelper str (Memory [0 | _ <- [0..30_000]] 0 0)
    where
        loopPositions = getLoops (filter (\(x, _) -> x == '[' || x == ']') $ zip str [0..]) []
        bfRunHelper :: String -> Memory -> IO()
        bfRunHelper instrs (Memory cells ip mp) | ip < length instrs = case instr of
            '>' -> bfRunHelper instrs (Memory cells (ip + 1) (mp + 1))
            '<' -> bfRunHelper instrs (Memory cells (ip + 1) (mp - 1))
            '+' -> bfRunHelper instrs (Memory (changeAt cells mp (cells !! mp + 1)) (ip + 1) mp)
            '-' -> bfRunHelper instrs (Memory (changeAt cells mp (cells !! mp - 1)) (ip + 1) mp)
            '[' -> if cells !! mp == 0
                then bfRunHelper instrs (Memory cells (getLoop ip loopPositions + 1) mp)
                else bfRunHelper instrs (Memory cells (ip + 1) mp)
            ']' -> if cells !! mp /= 0
                then bfRunHelper instrs (Memory cells (getLoop ip loopPositions + 1) mp)
                else bfRunHelper instrs (Memory cells (ip + 1) mp)
            '.' -> do
                putChar $ chr (cells !! mp)
                bfRunHelper instrs (Memory cells (ip + 1) mp)
            ',' -> do
                ch <- getChar
                bfRunHelper instrs (Memory (changeAt cells mp (ord ch)) (ip + 1) mp)
            _   -> bfRunHelper instrs (Memory cells (ip + 1) mp)
                                               | otherwise = return ()
            where
                instr = instrs !! ip

bfRepl :: IO()
bfRepl = do
    putStr "> "

    bfExpr <- getLine
    bfRun bfExpr

    bfRepl

main :: IO()
main = do
    args <- getArgs

    if null args
    then bfRepl
    else do
        text <- readFile (head args)
        bfRun text

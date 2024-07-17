import Data.List

data Tree a = None | Node (Maybe a) Int (Tree a) (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
    show None = ""
    show (Node Nothing f l r) = "T(" ++ show f ++ ": L" ++ show l ++ ", R" ++ show r ++ ")"
    show (Node (Just v) f _ _) = "N(" ++ show v ++ ": "++ show f ++ ")"

testD = Node (Just 'd') 6 None None
testE = Node (Just 'e') 5 None None

getElemFreq :: Ord a => [a] -> [(Maybe a, Int)]
getElemFreq xs = map (\x -> (Just (head x), length x)) $ group $ sort xs

connectTrees :: Tree a -> Tree a -> Tree a
connectTrees None _ = error "Connecting empty node"
connectTrees _ None = error "Connecting empty node"
connectTrees x@(Node _ f1 _ _) y@(Node _ f2 _ _) | f1 > f2 = Node Nothing (f1 + f2) x y
                                                 | f1 <= f2 = Node Nothing (f1 + f2) y x

sortLT :: Tree a -> Tree a -> Ordering
sortLT x@(Node _ f1 _ _) y@(Node _ f2 _ _) | f1 > f2 = LT
                                           | f1 <= f2 = GT

lastTwo :: [a] -> (a, a)
lastTwo [a,b] = (a,b)
lastTwo (a:b:xs) = lastTwo (b:xs)

leastCommon :: [Tree a] -> ([Tree a], (Tree a, Tree a))
leastCommon xs = (rest, lastTwo xs)
    where
        rest = take (length xs - 2) xs

huffman :: Ord a => [a] -> Tree a
huffman lst = huffmanIter $ map (\(el, freq) -> Node el freq None None) $ getElemFreq lst
    where
        huffmanIter :: [Tree a] -> Tree a
        huffmanIter xs | length xs == 1 = head xs
                       | otherwise = huffmanIter $ nextStep xs

        nextStep :: [Tree a] -> [Tree a]
        nextStep xs = xs'
            where
                (rest, (a, b)) = leastCommon $ sortBy sortLT xs
                xs' = sortBy sortLT $ connectTrees a b : rest

huffmanTable :: Ord a => [a] -> [(a, String)]
huffmanTable lst = walkTree (huffman lst) ""
    where
        walkTree :: Tree a -> String -> [(a, String)]
        walkTree (Node Nothing _ l r) acc = walkTree l (acc++"1") ++ walkTree r (acc++"0")
        walkTree (Node (Just v) _ _ _) acc = [(v, acc)]

huffmanEncode :: (Eq a, Ord a) => [a] -> String
huffmanEncode lst = concatMap (\x -> findCode x huffTable) lst
    where
        huffTable = huffmanTable lst
        findCode x table = if fst curr == x then snd curr else findCode x rest
            where
                curr = head table
                rest = tail table

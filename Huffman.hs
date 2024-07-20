import Data.List

data Tree a = None | Node (Maybe a) Int (Tree a) (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
    show None = ""
    show (Node Nothing f l r) = "T(" ++ show f ++ ": L" ++ show l ++ ", R" ++ show r ++ ")"
    show (Node (Just v) f _ _) = "N(" ++ show v ++ ": "++ show f ++ ")"

getElemFreq :: Ord a => [a] -> [(Maybe a, Int)]
getElemFreq xs = map (\x -> (Just (head x), length x)) $ group $ sort xs

connectTrees :: Tree a -> Tree a -> Tree a
connectTrees None _ = error "Connecting empty node"
connectTrees _ None = error "Connecting empty node"
connectTrees x@(Node _ f1 _ _) y@(Node _ f2 _ _) | f1 <= f2 = Node Nothing (f1 + f2) x y
                                                 | f1 > f2 = Node Nothing (f1 + f2) y x

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

newtype HuffmanTable a = HuffmanTable [(a, String)]

binSort :: (Ord a) => (a, String) -> (a, String) -> Ordering
binSort x y | binX == binY = compare vX vY
            | length binX == length binY = compare binX binY
            | otherwise = compare (length binX) (length binY)
    where
        binX = snd x
        binY = snd y
        vX = fst x
        vY = fst y

instance (Show a, Ord a) => Show (HuffmanTable a) where
    show (HuffmanTable xs) = show $ sortBy binSort xs

connectHT :: HuffmanTable a -> HuffmanTable a -> HuffmanTable a
connectHT (HuffmanTable xs) (HuffmanTable ys) = HuffmanTable (xs ++ ys)

huffmanTable :: Ord a => [a] -> HuffmanTable a
huffmanTable lst = walkTree (huffman lst) ""
    where
        walkTree :: Tree a -> String -> HuffmanTable a
        walkTree (Node Nothing _ l r) acc = walkTree l (acc++"0") `connectHT` walkTree r (acc++"1")
        walkTree (Node (Just v) _ _ _) acc = HuffmanTable [(v, acc)]

huffmanEncode :: (Eq a, Ord a) => [a] -> String
huffmanEncode lst = concatMap (`findCode` huffTable) lst
    where
        huffTable = huffmanTable lst
        findCode :: Eq a => a -> HuffmanTable a -> String
        findCode x (HuffmanTable table) = if fst curr == x then snd curr else findCode x rest
            where
                curr = head table
                rest = HuffmanTable $ tail table

huffmanDecode :: String -> HuffmanTable a -> [a]
huffmanDecode bin ht | rest == "" = [decoded]
                     | otherwise = decoded : huffmanDecode rest ht
    where
        (decoded, rest) = huffmanGetNext bin 0 ht
        huffmanGetNext :: String -> Int -> HuffmanTable a -> (a, String)
        huffmanGetNext bin n ht@(HuffmanTable table) | length matches == 1 = (fst match, drop n bin)
                                                     | otherwise = huffmanGetNext bin (n+1) ht
            where
                matches = filter (\x -> snd x == take n bin) table
                match = head matches

import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import System.Environment (getArgs)

data Expr = Val  Int
          | Atom String
          | List [Expr]

data OpType = BinaryOp (Int -> Int -> Int)
            | UnaryOp  (Int -> Int)

instance Show Expr where
    show (Val x) = show x
    show (Atom s) = show s
    show (List xs) = show xs

funcs :: [(String, OpType)]
funcs = [ ("+", BinaryOp (+))
        , ("-", BinaryOp (-))
        , ("*", BinaryOp (*))
        , ("/", BinaryOp div)
        , ("%", BinaryOp mod)
        , ("add1", UnaryOp (+1))
        ]

getVal :: Expr -> Int
getVal (Val x) = x
getVal _ = error "unreachable code"

apply :: OpType -> [Expr] -> Expr
apply (BinaryOp f) args = Val $ foldl1 f (map (getVal . eval) args)
apply (UnaryOp  f) args =
    if length args == 1
        then Val $ f $ (getVal . eval . head) args
        else error "Unary operation applied to multiple arguments"

lookupOrCrash :: (Show a, Eq a) => a -> [(a, b)] -> b
lookupOrCrash a xs = let res = lookup a xs in case res of
    Just x  -> x
    Nothing -> error $ "Unknown symbol: " ++ show a


eval :: Expr -> Expr
eval v@(Val _) = v
eval (List ((Atom "list") : args)) = List args
eval (List ((Atom f) : args)) = apply (lookupOrCrash f funcs) args
eval x = error $ "Cannot evaluate expression: " ++ show x


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser Expr
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseInt :: Parser Expr
parseInt = do
    x <- many1 digit
    (return . Val . read) x

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser Expr
parseList = List <$> sepBy parseExpr spaces

parseExpr :: Parser Expr
parseExpr = try parseAtom
    <|> try parseInt
    <|> do
        _ <- char '('
        x <- try parseList
        _ <- char ')'
        return x

run :: String -> Expr
run input = case parse parseExpr "s-exp" input of
    Right parsed -> eval parsed
    Left err -> error $ show err

main :: IO()
main = do
    hSetBuffering stdout NoBuffering

    args <- getArgs

    let loop = do putStr "expr> "
                  input <- getLine
                  if input == "quit"
                  then return ()
                  else do print $ run input
                          loop

    case args of
        [] -> loop
        [expr] -> print $ run expr
        _ -> error "Unknown paramaters"

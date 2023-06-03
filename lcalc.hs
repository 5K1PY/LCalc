import Data.Char
import Debug.Trace

debug = flip trace
-- Utilities
contains :: Eq a => a -> [a] -> Bool
contains x [] = False
contains x (y:rs) 
    | x == y    = True
    | otherwise = contains x rs

-- Tokenization
data LToken
    = LNamed String |
      LLambda | LDot |
      LOpen | LClose
    deriving Show

isSpecial x = contains x "()\\. "
isNotSpecial = not . isSpecial

tokenize :: String -> [LToken]
tokenize [] = []
tokenize ('(':rs) = LOpen : tokenize rs
tokenize (')':rs) = LClose : tokenize rs
tokenize ('\\':rs) = LLambda : tokenize rs
tokenize ('.':rs) = LDot : tokenize rs
tokenize (' ':rs) = tokenize rs
tokenize r@(r0:rs)
    | nextToken == "" = error ("Unknown symbol " ++ [r0])
    | otherwise = (LNamed nextToken):(tokenize $ dropWhile isNotSpecial r)
    where
        nextToken = takeWhile isNotSpecial r

-- Building AST
data LObject = 
    LVar String |
    LFunc String [LObject] |
    LObjList [LObject]
  deriving Show

build :: [LToken] -> ([LObject], [LToken])
build [] = ([], [])
build d@(LClose:rs) = ([], rs)
build d@(LOpen:rs)   
    | length inside == 1 = ((head inside):same, up)
    | otherwise          = ((LObjList inside):same, up)
    where
        (inside, rest) = build rs
        (same, up) = build rest

build d@((LNamed x):rs) = ((LVar x):same, rest)
    where
        (same, rest) = build rs

build d@(LLambda:(LNamed x):LDot:rs) = ((LFunc x inside):same, up)
    where
        (inside, rest) = build rs
        (same, up) = build rest

build d = error "Error while parsing"

parse :: String -> [LObject]
parse x = fst $ build $ tokenize x

-- Displaying
display :: [LObject] -> String
display [] = ""
display ((LVar x):(LVar y):rs) = x ++ " " ++ (display ((LVar y):rs))
display ((LVar x):rs) = x ++ (display rs)
display ((LFunc x y):rs) = "(\\" ++ x ++ "." ++ (display y) ++ ")" ++ (display rs)
display ((LObjList x):rs) = "(" ++ (display x) ++ ")" ++ (display rs)

number :: String -> (String, String)
number = number_func 1
number_func :: Int -> String -> (String, String)
number_func i [] = ("", "")
number_func i ('\\':rs) = ('\\':exp, (show i)++" "++nums) -- TODO: Fix this for number bigger than 9
    where
        (exp, nums) = number_func (i+1) (rs)

number_func i (x:rs) = (x:exp, ' ':nums)
    where 
        (exp, nums) = number_func i (rs)

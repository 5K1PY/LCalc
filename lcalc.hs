import Data.Char
import Debug.Trace

debug = flip trace
-- Utilities
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

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
-- TODO: Unmatched parenthesis
data LObject = 
    LVar String |
    LFunc String [LObject] |
    LObjList [LObject]
  deriving Show

is_func :: LObject -> Bool
is_func (LFunc _ _) = True
is_func _ = False

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

data LTObject = 
    LTVar String |
    LTFunc String [LTObject] Bool |
    LTObjList [LTObject] |
    Highlighted LTObject
  deriving Show

is_callable :: LTObject -> Bool
is_callable (LTFunc _ _ True) = True
is_callable _ = False

addTags :: [LObject] -> [LTObject]
addTags [] = []
addTags ((LVar x):rs) = (LTVar x):(addTags rs)
addTags ((LFunc a x):rs) = (LTFunc a (addTags x) (length rs /= 0)):(addTags rs)
addTags ((LObjList x):rs) = (LTObjList (addTags x)):(addTags rs)

parse :: String -> [LTObject]
parse x = addTags $ fst $ build $ tokenize x

-- Evaluating
cnt :: (LObject -> Bool) -> [LObject] -> Int
cnt _ [] = 0
cnt f (r0@(LVar x):rs) = (boolToInt $ f r0) + (cnt f rs)
cnt f (r0@(LFunc x y):rs) = (boolToInt $ f r0) + (cnt f y) + (cnt f rs)
cnt f (r0@(LObjList x):rs) = (boolToInt $ f r0) + (cnt f x) + (cnt f rs)

{-
replace :: (LObject -> Bool) -> [LObject] -> Int -> LObject -> [LObject]
replace _ [] _ _ = []
replace _ r0:r 0 obj = obj:r
replace f (r0@(LVar x):rs) i obj = r0:(replace f rs (i-(boolToInt $ f r0)) obj)
replace f (r0@(LFunc x y):rs) i obj
    | i < c = (LFunc x (replace f y (i-(boolToInt $ f r0)) obj)):rs

    where
        c = cnt f y
cnt f (r0@(LFunc x y):rs) = (boolToInt $ f r0) + (cnt f y) + (cnt f rs)
cnt f (r0@(LObjList x):rs) = (boolToInt $ f r0) + (cnt f x) + (cnt f rs)
-}

{- 
evaluate :: [LObject] -> [LObject]
evaluate (LFunc name body):arg:rs = (replace body name arg):rs
evaluate (LFunc _ _):[] = error("No function argument.")
evaluate _ = error("No function to evaluate.")

replace :: [LObject] -> String -> LObject -> [LObject]
replace [] name val = []
replace (LVar lname):rs name val = new:(replace rs name val)
    where
        new = case lname of
            name -> val
            _ -> LVar lname
replace (LObjList x):rs name val = (LObjList $ replace x name val):(replace rs name val)
replace (LFunc a body):rs name val
 | a == name = (LObjList $ replace x name val):(replace rs name val)
 | otherwise = 

evaluate_ith :: [LObject] -> Int -> (Int, [LObject])
evaluate_ith _ [] = error("No function with such number.")
evaluate_ith i (LVar x):rs = evaluate_ith i rs
evaluate_ith i (LObjList x):rs
 | i < c     = evaluate_ith i x
 | otherwise = evaluate_ith (i-c) rs
    where
        c = cnt_func x
evaluate_ith i lo@(LFunc arg x):rs
 | i == 0    = evaluate lo
 | i-1 < c   = evaluate_ith (i-1) x
 | otherwise = evaluate_ith (i-c-1) rs
    where
        c = cnt_func x
-}

-- Displaying
display :: [LTObject] -> String
display [] = ""
display ((LTVar x):(LTVar y):rs) = x ++ " " ++ (display ((LTVar y):rs))
display ((LTVar x):rs) = x ++ (display rs)
display ((LTFunc x y _):rs) = "(\\" ++ x ++ "." ++ (display y) ++ ")" ++ (display rs)
display ((LTObjList x):rs) = "(" ++ (display x) ++ ")" ++ (display rs)
display (Highlighted x:rs) = "[" ++ (display [x]) ++ "]" ++ (display rs)


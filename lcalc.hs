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
    LTFunc String [LTObject] |
    LTObjList [LTObject] |
    Highlighted LTObject
  deriving Show

callable :: [LTObject] -> Bool
callable ((LTFunc _ _):_:rs) = True
callable _ = False

addTags :: [LObject] -> [LTObject]
addTags [] = []
addTags ((LVar x):rs) = (LTVar x):(addTags rs)
addTags ((LFunc a x):rs) = (LTFunc a (addTags x)):(addTags rs)
addTags ((LObjList x):rs) = (LTObjList (addTags x)):(addTags rs)

parse :: String -> [LTObject]
parse x = addTags $ fst $ build $ tokenize x


-- Evaluating
cnt :: ([LTObject] -> Bool) -> [LTObject] -> Int
cnt _ [] = 0
cnt f r@((LTVar x):rs) = (boolToInt $ f r) + (cnt f rs)
cnt f r@((LTFunc x y):rs) = (boolToInt $ f r) + (cnt f y) + (cnt f rs)
cnt f r@((LTObjList x):rs) = (boolToInt $ f r) + (cnt f x) + (cnt f rs)
cnt f r@((Highlighted x):rs) = (boolToInt $ f r) + (cnt f [x]) + (cnt f rs)


call :: LTObject -> LTObject -> LTObject
call (LTFunc a x) obj = LTObjList (replaceVar x a obj)
call fn _ = error((display [fn]) ++ " cannot be called")

mapVar :: [LTObject] -> String -> (LTObject -> LTObject) -> [LTObject]
mapVar [] name fn = []
mapVar (r0@(LTVar x):rs) name fn
    | name == x = (fn r0):(mapVar rs name fn)
    | otherwise = r0:(mapVar rs name fn)

mapVar (r0@(LTFunc a x):rs) name fn
    | a /= name = (LTFunc a mapped):(mapVar rs name fn)
    | otherwise = r0:(mapVar rs name fn)
    where
        mapped = mapVar x name fn

mapVar ((LTObjList x):rs) name fn = (LTObjList mapped):(mapVar rs name fn)
    where
        mapped = mapVar x name fn

mapVar ((Highlighted x):rs) name fn = (Highlighted mapped):(mapVar rs name fn)
    where
        mapped = head $ mapVar [x] name fn

replaceVar :: [LTObject] -> String -> LTObject -> [LTObject]
replaceVar r name obj = mapVar r name (\x -> obj)

-- Displaying
display :: [LTObject] -> String
display [] = ""
display ((LTVar x):(LTVar y):rs) = x ++ " " ++ (display ((LTVar y):rs))
display ((LTVar x):rs) = x ++ (display rs)
display ((LTFunc x y):rs) = "(\\" ++ x ++ "." ++ (display y) ++ ")" ++ (display rs)
display ((LTObjList x):rs) = "(" ++ (display x) ++ ")" ++ (display rs)
display (Highlighted x:rs) = "[" ++ (display [x]) ++ "]" ++ (display rs)

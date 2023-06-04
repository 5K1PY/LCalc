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

dropTail :: Show a => Int -> [a] -> [a]
dropTail x l = take (length l - x) l

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
tokenize r@(r0:rs) = (LNamed nextToken):(tokenize $ dropWhile isNotSpecial r)
    where
        nextToken = takeWhile isNotSpecial r

-- Building AST
-- TODO: Unmatched parenthesis
data LObject = 
    LVar String |
    LFunc String [LObject] |
    LObjList [LObject]
  deriving Show

build :: [LToken] -> ([LObject], [LToken])
build [] = ([], [])
build d@((LNamed x):rs) = ((LVar x):same, rest)
    where
        (same, rest) = build rs

build d@(LOpen:LLambda:(LNamed x):LDot:rs) = ((LFunc x inside):same, up)
    where
        (inside, rest) = build rs
        (same, up) = build rest

build d@(LClose:rs) = ([], rs)

build d@(LOpen:rs) = ((LObjList inside):same, up)
    where
        (inside, rest) = build rs
        (same, up) = build rest

build d = error "Error while parsing"

data Highlight = Base | Argument | Full deriving (Eq, Show)
data LTObject = 
    LTVar String Highlight |
    LTFunc String LTObject Highlight |
    LTObjList [LTObject] Highlight
  deriving Show

is_func :: LTObject -> Bool
is_func (LTFunc _ _ _) = True
is_func _ = False

callable :: LTObject -> Bool
callable (LTObjList ((LTFunc _ _ _):arg:args) _) = True
callable _ = False

addTags :: LObject -> LTObject
addTags (LVar x) = (LTVar x Base)
addTags (LFunc a x) = (LTFunc a (LTObjList (map addTags x) Base) Base)
addTags (LObjList x) = (LTObjList (map addTags x) Base)

parse :: String -> LTObject
parse x = LTObjList (map addTags (fst $ build $ tokenize x)) Base

-- Evaluating
mapLT :: (LTObject -> (LTObject, Bool)) -> LTObject -> LTObject
mapLT f obj
    | apply     = subMapLT f new_obj
    | otherwise = new_obj
    where
        (new_obj, apply) = f obj

subMapLT :: (LTObject -> (LTObject, Bool)) -> LTObject -> LTObject
subMapLT f (LTFunc a x h) = LTFunc a (mapLT f x) h
subMapLT f (LTObjList x h) = LTObjList (map (mapLT f) x) h
subMapLT f obj = obj

unpack_one :: LTObject -> (LTObject, Bool)
unpack_one (LTObjList (x:[]) h) = (x, True)
unpack_one obj = (obj, True)

unpack :: LTObject -> LTObject
unpack = mapLT unpack_one

set_highlight :: Highlight -> LTObject -> (LTObject, Bool)
set_highlight h (LTVar a _) = ((LTVar a h), True)
set_highlight h (LTObjList x _) = ((LTObjList x h), True)
set_highlight h (LTFunc a x _) = ((LTFunc a x h), True)

map_highlight :: Highlight -> LTObject -> LTObject
map_highlight h = mapLT (set_highlight h)

call :: LTObject -> LTObject
call (LTObjList ((LTFunc a x _):arg:rest) h) = (LTObjList (replaced:rest) h)
    where
        replaced = mapLT (replaceVar a (map_highlight Full arg)) x
call obj = error((display $ map_highlight Base $ obj) ++ " cannot be called")

highlight_call :: LTObject -> LTObject
highlight_call (LTObjList (f@(LTFunc a x h1):arg:rest) h2) =
    (LTObjList ((LTFunc a replaced Argument):(map_highlight Full arg):rest) h2)
    where
        replaced = mapLT (replaceVar a (LTVar a Argument)) x
highlight_call obj = error((display $ map_highlight Base $ obj) ++ " cannot be called")

replaceVar :: String -> LTObject -> LTObject -> (LTObject, Bool)
replaceVar name to obj@(LTVar a h)
    | a == name = (to, True)
    | otherwise = (obj, True)

replaceVar name to f@(LTFunc a _ _)
    | name == a = (f, False)
    | otherwise = (f, True)

replaceVar name to obj = (obj, True)

cnt :: (LTObject -> Bool) -> LTObject -> Int
cnt f r@(LTVar x h) = (boolToInt $ f r)
cnt f r@(LTFunc a x h) = (boolToInt $ f r) + (cnt f x)
cnt f r@(LTObjList x h) = (boolToInt $ f r) + sum (map (cnt f) x)

apply_ith :: (LTObject -> LTObject) -> Int -> LTObject -> LTObject
apply_ith f i obj = head $ apply_ith_callable f i [obj]

apply_ith_callable :: (LTObject -> LTObject) -> Int -> [LTObject] -> [LTObject]
apply_ith_callable _ _ [] = error("No such function.")
apply_ith_callable f i (r0@(LTVar a h):rs) = r0:(apply_ith_callable f i rs)
apply_ith_callable f i (r0@(LTFunc a x h):rs)
    | i < c     = (LTFunc a (apply_ith f i x) h):rs
    | otherwise = r0:(apply_ith_callable f (i-c) rs)
    where
        c = cnt callable r0
apply_ith_callable f 0 ((r0@(LTObjList ((LTFunc _ _ _):arg:args) h)):rs) = (f r0):rs
apply_ith_callable f i (r0@(LTObjList x h):rs)
    | i < c     = (LTObjList (apply_ith_callable f i x) h):rs
    | otherwise = r0:(apply_ith_callable f (i-c) rs)
    where
        c = cnt callable r0

-- Displaying
magenta = "\ESC[95m"
blue = "\ESC[96m"
green = "\ESC[92m"
reset = "\ESC[39;49m"

highlight :: Highlight -> String
highlight Argument = green
highlight Full = blue
highlight Base = reset

display = display_one False False Base

display_one :: Bool -> Bool -> Highlight -> LTObject -> String
display_one _ _ h0 (LTVar name h) = (highlight h) ++ name ++ (highlight h0) ++ " "
display_one cal _ h0 (LTFunc a x h) = (highlight full) ++ "(" ++ lambda ++ arg ++ "." ++ contents ++ ")" ++ (highlight h0) ++ " "
    where
        lambda = case cal of
            False -> "λ" 
            True -> (magenta) ++ "λ" ++ (highlight full)
        full = case h of
            Full -> Full
            _ -> Base
        arg = case h of
            Argument -> (highlight Argument) ++ a ++ (highlight Base) 
            _ -> (highlight h) ++ a
        contents = dropTail 1 $ display_one False False full x

display_one _ close h0 obj@(LTObjList x h) 
    | close     = (highlight h) ++ "(" ++ contents ++ ")" ++ (highlight h0) ++ " "
    | otherwise = (highlight h) ++ contents ++ (highlight h0) ++ " "
    where
        contents = dropTail 1 $ display_list True h x

display_list :: Bool -> Highlight -> [LTObject] -> String
display_list c h x = foldl (++) (display_one (length rest >= 1) c h first) (map (display_one False c h) rest)
    where
        first = head x
        rest = drop 1 x

-- Main loop
highlight_ith :: LTObject -> Int -> LTObject
highlight_ith exp i = apply_ith highlight_call i exp
call_ith :: LTObject -> Int -> LTObject
call_ith exp i = apply_ith call i exp

main :: IO()
main = do
    putStrLn "Enter lambda calculus expression:"
    exp <- getLine
    expressionInteract $ unpack $ parse exp
    main

-- TODO: Fix invalid user input
expressionInteract :: LTObject -> IO()
expressionInteract exp = do
    putStrLn $ display exp
    if (cnt callable exp) == 0 then do
        putStrLn "Expression is in normal form." 
    else do
        line <- getLine
        putStrLn $ display $ unpack $ highlight_ith exp2 (read line)
        expressionInteract (unpack $ call_ith exp2 (read line))
        where exp2 = map_highlight Base exp

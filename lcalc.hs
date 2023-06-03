import Data.Char
import Debug.Trace

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

is_func :: LObject -> Bool
is_func (LFunc _ _) = True
is_func _ = False

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
    LTFunc String [LTObject] Highlight |
    LTObjList [LTObject] Highlight
  deriving Show

callable :: [LTObject] -> Bool
callable ((LTFunc _ _ _):_:rs) = True
callable _ = False

addTags :: [LObject] -> [LTObject]
addTags [] = []
addTags ((LVar x):rs) = (LTVar x Base):(addTags rs)
addTags ((LFunc a x):rs) = (LTFunc a (addTags x) Base):(addTags rs)
addTags ((LObjList x):rs) = (LTObjList (addTags x) Base):(addTags rs)

parse :: String -> [LTObject]
parse x = addTags $ fst $ build $ tokenize x


-- Evaluating
unpack :: [LTObject] -> [LTObject]
unpack [] = []
unpack (r0@(LTVar _ _):rs) = r0:(unpack rs)
unpack ((LTObjList x h):rs)
    | length unp == 1 = (head unp):(unpack rs)
    | otherwise       = (LTObjList unp h):(unpack rs)
    where
        unp = unpack x
unpack ((LTFunc a x h):rs) = (LTFunc a (unpack x) h):(unpack rs)

set_highlight :: Highlight -> LTObject -> LTObject
set_highlight h (LTVar a _) = (LTVar a h)
set_highlight h (LTObjList x _) = (LTObjList (map (set_highlight h) x) h)
set_highlight h (LTFunc a x _) = (LTFunc a (map (set_highlight h) x) h)

unhighlight :: [LTObject] -> [LTObject]
unhighlight [] = []
unhighlight (r0@(LTVar a _):rs) = (LTVar a Base):(unhighlight rs)
unhighlight ((LTObjList x _):rs) = (LTObjList (unhighlight x) Base):(unhighlight rs)
unhighlight ((LTFunc a x _):rs) = (LTFunc a (unhighlight x) Base):(unhighlight rs)

cnt :: ([LTObject] -> Bool) -> [LTObject] -> Int
cnt _ [] = 0
cnt f r@((LTVar x _):rs) = (boolToInt $ f r) + (cnt f rs)
cnt f r@((LTFunc x y _):rs) = (boolToInt $ f r) + (cnt f y) + (cnt f rs)
cnt f r@((LTObjList x _):rs) = (boolToInt $ f r) + (cnt f x) + (cnt f rs)

call :: LTObject -> LTObject -> [LTObject]
call (LTFunc a x _) obj = [LTObjList (replaceVar x a (set_highlight Full obj)) Base]
call fn _ = error((display [fn]) ++ " cannot be called")

highlight_call :: LTObject -> LTObject -> [LTObject]
highlight_call (LTFunc a x h) obj = [LTFunc a (replaceVar x a (LTVar a Full)) Argument, (set_highlight Full obj)]
highlight_call fn _ = error((display [fn]) ++ " cannot be called")

mapVar :: [LTObject] -> String -> (LTObject -> LTObject) -> [LTObject]
mapVar [] name fn = []
mapVar (r0@(LTVar x h):rs) name fn
    | name == x = (fn r0):(mapVar rs name fn)
    | otherwise = r0:(mapVar rs name fn)

mapVar (r0@(LTFunc a x h):rs) name fn
    | a /= name = (LTFunc a mapped h):(mapVar rs name fn)
    | otherwise = r0:(mapVar rs name fn)
    where
        mapped = mapVar x name fn

mapVar ((LTObjList x h):rs) name fn = (LTObjList mapped h):(mapVar rs name fn)
    where
        mapped = mapVar x name fn

replaceVar :: [LTObject] -> String -> LTObject -> [LTObject]
replaceVar r name obj = mapVar r name (\x -> obj)

apply_ith_callable :: (LTObject -> LTObject -> [LTObject]) -> Int -> [LTObject] -> [LTObject]
apply_ith_callable _ _ [] = error("No such function.")
apply_ith_callable f i (r0@(LTVar x h):rs) = r0:(apply_ith_callable f i rs)
apply_ith_callable f i (r0@(LTFunc a x h):arg:rs)
    | i == 0    = (f r0 arg) ++ rs
    | i-1 < c   = (LTFunc a (apply_ith_callable f (i-1) x) h):rs
    | otherwise = r0:(apply_ith_callable f (i-1-c) rs)
    where
        c = cnt callable x
apply_ith_callable f i (r0@(LTFunc a x h):[]) = [LTFunc a (apply_ith_callable f i x) h]

apply_ith_callable f i (r0@(LTObjList x h):rs)
    | i < c     = (LTObjList (apply_ith_callable f i x) h):rs
    | otherwise = r0:(apply_ith_callable f (i-c) rs)
    where
        c = cnt callable x

-- Displaying
magenta = "\ESC[95m" 
green = "\ESC[92m"
reset = "\ESC[39;49m"

spaced :: [LTObject] -> String
spaced ((LTVar _ _):rs) = " "
spaced _ = ""

highlight :: String -> Highlight -> String
highlight s Full = green ++ s ++ reset
highlight s _ = s

display :: [LTObject] -> String
display [] = ""
display ((LTVar x h):rs) = (highlight x h) ++ (spaced rs) ++ (display rs)
display r@((LTFunc x y h):rs) =
    (highlight ("(" ++ lambda ++ arg ++ ".") h) ++ (display y) ++ (highlight (")") h) ++ (spaced rs) ++ (display rs)
    where
        lambda = case (callable r, h) of
            (True, Full) -> magenta ++ "λ" ++ green
            (True, _) -> magenta ++ "λ" ++ reset
            (False, _) -> "λ"
        arg = case h of
            Argument -> green ++ x ++ reset
            _ -> x
 
display ((LTObjList x h):rs) = (highlight ("(" ++ (display x) ++ ")") h) ++ (spaced rs) ++ (display rs)


-- Main loop
highlight_ith :: [LTObject] -> Int -> [LTObject]
highlight_ith exp i = apply_ith_callable highlight_call i exp
call_ith :: [LTObject] -> Int -> [LTObject]
call_ith exp i = apply_ith_callable call i exp

main :: IO()
main = do
    putStrLn "Enter lambda calculus expression:"
    exp <- getLine
    expressionInteract $ unpack $ parse exp
    main

-- TODO: Fix invalid user input
expressionInteract :: [LTObject] -> IO()
expressionInteract exp = do
    if (cnt callable exp) == 0 then do
        putStrLn "Expression is in normal form." 
    else do
        line <- getLine
        putStrLn $ display $ unpack $ highlight_ith exp (read line)
        putStrLn $ display $ unpack $ call_ith exp (read line)
        expressionInteract (unpack $ unhighlight $ call_ith exp (read line))

import Control.Exception
import Data.Char
import Data.List
import Debug.Trace

-- TODO: Remove debugging
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

-- Tokenizes input into tokens (Lambdas, dots, parenthesis, variables) 
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
data LObject = 
    LVar String |
    LFunc String [LObject] |
    LObjList [LObject]
  deriving Show

-- Build AST
build :: [LToken] -> LTObject
build tokens
    | length rest == 0 = LTObjList (map addTags parsed) Base
    | otherwise        = error "Unmatched parenthesis."
    where
        (parsed, rest) = _build 0 tokens

-- Builds AST from tokens to first unmatched closing parenthesis, returns rest
_build :: Int -> [LToken] -> ([LObject], [LToken])
_build 0 [] = ([], [])
_build _ [] = error "Unmatched parenthesis."
_build depth d@((LNamed x):rs) = ((LVar x):same, rest)
    where
        (same, rest) = _build depth rs

_build depth d@(LOpen:LLambda:(LNamed x):LDot:rs) = ((LFunc x inside):same, up)
    where
        (inside, rest) = _build (depth+1) rs
        (same, up) = _build depth rest

_build 0 d@(LClose:rs) = error "Unmatched parenthesis."
_build _ d@(LClose:rs) = ([], rs)

_build depth d@(LOpen:rs) = ((LObjList inside):same, up)
    where
        (inside, rest) = _build (depth+1) rs
        (same, up) = _build depth rest

_build _ _ = error "Invalid expression."

data Highlight = Base | Argument | Full deriving (Eq, Show)
data LTObject = 
    LTVar String Highlight |
    LTFunc String LTObject Highlight |
    LTObjList [LTObject] Highlight
  deriving (Eq, Show)

-- Helping functions
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
parse x = build $ tokenize x

-- Apply given function to object and replace it
-- If function returns True apply recursively on all subobjects
mapLT :: (LTObject -> (LTObject, Bool)) -> LTObject -> LTObject
mapLT f obj
    | apply     = subMapLT f new_obj
    | otherwise = new_obj
    where
        (new_obj, apply) = f obj

-- Like MapLT but only on subobjects
subMapLT :: (LTObject -> (LTObject, Bool)) -> LTObject -> LTObject
subMapLT f (LTFunc a x h) = LTFunc a (mapLT f x) h
subMapLT f (LTObjList x h) = LTObjList (map (mapLT f) x) h
subMapLT f obj = obj

-- Removes redundant parenthesis (=LTObjLists with one item) in object and all subobjects 
unpack_one :: LTObject -> (LTObject, Bool)
unpack_one (LTObjList (x:[]) h) = (fst $ unpack_one x, False)
unpack_one obj = (obj, True)

-- Removes redundant parenthesis
unpack :: LTObject -> LTObject
unpack = mapLT unpack_one

-- Sets a highlight of given object
set_highlight :: Highlight -> LTObject -> (LTObject, Bool)
set_highlight h (LTVar a _) = ((LTVar a h), True)
set_highlight h (LTObjList x _) = ((LTObjList x h), True)
set_highlight h (LTFunc a x _) = ((LTFunc a x h), True)

-- Highlights object and all subobjects
map_highlight :: Highlight -> LTObject -> LTObject
map_highlight h = mapLT (set_highlight h)


-- Make name from string and int
name :: String -> Int -> String
name x i = x ++ (show i)

-- Remove ending number from name
remove_num :: String -> String
remove_num = reverse . dropWhile isNumber . reverse

-- Find a reasonable new name that in given list
free_name :: String -> [String] -> String
free_name n taken = head [new_name | i <- [1..], let new_name = (name (remove_num n) i), not $ contains new_name taken]

-- Get all arguments in given object
collect_args :: LTObject -> [String]
collect_args (LTVar _ _) = []
collect_args (LTFunc a x _) = a:(collect_args x)
collect_args (LTObjList x _) = foldr (++) [] (map collect_args x)

-- Get all free variables in given object
collect_free_vars :: [String] -> LTObject -> [String]
collect_free_vars bound (LTVar name _)
    | contains name bound = []
    | otherwise = [name]
collect_free_vars bound (LTFunc arg body _) = collect_free_vars (arg:bound) body
collect_free_vars bound (LTObjList body _) = foldr (++) [] (map (collect_free_vars bound) body)

-- Do alpha-conversion if necessary
alpha :: LTObject -> LTObject
alpha obj@(LTObjList (f@(LTFunc a x _):arg:rest) h)
    | length same == 0 = obj
    | otherwise        = (LTObjList (replaced:arg:rest) Argument)
    where
        same = intersect (collect_args f) (collect_free_vars [] arg)
        name = free_name (head same) (collect_free_vars [] f ++ collect_args f)
        replaced = (mapLT (renameArg (head same) name) f)
alpha obj = error((display $ map_highlight Base $ obj) ++ " cannot be called")

-- Repeatedly apply alpha-conversion until necessary
alphaRep :: LTObject -> Int -> LTObject
alphaRep obj i
    | obj == reduced = obj
    | otherwise      = alphaRep reduced i
    where
        reduced = apply_ith alpha i obj

-- Rename argument in object and all subobjects
renameArg :: String -> String -> LTObject -> (LTObject, Bool)
renameArg name new_name f@(LTFunc a body _)
    | name == a = ((LTFunc new_name content Argument), True)
    | otherwise = (f, True)
    where
        content = mapLT (replaceVar name (LTVar new_name Argument)) body

renameArg _ _ obj = (obj, True)


-- Do beta-reduction
beta :: LTObject -> LTObject
beta (LTObjList ((LTFunc a x _):arg:rest) h) = (LTObjList (replaced:rest) h)
    where
        replaced = mapLT (replaceVar a (map_highlight Full arg)) x
beta obj = error((display $ map_highlight Base $ obj) ++ " cannot be called")

-- Highlight function how it will be beta-reduced
highlight_beta :: LTObject -> LTObject
highlight_beta (LTObjList (f@(LTFunc a x h1):arg:rest) h2) =
    (LTObjList ((LTFunc a replaced Argument):(map_highlight Full arg):rest) h2)
    where
        replaced = mapLT (replaceVar a (LTVar a Argument)) x
highlight_beta obj = error((display $ map_highlight Base $ obj) ++ " cannot be called")

-- Replace all occurrences of a variable in given object with its
-- (ignore LTFunc with overshadowing arguments) 
replaceVar :: String -> LTObject -> LTObject -> (LTObject, Bool)
replaceVar name to obj@(LTVar a h)
    | a == name = (to, False)
    | otherwise = (obj, True)

replaceVar name to f@(LTFunc a _ _)
    | name == a = (f, False)
    | otherwise = (f, True)

replaceVar name to obj = (obj, True)

-- Count how many objects satisfying given function are in LTObject 
cnt :: (LTObject -> Bool) -> LTObject -> Int
cnt f r@(LTVar x h) = (boolToInt $ f r)
cnt f r@(LTFunc a x h) = (boolToInt $ f r) + (cnt f x)
cnt f r@(LTObjList x h) = (boolToInt $ f r) + sum (map (cnt f) x)

apply_ith :: (LTObject -> LTObject) -> Int -> LTObject -> LTObject
apply_ith f i obj = head $ apply_ith_callable f i [obj]

-- Apply given function to LTObjList with i-th callable LTFunc and replace the LTObjList it with result
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
    | j < c     = (LTObjList (apply_ith_callable f j x) h):rs
    | otherwise = r0:(apply_ith_callable f (j-c) rs)
    where
        c = cnt callable r0
        j = i - (boolToInt $ callable r0)

-- Displaying
magenta = "\ESC[95m"
blue = "\ESC[96m"
green = "\ESC[92m"
reset = "\ESC[39;49m"

-- Returns color of highlight
highlight :: Highlight -> String
highlight Argument = green
highlight Full = blue
highlight Base = reset

-- Displays given object
display = display_one False False Base

{-
Displays given object based on its relative position
Args:
    - is current obj callable
    - is content of LTList (for putting in parenthesis)
    - higher level highlight
    - object to display
-}
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
        contents = dropTail 1 $ display_list h x

display_list :: Highlight -> [LTObject] -> String
display_list _ [] = ""
display_list h x = foldl (++) (display_one (length rest >= 1) True h first) (map (display_one False True h) rest)
    where
        first = head x
        rest = drop 1 x

-- Main loop
highlight_ith :: LTObject -> Int -> LTObject
highlight_ith exp i = apply_ith highlight_beta i exp
call_ith :: LTObject -> Int -> LTObject
call_ith exp i = apply_ith beta i exp

showException :: SomeException -> IO()
showException ex = putStrLn (takeWhile (/= '\n') $ show ex)

main :: IO()
main = do
    putStrLn "Enter lambda calculus expression:"
    exp <- getLine
    catch (expressionInteract (unpack $ parse exp) "") showException
    main

-- TODO: Fix invalid user input
-- TODO: Exiting
expressionInteract :: LTObject -> String -> IO()
expressionInteract exp msg = do
    putStrLn $ (display exp) ++ msg
    if callables == 0 then do
        putStrLn "Expression is in normal form." 
    else do
        putStr $ "Choose callable to evaluate (0-" ++ (show (callables-1)) ++ "):" 
        line <- getLine
        let i = read line
        alphaReduceIO unh_exp i
        let rep_exp = map_highlight Base (alphaRep unh_exp i)
        putStrLn $ display $ unpack $ highlight_ith rep_exp i
        expressionInteract (unpack $ call_ith rep_exp i) "\t(β-reduction)"
        where
            callables = cnt callable exp
            unh_exp = map_highlight Base exp

alphaReduceIO :: LTObject -> Int -> IO()
alphaReduceIO exp i = do
    if (exp == exp2) then do
        return ()
    else do
        putStrLn $ (display $ unpack exp2) ++ "\t(α-converison)"
        (alphaReduceIO (map_highlight Base exp2) i)
    where
        exp2 = apply_ith alpha i exp

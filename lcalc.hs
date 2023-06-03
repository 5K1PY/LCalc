import Data.Char

data LNamed = LName String
data LToken
    = LNamed String |
      LLambda | LDot |
      LOpen | LClose
    deriving Show

tokenize :: String -> [LToken]
tokenize [] = []
tokenize ('(':rs) = LOpen : tokenize rs
tokenize (')':rs) = LClose : tokenize rs
tokenize ('\\':rs) = LLambda : tokenize rs
tokenize ('.':rs) = LDot : tokenize rs
tokenize (' ':rs) = tokenize rs
tokenize r@(r0:rs)
    | nextToken == "" = error ("Unknown symbol " ++ [r0])
    | otherwise = (LNamed nextToken):(tokenize $ dropWhile isSpace r)
    where
        nextToken = takeWhile isSpace r


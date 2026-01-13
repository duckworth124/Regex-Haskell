import Control.Applicative
import Control.Monad
import Control.Monad.State

type Parser a = StateT String Maybe a

data RegexPattern = Literal Char
newtype Regex = Regex {getPatterns :: [RegexPattern]}

applyPattern :: RegexPattern -> Parser String
applyPattern (Literal c) = return <$> parseChar c

apply :: Regex -> Parser String
apply (Regex xs) = concat <$> mapM applyPattern xs

parsePred :: (Char -> Bool) -> Parser Char
parsePred f = do
        (x : xs) <- get
        guard (f x)
        put xs
        return x

parseChar :: Char -> Parser Char
parseChar = parsePred . (==)

-- for now we let any character through. this will change as more metacharacters are added
parseLiteral :: Parser RegexPattern
parseLiteral = Literal <$> parsePred (const True)

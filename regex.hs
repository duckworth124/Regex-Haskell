import Control.Applicative
import Control.Monad
import Control.Monad.State

type Parser a = StateT String Maybe a

data RegexPattern = Literal Char
newtype Regex = Regex {getPatterns :: [RegexPattern]}

parsePred :: (Char -> Bool) -> Parser Char
parsePred f = do
        (x : xs) <- get
        guard (f x)
        put xs
        return x

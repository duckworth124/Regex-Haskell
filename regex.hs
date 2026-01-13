import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust)

type Parser a = StateT String Maybe a
data RegexPattern = Literal Char | PositiveGroup [Char] | NegativeGroup [Char] | End
        deriving (Show)
newtype Regex = Regex [RegexPattern]
        deriving (Show)

runParser :: Parser a -> String -> Maybe a
runParser = evalStateT

applyPattern :: RegexPattern -> Parser String
applyPattern (Literal c) = return <$> parseChar c
applyPattern (PositiveGroup cs) = return <$> parsePred (`elem` cs)
applyPattern (NegativeGroup cs) = return <$> parsePred (not . (`elem` cs))
applyPattern End = [] <$ eof

applyRegex :: Regex -> Parser String
applyRegex (Regex xs) = concat <$> mapM applyPattern xs

parsePred :: (Char -> Bool) -> Parser Char
parsePred f = do
        (x : xs) <- get
        guard (f x)
        put xs
        return x

eof :: Parser ()
eof = get >>= guard . null

parseChar :: Char -> Parser Char
parseChar = parsePred . (==)

parseString :: String -> Parser String
parseString = mapM parseChar

metacharacters :: [Char]
metacharacters = ['[', ']', '\\', '$']

parseEscaped :: Parser Char
parseEscaped = asum $ map ((parseChar '\\' *>) . parseChar) metacharacters

parseNotEscaped :: Parser Char
parseNotEscaped = parsePred $ not . (`elem` metacharacters)

parseLiteral :: Parser RegexPattern
parseLiteral = Literal <$> (parseNotEscaped <|> parseEscaped)

parsePositiveGroup :: Parser RegexPattern
parsePositiveGroup = parseChar '[' *> (PositiveGroup <$> some (parsePred (/= ']'))) <* parseChar ']'

parseNegativeGroup :: Parser RegexPattern
parseNegativeGroup = parseString "[^" *> (PositiveGroup <$> some (parsePred (/= ']'))) <* parseChar ']'

parseEnd :: Parser RegexPattern
parseEnd = End <$ parseChar '$'

parsePattern :: Parser RegexPattern
parsePattern = parsePositiveGroup <|> parseEnd <|> parseLiteral

parseRegex :: Parser Regex
parseRegex = Regex <$> many parsePattern

-- helper function for quick testing in the repl
grep :: String -> String -> Maybe String
grep = runParser . applyRegex . fromJust . runParser (parseRegex <* eof)

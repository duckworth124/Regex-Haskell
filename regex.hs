import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char (isDigit)
import Data.Maybe (fromJust)

-- state stores unparsed string and whether we are at the start or not
type Parser a = StateT (String, Bool) Maybe a
data RegexPattern
        = Literal Char
        | PositiveGroup [Char]
        | NegativeGroup [Char]
        | End
        | Start
        | Any
        | Group Regex
        | Digit
        deriving (Show)

newtype Regex = Regex [RegexPattern]
        deriving (Show)

runParser :: Parser a -> String -> Maybe a
runParser p s = evalStateT p (s, True)

applyPattern :: RegexPattern -> Parser String
applyPattern (Literal c) = return <$> parseChar c
applyPattern (PositiveGroup cs) = return <$> parsePred (`elem` cs)
applyPattern (NegativeGroup cs) = return <$> parsePred (not . (`elem` cs))
applyPattern End = [] <$ eof
applyPattern Start = [] <$ start
applyPattern Any = return <$> parsePred (const True)
applyPattern (Group r) = applyRegex r
applyPattern Digit = return <$> parsePred (isDigit)

applyRegex :: Regex -> Parser String
applyRegex (Regex xs) = concat <$> mapM applyPattern xs

parsePred :: (Char -> Bool) -> Parser Char
parsePred f = do
        (x : xs, _) <- get
        guard . f $ x
        put (xs, False)
        return x

eof :: Parser ()
eof = get >>= guard . null . fst

start :: Parser ()
start = get >>= guard . snd

parseChar :: Char -> Parser Char
parseChar = parsePred . (==)

parseString :: String -> Parser String
parseString = mapM parseChar

metacharacters :: [Char]
metacharacters = ['[', ']', '\\', '$', '^', '.', '(', ')']

parseEscaped :: Parser Char
parseEscaped = asum $ map ((parseChar '\\' *>) . parseChar) metacharacters

parseNotEscaped :: Parser Char
parseNotEscaped = parsePred $ not . (`elem` metacharacters)

parseLiteral :: Parser RegexPattern
parseLiteral = Literal <$> (parseNotEscaped <|> parseEscaped)

parsePositiveGroup :: Parser RegexPattern
parsePositiveGroup = parseChar '[' *> (PositiveGroup <$> some (parsePred (/= ']'))) <* parseChar ']'

parseNegativeGroup :: Parser RegexPattern
parseNegativeGroup = parseString "[^" *> (NegativeGroup <$> some (parsePred (/= ']'))) <* parseChar ']'

parseEnd :: Parser RegexPattern
parseEnd = End <$ parseChar '$'

parseStart :: Parser RegexPattern
parseStart = Start <$ parseChar '^'

parseAny :: Parser RegexPattern
parseAny = Any <$ parseChar '.'

parseGroup :: Parser RegexPattern
parseGroup = parseChar '(' *> (Group <$> parseRegex) <* parseChar ')'

parseDigit :: Parser RegexPattern
parseDigit = Digit <$ parseString "\\d"

parsePattern :: Parser RegexPattern
parsePattern =
        parseLiteral
                <|> parsePositiveGroup
                <|> parseNegativeGroup
                <|> parseEnd
                <|> parseStart
                <|> parseAny
                <|> parseGroup
                <|> parseDigit

parseRegex :: Parser Regex
parseRegex = Regex <$> many parsePattern

-- helper function for quick testing in the repl
grep :: String -> String -> Maybe String
grep = runParser . applyRegex . fromJust . runParser (parseRegex <* eof)

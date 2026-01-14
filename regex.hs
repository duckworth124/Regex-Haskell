import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
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
        | NotDigit
        | WhiteSpace
        | NotWhiteSpace
        | Word
        | NotWord
        | Some RegexPattern
        | Many RegexPattern
        | Optional RegexPattern
        deriving (Show)

newtype Regex = Regex [RegexPattern] deriving (Show)

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
applyPattern Digit = return <$> parsePred isDigit
applyPattern NotDigit = return <$> parsePred (not . isDigit)
applyPattern WhiteSpace = return <$> parsePred isSpace
applyPattern NotWhiteSpace = return <$> parsePred (not . isSpace)
applyPattern Word = return <$> parsePred isAlpha
applyPattern NotWord = return <$> parsePred (not . isAlpha)
applyPattern (Some p) = concat <$> some (applyPattern p)
applyPattern (Many p) = concat <$> many (applyPattern p)
applyPattern (Optional p) = concat <$> optional (applyPattern p)

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
metacharacters = ['[', ']', '\\', '$', '^', '.', '(', ')', '+', '*', '?']

parseEscaped :: Parser Char
parseEscaped = asum $ map ((parseChar '\\' *>) . parseChar) metacharacters

parseNotEscaped :: Parser Char
parseNotEscaped = parsePred $ not . (`elem` metacharacters)

parsePattern :: Parser RegexPattern
parsePattern = do
        p <-
                asum
                        [ Literal <$> (parseNotEscaped <|> parseEscaped)
                        , parseChar '[' *> (PositiveGroup <$> some (parsePred (/= ']'))) <* parseChar ']'
                        , parseString "[^" *> (NegativeGroup <$> some (parsePred (/= ']'))) <* parseChar ']'
                        , End <$ parseChar '$'
                        , Start <$ parseChar '^'
                        , Any <$ parseChar '.'
                        , parseChar '(' *> (Group <$> parseRegex) <* parseChar ')'
                        , Digit <$ parseString "\\d"
                        , NotDigit <$ parseString "\\D"
                        , WhiteSpace <$ parseString "\\s"
                        , NotWhiteSpace <$ parseString "\\S"
                        , Word <$ parseString "\\w"
                        , NotWord <$ parseString "\\W"
                        ]

        (Some p <$ parseChar '+')
                <|> (Many p <$ parseChar '*')
                <|> (Optional p <$ parseChar '?')
                <|> return p

parseRegex :: Parser Regex
parseRegex = Regex <$> many parsePattern

-- helper function for quick testing in the repl
grep :: String -> String -> Maybe String
grep = runParser . applyRegex . fromJust . runParser (parseRegex <* eof)

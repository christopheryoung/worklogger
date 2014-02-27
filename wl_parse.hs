
import Text.ParserCombinators.Parsec
import Data.Time.Calendar (Day, fromGregorian, toGregorian, diffDays)
import Control.Applicative ((<$>), (*>), (<*))

data LogLine = LogLine {
  getDate :: Day
  ,getTags :: [String]
  ,getActivity :: String
  ,getDuration :: Integer
  } deriving (Ord, Show, Eq)

integer = rd <$> many1 digit
  where rd = read :: String -> Integer

dateSep = oneOf "/"

cint n = rd <$> count n digit where
  rd = read :: String -> Integer

date :: Parser Day
date = do
  month <- integer
  dateSep
  day <- integer
  dateSep
  year <- integer
  return $ fromGregorian year (fromIntegral month) (fromIntegral day)

tag :: Parser String
tag = (many (noneOf ":") >> char ':') *> many (noneOf " \n\r:")

allTags :: Parser [String]
allTags = manyTill tag (notFollowedBy tag)

activity :: Parser String
activity = spaces *> many (noneOf " ") <* spaces

duration :: Parser Integer
duration = string "= " *> integer <* string " mins"

simpleDuration :: Parser Integer
simpleDuration = string " " *> integer <* string " mins"

timedActivityLogLineParser :: Parser LogLine
timedActivityLogLineParser = do
  d <- date
  ts <- allTags
  a <- activity
  dur <- duration
  return $ LogLine d ts a dur
  
bookLogLineParser :: Parser LogLine
bookLogLineParser = do
  d <- date
  t <- spaces *> string ":book"
  book <- many (noneOf "\n\r")
  return $ LogLine d [t] book 0

medLogLineParser :: Parser LogLine
medLogLineParser = do
  d <- date
  t <- spaces *> string ":med"
  dur <- simpleDuration
  return $ LogLine d [t] "" dur

logLineParser :: Parser LogLine
logLineParser = try medLogLineParser <|> try timedActivityLogLineParser <|> bookLogLineParser

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- testLine = "12/31/2013 :gym :cardio trampoline = 20 mins\n"
-- testLine = "12/31/2013 :book woo, bar \n"
-- testLine = "12/31/2013 :med 9 mins\n"

-- main = case parse logLineParser "(test)" testLine of
--             Left err  -> print err
--             Right res -> print res

main =
  do c <- getContents
     case parse logLineParser "(stdin)" c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ print r

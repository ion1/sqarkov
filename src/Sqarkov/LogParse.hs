module Sqarkov.LogParse
  ( LogEntry (..), LogEvent (..), Timestamp, Nick, Content
  , logGram7s, logEntries, logEntry
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as AP
import Data.Text (Text)
import qualified Data.Text as Text

import Sqarkov.NGram

data LogEntry = LogEntry Timestamp LogEvent
              | OtherEntry Text
  deriving (Eq, Ord, Show, Read)

data LogEvent = Message Nick Content
              | OtherEvent Text
  deriving (Eq, Ord, Show, Read)

type Timestamp = ()
type Nick      = Text
type Content   = Text

logGram7s :: Text -> Parser [Gram7]
logGram7s channel = concatMap go <$> logEntries
  where
    go (LogEntry _ (Message mnick mcontent)) =
      gram7Phrase channel mnick mcontent
    go _ = []

logEntries :: Parser [LogEntry]
logEntries = many logEntry

logEntry :: Parser LogEntry
logEntry = LogEntry <$> timestamp <*> (message <|> OtherEvent <$> line)
       <|> OtherEntry <$> line

timestamp :: Parser Timestamp
timestamp = do
  _year  <- replicateM 4 digit <* char '-'
  _month <- replicateM 2 digit <* char '-'
  _day   <- replicateM 2 digit <* char ' '
  _hour  <- replicateM 2 digit <* char ':'
  _min   <- replicateM 2 digit <* char ':'
  _sec   <- replicateM 2 digit <* char ' '
  _tzsign <- satisfy (\c -> c == '+' || c == '-')
  _tzhour <- replicateM 2 digit
  _tzmin  <- replicateM 2 digit <* char '\t'
  return ()

message :: Parser LogEvent
message = Message <$> (nickPrefix *> nick <* char '\t')
                  <*> line

line :: Parser Text
line = AP.takeWhile (/= '\n') <* char '\n'

nickPrefix :: Parser Text
nickPrefix = Text.singleton <$> satisfy isNickPrefixChar
         <|> pure Text.empty
  where
    isNickPrefixChar :: Char -> Bool
    isNickPrefixChar c = c == ' ' || c == '+' || c == '~' || c == '@'

nick :: Parser Nick
nick = Text.cons <$> satisfy isNickInitChar
                 <*> AP.takeWhile isNickChar
  where
    isNickChar :: Char -> Bool
    isNickChar c = isNickInitChar c || c == '-'

    isNickInitChar :: Char -> Bool
    isNickInitChar c = ('A' <= c && c <= 'Z')
                    || ('a' <= c && c <= 'z')
                    || ('[' <= c && c <= '`')  -- [ \ ] ^ _ `
                    || ('{' <= c && c <= '}')  -- { | }

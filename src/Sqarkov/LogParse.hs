{-# LANGUAGE OverloadedStrings #-}

module Sqarkov.LogParse
  ( LogEvent (..), Timestamp, Nick, Content
  , logGram7s, logEvents, logEvent
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as AP
import Data.Text (Text)
import qualified Data.Text as Text

import Sqarkov.NGram

data LogEvent = Message Nick Content
              | OtherEvent Text
  deriving (Eq, Ord, Show, Read)

type Timestamp = ()
type Nick      = Text
type Content   = Text

logGram7s :: Text -> Parser [Gram7]
logGram7s channel = concatMap go <$> logEvents
  where
    go (Message mnick mcontent) =
      gram7Phrase channel mnick mcontent
    go _ = []

logEvents :: Parser [LogEvent]
logEvents = many logEvent

logEvent :: Parser LogEvent
logEvent = timestamp *> (char '\t' *> weechatEvent <|> char ' ' *> irssiEvent)
       <|> OtherEvent <$> line
  where
    weechatEvent = Message <$> (nickPrefix *> nick <* char '\t')
                           <*> line
               <|> OtherEvent <$> line
    irssiEvent = Message <$> (char '<' *> nickPrefix *> nick <* "> ")
                         <*> line
             <|> OtherEvent <$> line

timestamp :: Parser Timestamp
timestamp = do
  _year  <- replicateM 4 digit <* char '-'
  _month <- replicateM 2 digit <* char '-'
  _day   <- replicateM 2 digit <* char ' '
  _hour  <- replicateM 2 digit <* char ':'
  _min   <- replicateM 2 digit <* char ':'
  _sec   <- replicateM 2 digit
  (char ' ' *> tz) <|> return ()
  where
    tz = do
      _tzsign <- satisfy (\c -> c == '+' || c == '-')
      _tzhour <- replicateM 2 digit
      _tzmin  <- replicateM 2 digit
      return ()

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
    -- http://tools.ietf.org/html/rfc2812#section-2.3.1
    -- Also allow digit as the initial character to recognize IRCnet UID nicks.

    isNickChar :: Char -> Bool
    isNickChar c = isNickInitChar c || c == '-'

    isNickInitChar :: Char -> Bool
    isNickInitChar c = ('A' <= c && c <= 'Z')
                    || ('a' <= c && c <= 'z')
                    || ('0' <= c && c <= '9')
                    || ('[' <= c && c <= '`')  -- [ \ ] ^ _ `
                    || ('{' <= c && c <= '}')  -- { | }

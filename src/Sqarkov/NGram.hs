module Sqarkov.NGram
  ( Gram7 (..), gram7Phrase
  ) where

import Control.Applicative
import Data.Char
import Data.Function
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Sqarkov.Char

data Gram7 = Gram7 !Text  -- ^ Channel
                   !Text  -- ^ Nick
                   !Text  -- ^ −3
                   !Text  -- ^ −2
                   !Text  -- ^ −1
                   !Text  -- ^ mid
                   !Text  -- ^ +1
                   !Text  -- ^ +2
                   !Text  -- ^ +3
  deriving (Eq, Ord, Show, Read)

instance ToRow Gram7 where
  toRow (Gram7 ch n w1 w2 w3 w4 w5 w6 w7) =
    map toField [ch, n, w1, w2, w3, w4, w5, w6, w7]

instance FromRow Gram7 where
  fromRow = Gram7 <$> field <*> field <*> field <*> field <*> field <*> field
                  <*> field <*> field <*> field

gram7Phrase :: Text -> Text -> Text -> [Gram7]
gram7Phrase channel nick phrase = phraseGrams
  where
    split = Text.groupBy ((==) `on` category) phrase

    -- Total length for a minimal split phrase: 7.
    withDelims = replicate 3 Text.empty ++ split ++ replicate 3 Text.empty

    phraseGrams = do
      (g1:g2:g3:g4:g5:g6:g7:_) <- tails withDelims
      return (Gram7 channel' nick' g1 g2 g3 g4 g5 g6 g7)

    channel' = Text.toLower channel
    nick'    = Text.toLower . Text.filter isAlpha $ nick

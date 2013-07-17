module Sqarkov.Char
  ( Category (..)
  , category
  , categoryMap
  ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

data Category = AlphaNum
              | Mark
              | Symbol
              | Separator
              | Other
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

category :: Char -> Category
category c = categoryMap Map.! generalCategory c

categoryMap :: Map GeneralCategory Category
categoryMap =
  Map.fromList
    [ (UppercaseLetter,      AlphaNum)
    , (LowercaseLetter,      AlphaNum)
    , (TitlecaseLetter,      AlphaNum)
    , (ModifierLetter,       AlphaNum)
    , (OtherLetter,          AlphaNum)

    , (NonSpacingMark,       Mark)
    , (SpacingCombiningMark, Mark)
    , (EnclosingMark,        Mark)

    , (DecimalNumber,        AlphaNum)
    , (LetterNumber,         AlphaNum)
    , (OtherNumber,          AlphaNum)

    , (ConnectorPunctuation, Symbol)
    , (DashPunctuation,      Symbol)
    , (OpenPunctuation,      Symbol)
    , (ClosePunctuation,     Symbol)
    , (InitialQuote,         Symbol)
    , (FinalQuote,           Symbol)
    , (OtherPunctuation,     Symbol)

    , (MathSymbol,           Symbol)
    , (CurrencySymbol,       Symbol)
    , (ModifierSymbol,       Symbol)
    , (OtherSymbol,          Symbol)

    , (Space,                Separator)
    , (LineSeparator,        Separator)
    , (ParagraphSeparator,   Separator)

    , (Control,              Other)
    , (Format,               Other)
    , (Surrogate,            Other)
    , (PrivateUse,           Other)
    , (NotAssigned,          Other)
    ]

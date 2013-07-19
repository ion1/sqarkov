{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.Foldable (foldMap)
import Data.Function
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.PostgreSQL.Simple
import System.Environment
import System.Exit
import System.IO

import Sqarkov.Database

main :: IO ()
main = do
  channel:nicks <- map Text.pack <$> getArgs

  withDatabase $ \db ->
    withTransaction db $ do
      res <- case nicks of
        [] -> phraseChannel      db channel
        _  -> phraseChannelNicks db channel nicks
      maybe notFound (Text.putStrLn . pretty) res

  where
    notFound = do
      hPutStrLn stderr "No phrase found."
      exitWith (ExitFailure 1)

    pretty :: [(Text, Text)] -> Text
    pretty tuples =
      foldMap (\fmt -> prettyWith fmt wordGroups <> "\n")
              [ ansiFormat, weechatFormat ]
      where
        wordGroups = map (fst . head &&& foldMap snd)
                   . groupBy ((==) `on` fst)
                   $ tuples

    prettyWith :: (Text, Text, Text, Text -> Text) -> [(Text, Text)] -> Text
    prettyWith (colorA, colorB, normal, escape) wordGroups
      = "<" <> Text.intercalate "," (alternateA (map mangle nicks)) <> "> "
     <> mconcat (alternateA phrases)
      where
        (nicks, phrases) = unzip wordGroups

        alternateA (x:xs) = colorA <> escape x <> normal : alternateB xs
        alternateA []     = []
        alternateB (x:xs) = colorB <> escape x <> normal : alternateA xs
        alternateB []     = []

    ansiFormat    = ("\ESC[32m", "\ESC[33m", "\ESC[0m", id)
    weechatFormat = ("\3c03",    "\3c07",    "\3o",     escape)
      where
        escape text = case Text.uncons text of
                        Just (',', _) -> zeroWidthNoBreakSpace <> text
                        _             -> text

    -- Modify the nicks so they are unlikely to cause highlights when pasted to
    -- IRC.
    mangle :: Text -> Text
    mangle = Text.intercalate zeroWidthNoBreakSpace . split 1
      where
        split n xs =
          case Text.splitAt n xs of
            (as, bs) | Text.null as -> []
                     | otherwise    -> as : split (n+2) bs

zeroWidthNoBreakSpace :: Text
zeroWidthNoBreakSpace = "\xfeff"

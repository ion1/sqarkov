{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Foldable (toList)
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

    pretty (chs, nicks, phr)
      = mconcat . intersperse " "
      $ [ Text.intercalate "," (toList chs)
        , "<" <> (Text.intercalate "," . map mangle . toList) nicks <> ">"
        , phr
        ]

    -- Modify the nicks so they are unlikely to cause highlights when pasted to
    -- IRC.
    mangle :: Text -> Text
    mangle = Text.intercalate zeroWidthNoBreakSpace . split 1
      where
        zeroWidthNoBreakSpace = "\xfeff"

        split n xs =
          case Text.splitAt n xs of
            (as, bs) | Text.null as -> []
                     | otherwise    -> as : split (n+2) bs

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Foldable (toList)
import Data.List
import Data.Monoid
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
        , "<" <> Text.intercalate "," (toList nicks) <> ">"
        , phr
        ]

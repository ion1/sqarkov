{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.Foldable (foldMap)
import Data.Function
import Data.List
import qualified Data.Map as Map
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
      foldMap (\cf -> prettyWith cf nicks nickColor wordGroups <> "\n")
              [ ansiColor, weechatColor ]
      where
        nicks = nub (map fst tuples)
        nickColor = (m Map.!)  -- Set.findIndex with containers ≥ 0.5.2.
          where m = Map.fromList . zip nicks $ [0..]

        wordGroups = map (fst . head &&& foldMap snd)
                   . groupBy ((==) `on` fst)
                   $ tuples

    prettyWith :: (Integer -> Text -> Text) -> [Text] -> (Text -> Integer)
               -> [(Text, Text)] -> Text
    prettyWith color nicks nickColor wordGroups
      = "<" <> Text.intercalate "," (map (color' <*> mangle) nicks) <> "> "
     <> foldMap (uncurry color') wordGroups
      where color' n = color (nickColor n)

    ansiColor n t
      | n >= 1 && n <=  6 = normal n       <> t <> none
      | n >= 7 && n <= 13 = bright (n - 7) <> t <> none
      | otherwise         = t
      where
        normal c = color False (30 + c)
        bright c = color True  (30 + c)
        none     = color False 0

        color :: Bool -> Integer -> Text
        color isBright c = "\ESC[" <> Text.pack (show c)
                        <> if isBright then ";1" else "" <> "m"

    weechatColor n t
      | n >= 1 && n <=  6 = normal n       <> escapeComma t <> none
      | n >= 7 && n <= 13 = bright (n - 7) <> escapeComma t <> none
      | otherwise         = t
      where
        escapeComma text = case Text.uncons text of
          Just (',', _) -> zeroWidthNoBreakSpace <> text
          _             -> text

        normal = color False
        bright = color True
        none   = "\3o"
        color isBright c = "\3c" <> colorMap Map.! (isBright, c)

        -- http://en.wikipedia.org/wiki/ANSI_escape_code#Colors
        -- http://www.weechat.org/files/doc/stable/weechat_user.en.html#command_line_colors
        colorMap = Map.fromList [ ((False, 0), "01")
                                , ((False, 1), "05")
                                , ((False, 2), "03")
                                , ((False, 3), "07")
                                , ((False, 4), "02")
                                , ((False, 5), "06")
                                , ((False, 6), "10")
                                , ((False, 7), "15")
                                , ((True,  0), "14")
                                , ((True,  1), "04")
                                , ((True,  2), "09")
                                , ((True,  3), "08")
                                , ((True,  4), "12")
                                , ((True,  5), "13")
                                , ((True,  6), "11")
                                , ((True,  7), "00")
                                ]

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

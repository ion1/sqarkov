module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as TextL
import Database.PostgreSQL.Simple
import System.Environment
import System.FilePath
import System.IO
import System.TimeIt
import Text.Printf

import Sqarkov.LogParse
import Sqarkov.Database

main :: IO ()
main =
  withDatabase $ \db -> do
    withTransaction db $ do
      files <- getArgs

      forM_ files $ \f -> do
        hPrint stderr f
        let channel = (Text.pack . takeFileName . takeDirectory) f

        Just gram7s <- maybeResult . parse (logGram7s channel <* endOfInput)
                   <$> TextL.readFile f
        let num = length gram7s

        (time, ()) <- timeItT $ insertGram7s db gram7s

        printf "%d/%.2f s = %.2f/1 s\n" num time (fromIntegral num / time)

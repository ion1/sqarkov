module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.IO as TextL
import Database.PostgreSQL.Simple
import System.Environment
import System.FilePath
import System.IO

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

        gram7s <- parseIO (logGram7s channel <* endOfInput) =<< TextL.readFile f

        insertGram7s db gram7s

parseIO :: Parser a -> TextL.Text -> IO a
parseIO p inp = either (ioError . userError) return
              $ eitherResult (parse p inp)

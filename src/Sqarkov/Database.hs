{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeOperators #-}

module Sqarkov.Database
  ( withDatabase
  , insertGram7s
  , phraseChannel
  , phraseChannelNicks
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSB
import Data.Foldable (foldMap, toList)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Database.Migrate as M
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Copy
import Database.PostgreSQL.Simple.SqlQQ

import Sqarkov.NGram

withDatabase :: (Connection -> IO a) -> IO a
withDatabase act =
  bracket (connectPostgreSQL "dbname=sqarkov") close $ \db -> do
    migs <- ioEitherT (M.find "migrations")
    _  <- M.executeMigrate migs db (M.migrate M.psqlMigrateDatabase)

    act db

insertGram7s :: Connection -> [Gram7] -> IO ()
insertGram7s db gram7s = do
  _ <- execute_ db createTempTable

  copy_ db copyTemp
  putCopyBuilder db . foldMap (BSB.byteString . escapeCopyGram7) $ gram7s
  _ <- putCopyEnd db

  _ <- execute_ db transferMain
  _ <- execute_ db dropTempTable
  return ()

  where
    createTempTable =
      [sql|
        create temporary table gram7_import
          ( channel text not null
          , nick    text not null
          , wl3     text not null
          , wl2     text not null
          , wl1     text not null
          , wm0     text not null
          , wr1     text not null
          , wr2     text not null
          , wr3     text not null
          );
      |]
    copyTemp =
      [sql|
        copy gram7_import
          (channel, nick, wl3, wl2, wl1, wm0, wr1, wr2, wr3)
          from stdin;
      |]
    transferMain =
      [sql|
        insert into channel (name)
          select distinct channel from gram7_import
            where not exists (select 0 from channel c where c.name = channel);

        insert into nick (name)
          select distinct nick from gram7_import
            where not exists (select 0 from nick n where n.name = nick);

        insert into word (word)
          select distinct wm0 from gram7_import
            where not exists (select 0 from word w where w.word = wm0);

        insert into gram7 (channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id)
          select ch.id, n.id, wl3.id, wl2.id, wl1.id, wm0.id, wr1.id, wr2.id, wr3.id
            from gram7_import g
            left join channel ch  on ch.name  = g.channel
            left join nick    n   on n.name   = g.nick
            left join word    wl3 on wl3.word = g.wl3
            left join word    wl2 on wl2.word = g.wl2
            left join word    wl1 on wl1.word = g.wl1
            left join word    wm0 on wm0.word = g.wm0
            left join word    wr1 on wr1.word = g.wr1
            left join word    wr2 on wr2.word = g.wr2
            left join word    wr3 on wr3.word = g.wr3;
      |]
    dropTempTable =
      [sql| drop table gram7_import; |]

phraseChannel :: Connection -> Text -> IO (Maybe [(Text, Text)])
phraseChannel db channel = phrase db sel (Only channel)
  where
    sel = [sql|
            join channel ch on ch.id = g.channel_id
            where ch.name = ?
          |]

phraseChannelNicks :: Connection -> Text -> [Text]
                  -> IO (Maybe [(Text, Text)])
phraseChannelNicks db channel nicks = phrase db sel (channel, In nicks)
  where
    sel = [sql|
            join channel ch on ch.id = g.channel_id
            join nick    n  on n.id  = g.nick_id
            where ch.name = ? and n.name in ?
          |]

phrase :: ToRow params => Connection -> Query -> params
       -> IO (Maybe [(Text, Text)])
phrase db sel params = do
  withSavepoint db $ do
    _ <- execute db createTempView params

    res <- fold_ db selectRandomPhrase mempty $
             \prev (n, phr) ->
               return $!! prev <> Just (Seq.singleton n, Seq.singleton phr)

    _ <- execute_ db dropTempView

    return $ fmap (\(ns, phrs) -> zip (majorityElems (toList ns)) (toList phrs))
                  res

  where
    createTempView =
      [sql|
        create temporary view gram7_selection as
          select g.id, g.channel_id, g.nick_id, g.wl3_id, g.wl2_id, g.wl1_id, g.wm0_id, g.wr1_id, g.wr2_id, g.wr3_id
            from gram7 g
      |] <> " " <> sel <> ";"
    selectRandomPhrase =
      [sql|
        select n.name, wm0.word
          from random_phrase() r
          join channel ch  on ch.id  = r.channel_id
          join nick    n   on n.id   = r.nick_id
          join word    wm0 on wm0.id = r.wm0_id
          order by r.ord
      |]
    dropTempView =
      [sql| drop view gram7_selection; |]

putCopyBuilder :: Connection -> BSB.Builder -> IO ()
putCopyBuilder db = mapM_ (putCopyData db) . BSL.toChunks . BSB.toLazyByteString

-- Only pick the majority nick that contributed to each word.
majorityElems :: Ord a => [a] -> [a]
majorityElems ns = concatMap (take 1 . majority) windows
  where
    padded = replicate 3 Nothing ++ map Just ns ++ replicate 3 Nothing

    windows = do
      (n1:n2:n3:n4:n5:n6:n7:_) <- tails padded
      return . catMaybes $ [n1, n2, n3, n4, n5, n6, n7]

    majority xs = sortBy (flip (comparing (counts Map.!))) xs
      where
        counts = Map.fromListWith (+) . map (, 1 :: Integer) $ xs

ioEitherT :: EitherT String IO a -> IO a
ioEitherT = either (ioError . userError) return <=< runEitherT

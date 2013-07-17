{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE TypeOperators #-}

module Sqarkov.Database
  ( withDatabase
  , insertGram7s
  , phraseChannel
  , phraseChannelNicks
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Either
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Database.Migrate as M
import Database.PostgreSQL.Simple
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
  -- TODO: “copy from” pending postgresql-simple support.
  _ <- executeMany db insertTemp gram7s
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
    insertTemp =
      [sql|
        insert into gram7_import
          (channel, nick, wl3, wl2, wl1, wm0, wr1, wr2, wr3)
          values (?, ?, ?, ?, ?, ?, ?, ?, ?);
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

phraseChannel :: Connection -> Text -> IO (Maybe (Set Text, Set Text, Text))
phraseChannel db channel = phrase db sel (Only channel)
  where
    sel = [sql|
            join channel ch on ch.id = g.channel_id
            where ch.name = ?
          |]

phraseChannelNicks :: Connection -> Text -> [Text]
                  -> IO (Maybe (Set Text, Set Text, Text))
phraseChannelNicks db channel nicks = phrase db sel (channel, In nicks)
  where
    sel = [sql|
            join channel ch on ch.id = g.channel_id
            join nick    n  on n.id  = g.nick_id
            where ch.name = ? and n.name in ?
          |]

phrase :: ToRow params => Connection -> Query -> params
       -> IO (Maybe (Set Text, Set Text, Text))
phrase db sel params =
  fold db (randomQuery sel) params mempty $
    \prev (ch, n, phr) ->
      return $ prev <> Just (Set.singleton ch, Set.singleton n, phr)

-- TODO: This seriously needs some cleaning up.
randomQuery :: Query -> Query
randomQuery sel =
  [sql|
    with recursive
    everything as (
      select channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id
      from gram7 g
  |]
  <> " " <> sel <> " " <>
  [sql|
    ),
    start as (
      select 0 num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id
        from everything
        offset floor(random()*(select count(*) from everything))
        limit 1
    ),
    -- Recursion to the left.
    rleft (num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id) as (
      (
        select num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id
          from start
      ) union (
        with sel as (
          select r.num - 1 num, n.channel_id, n.nick_id, n.wl3_id, n.wl2_id, n.wl1_id, n.wm0_id, n.wr1_id, n.wr2_id, n.wr3_id
            from everything n
            join rleft r on r.wl3_id = n.wl2_id
                        and r.wl2_id = n.wl1_id
                        and r.wl1_id = n.wm0_id
                        and r.wm0_id = n.wr1_id
                        and r.wr1_id = n.wr2_id
                        and r.wr2_id = n.wr3_id
        )
          select num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id
            from sel
            offset floor(random()*(select count(*) from sel))
            limit 1
      )
    ),
    -- Recursion to the right.
    rright (num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id) as (
      (
        select num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id
          from start
      ) union (
        with sel as (
          select r.num + 1 num, n.channel_id, n.nick_id, n.wl3_id, n.wl2_id, n.wl1_id, n.wm0_id, n.wr1_id, n.wr2_id, n.wr3_id
            from everything n
            join rright r on r.wl2_id = n.wl3_id
                         and r.wl1_id = n.wl2_id
                         and r.wm0_id = n.wl1_id
                         and r.wr1_id = n.wm0_id
                         and r.wr2_id = n.wr1_id
                         and r.wr3_id = n.wr2_id
        )
          select num, channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id
            from sel
            offset floor(random()*(select count(*) from sel))
            limit 1
      )
    )
    select ch.name, n.name, wm0.word
      from (select * from rleft union select * from rright) r
      join channel ch  on ch.id  = r.channel_id
      join nick    n   on n.id   = r.nick_id
      join word    wm0 on wm0.id = r.wm0_id
      order by r.num
  |]

ioEitherT :: EitherT String IO a -> IO a
ioEitherT = either (ioError . userError) return <=< runEitherT

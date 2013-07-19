create table channel
  ( id   serial primary key
  , name text not null
  );
create unique index channel_name on channel (name);

create table nick
  ( id   serial primary key
  , name text not null
  );
create unique index nick_name on nick (name);

create sequence word_id_seq minvalue 1;
create table word
  ( id   integer default nextval('word_id_seq') primary key
  , word text not null
  );
alter sequence word_id_seq owned by word.id;
create unique index word_word on word (word);
insert into word (id, word) values (0, '');

create table gram7
  ( id         serial primary key
  , channel_id integer not null references channel (id)
  , nick_id    integer not null references nick (id)
  -- left
  , wl3_id     integer not null references word (id)
  , wl2_id     integer not null references word (id)
  , wl1_id     integer not null references word (id)
  -- middle
  , wm0_id     integer not null references word (id)
  -- right
  , wr1_id     integer not null references word (id)
  , wr2_id     integer not null references word (id)
  , wr3_id     integer not null references word (id)
  );
create index gram7_ch         on gram7 (channel_id);
--create index gram7_ch_word    on gram7 (channel_id, wm0_id);
create index gram7_ch_left    on gram7 (channel_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id);
create index gram7_ch_right   on gram7 (channel_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id);
create index gram7_ch_n       on gram7 (channel_id, nick_id);
--create index gram7_ch_n_word  on gram7 (channel_id, nick_id, wm0_id);
create index gram7_ch_n_left  on gram7 (channel_id, nick_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id, wr3_id);
create index gram7_ch_n_right on gram7 (channel_id, nick_id, wl3_id, wl2_id, wl1_id, wm0_id, wr1_id, wr2_id);

create view gram7_exp as
  select g.id id, ch.name channel, n.name nick,
         wl3.word wl3, wl2.word wl2, wl1.word wl1,
         wm0.word wm0,
         wr1.word wr1, wr2.word wr2, wr3.word wr3
    from gram7   g
    join channel ch  on ch.id  = g.channel_id
    join nick    n   on n.id   = g.nick_id
    join word    wl3 on wl3.id = g.wl3_id
    join word    wl2 on wl2.id = g.wl2_id
    join word    wl1 on wl1.id = g.wl1_id
    join word    wm0 on wm0.id = g.wm0_id
    join word    wr1 on wr1.id = g.wr1_id
    join word    wr2 on wr2.id = g.wr2_id
    join word    wr3 on wr3.id = g.wr3_id;

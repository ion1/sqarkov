create type num_gram7 as
  ( ord        integer
  , gram7_id   integer
  , channel_id integer
  , nick_id    integer
  , wl3_id     integer
  , wl2_id     integer
  , wl1_id     integer
  , wm0_id     integer
  , wr1_id     integer
  , wr2_id     integer
  , wr3_id     integer
  );

create function random_phrase ()
  returns setof num_gram7
  language plpgsql
  as $$
    declare
      res  num_gram7%rowtype;
      orig num_gram7%rowtype;
    begin
      -- Pick a starting point.
      select 0, g.id, g.channel_id, g.nick_id, g.wl3_id, g.wl2_id, g.wl1_id, g.wm0_id, g.wr1_id, g.wr2_id, g.wr3_id
        into orig
        from gram7_selection g
        offset floor(random()*(select count(*) from gram7_selection)) limit 1;
      if orig.gram7_id is null then
        return;
      end if;
      return next orig;

      res := orig;

      -- Iterate to the left.
      loop
        with inner_selection as (
          select * from gram7_selection g
          where g.wl2_id = res.wl3_id
            and g.wl1_id = res.wl2_id
            and g.wm0_id = res.wl1_id
            and g.wr1_id = res.wm0_id
            and g.wr2_id = res.wr1_id
            and g.wr3_id = res.wr2_id
        )
        select res.ord - 1, i.id, i.channel_id, i.nick_id, i.wl3_id, i.wl2_id, i.wl1_id, i.wm0_id, i.wr1_id, i.wr2_id, i.wr3_id
          into res
          from inner_selection i
          offset floor(random()*(select count(*) from inner_selection)) limit 1;

        if res.gram7_id is null then
          exit;
        end if;
        return next res;
      end loop;

      res := orig;

      -- Iterate to the right.
      loop
        with inner_selection as (
          select * from gram7_selection g
          where g.wl3_id = res.wl2_id
            and g.wl2_id = res.wl1_id
            and g.wl1_id = res.wm0_id
            and g.wm0_id = res.wr1_id
            and g.wr1_id = res.wr2_id
            and g.wr2_id = res.wr3_id
        )
        select res.ord + 1, i.id, i.channel_id, i.nick_id, i.wl3_id, i.wl2_id, i.wl1_id, i.wm0_id, i.wr1_id, i.wr2_id, i.wr3_id
          into res
          from inner_selection i
          offset floor(random()*(select count(*) from inner_selection)) limit 1;

        if res.gram7_id is null then
          exit;
        end if;
        return next res;
      end loop;
    end;
  $$;

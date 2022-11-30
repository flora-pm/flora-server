-- migrate:up
create table if not exists downloads (
  download_id serial primary key,
  release_id uuid references releases,
  downloads integer,
  day timestamptz
);

create index on downloads (release_id);
create index on downloads (day);

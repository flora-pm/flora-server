-- migrate:up
create extension if not exists pg_trgm;
create type package_state as enum ('unknown', 'fully-imported');

-- A package is comprised of metadata and has many releases.
create table if not exists packages (
  package_id uuid primary key,
  namespace text not null,
  name text not null,
  owner_id uuid references users,
  created_at timestamptz not null,
  updated_at timestamptz not null,
  status package_state not null
);

create index package_name_trgm on packages USING GIN (name gin_trgm_ops);

create extension if not exists pg_trgm;
-- A package is comprised of metadata and has many releases.
create table if not exists packages (
  package_id uuid primary key,
  namespace text not null,
  name text not null,
  synopsis text,
  owner_id uuid references users,
  metadata jsonb not null, -- { homepage, documentation url, repository url, issues url }
  created_at timestamptz not null,
  updated_at timestamptz not null
);

create index package_name_trgm on packages USING GIN (name gin_trgm_ops);

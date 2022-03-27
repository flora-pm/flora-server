-- A package is comprised of metadata and has many releases.
create table if not exists packages (
  namespace text not null,
  name text not null,
  synopsis text not null,
  owner_id uuid references users,
  metadata jsonb not null, -- { homepage, documentation url, repository url, issues url }
  created_at timestamptz not null,
  updated_at timestamptz not null,
  primary key(namespace, name)
);

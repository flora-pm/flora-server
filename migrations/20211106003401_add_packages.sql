create table packages (
  package_id uuid primary key,
  namespace text not null,
  name text not null,
  synopsis text not null,
  owner_id uuid references users,
  metadata jsonb not null, -- { homepage, documentation url, repository url, issues url }
  created_at timestamptz not null,
  updated_at timestamptz not null
);

create unique index on packages(lower(name), lower(namespace));

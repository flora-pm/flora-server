create table packages (
  package_id uuid primary key,
  name text not null,
  synopsis text not null,
  license text not null,
  owner_id uuid references users,
  -- meta jsonb, -- { homepage, documentation link, repository url, issues url }
  created_at timestamptz not null,
  updated_at timestamptz not null
);

create unique index on packages(lower(name) text_pattern_ops);

-- A release belongs to a package, and contains multiple components.
create table if not exists releases (
  release_id uuid primary key,
  package_id uuid references packages,
  version int[] not null,
  metadata jsonb, -- { description, synopsis, homepage, documentation url, repository url, issues url }
  archive_checksum text not null,
  uploaded_at timestamptz,
  created_at timestamptz not null,
  updated_at timestamptz not null,
  readme text
);

create index on releases(package_id);
create index on releases(uploaded_at);
create unique index on releases(package_id, version);

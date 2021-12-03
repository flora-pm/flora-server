-- migrate:up
create table releases (
  release_id uuid primary key,
  package_id uuid references packages,
  version text not null,
  archive_checksum text not null,
  created_at timestamptz,
  updated_at timestamptz,
  unique (package_id, version)
);

create index on releases(package_id);

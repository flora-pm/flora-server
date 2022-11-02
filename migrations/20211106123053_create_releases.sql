-- migrate:up
create type import_status as enum ('imported', 'inexistent', 'not-imported');

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
  readme text,
  readme_status import_status not null,
  changelog text,
  changelog_status import_status,
  constraint consistent_readme_status
    check (
         (readme_status = 'imported' and readme is not null)
      or ((readme_status = 'not-imported' or readme_status = 'inexistent') and readme is null)
    ),
  constraint consistent_changelog_status
    check (
         ((changelog_status = 'imported')
            and changelog is not null)
      or ((changelog_status = 'not-imported'  or changelog_status = 'inexistent') and changelog is null)
    )
);

create index on releases(package_id);
create index on releases(uploaded_at);
create index on releases(readme_status);
create unique index on releases(package_id, version);
create index on releases(changelog_status);

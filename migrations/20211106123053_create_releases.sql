-- A release belongs to a package, and contains multiple components.
create table if not exists releases (
  release_id uuid primary key,
  package_name text not null,
  package_namespace text not null,
  version text not null,
  archive_checksum text not null,
  created_at timestamptz not null,
  updated_at timestamptz not null,

  constraint fk_package
    foreign key (package_name, package_namespace)
      references packages(name, namespace)
);

create unique index on releases(package_name, package_namespace, version);

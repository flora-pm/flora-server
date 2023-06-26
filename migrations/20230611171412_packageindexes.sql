create table if not exists package_indexes (
  package_index_id uuid primary key,
  repository text not null,
  timestamp timestamptz
);

create unique index on package_indexes(repository);

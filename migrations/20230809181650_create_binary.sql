create table if not exists blob_relations (
  blob_hash bytea not null,
  blob_dep_hash bytea not null,
  blob_dep_path text not null,
  blob_dep_directory bool not null,

  constraint pk_relation primary key (blob_hash, blob_dep_path)
);

-- Just points to the hash of the root directory
alter table releases add tarball_hash bytea;

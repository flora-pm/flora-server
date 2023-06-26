alter table releases
  add repository text default null;
alter table releases
  add constraint repository
    foreign key(repository)
    references package_indexes(repository);

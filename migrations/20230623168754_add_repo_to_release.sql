alter table releases
  add constraint repository
    foreign key(repository)
    references package_indexes(repository);

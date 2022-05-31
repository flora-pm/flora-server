create type package_state as enum ('unknown', 'fully-imported');

alter table if exists packages add column if not exists status package_state;
update packages set status = 'fully-imported';
alter table packages alter column status set not null;

alter table packages alter column synopsis drop not null;
alter table packages alter column metadata drop not null;
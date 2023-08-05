alter table packages
  drop column metadata;

alter table packages
  add deprecation_info jsonb default '{}';

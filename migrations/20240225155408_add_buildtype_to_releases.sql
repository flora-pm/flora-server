create type build_type as enum
  ('Simple', 'Configure', 'Make', 'Custom');

alter table releases
  add column build_type build_type not null default 'Simple';

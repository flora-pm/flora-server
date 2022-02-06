create table requirements (
  requirement_id uuid primary key,
  package_component_id uuid references package_components not null, -- Points to the dependent
  package_id uuid references packages not null, -- Points to the dependency
  requirement text not null,
  metadata jsonb not null
);

create index on "requirements" (package_component_id);

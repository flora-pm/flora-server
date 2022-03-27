create table if not exists requirements (
  requirement_id uuid primary key,
  package_component_id uuid references package_components not null, -- Points to the dependent
  package_name text not null,
  package_namespace text not null,
  requirement text not null,
  metadata jsonb not null,

  constraint fk_requirements
    foreign key (package_name, package_namespace)
      references packages(name, namespace)
);

create index on "requirements" (package_component_id);

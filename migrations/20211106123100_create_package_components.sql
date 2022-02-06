-- A package component is the smallest unit of a Hackage package.
-- Each package may contain a default, top-level library, as well as
-- sub-libraries, executables, test suites & benchmark.
-- All these are components have many dependency requirements
create type component as enum ('library', 'executable', 'test', 'benchmark', 'foreign-library');

create table package_components (
  package_component_id uuid primary key,
  release_id uuid references releases not null,
  component_name text not null,
  component_type component not null
);

create index on "package_components" (release_id);

create table "requirements" (
  id uuid primary key,
  release_id uuid references releases,
  package_id uuid references packages,
  "requirement" text
);

create index on "requirements" (release_id);

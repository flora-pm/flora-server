create table package_publishers (
  package_publisher_id uuid primary key,
  package_id uuid references packages not null,
  user_id uuid references users not null
);

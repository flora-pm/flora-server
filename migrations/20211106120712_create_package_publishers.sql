create table if not exists package_publishers (
  package_publisher_id uuid primary key,
  package_name text not null,
  package_namespace text not null,
  user_id uuid references users not null,

  constraint fk_package_publishers
    foreign key (package_name, package_namespace)
      references packages(name, namespace)
);

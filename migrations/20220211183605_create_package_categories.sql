create table if not exists package_categories (
  package_category_id uuid primary key,
  package_name text not null,
  package_namespace text not null,
  category_id uuid references categories,

  constraint fk_package_categories
    foreign key (package_name, package_namespace)
      references packages(name, namespace)
);

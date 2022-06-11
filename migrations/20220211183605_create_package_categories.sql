create table if not exists package_categories (
  package_id uuid references packages,
  category_id uuid references categories,
  primary key (package_id, category_id)
);

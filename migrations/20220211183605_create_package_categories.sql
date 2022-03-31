create table if not exists package_categories (
  package_category_id uuid primary key,
  package_id uuid references packages,
  category_id uuid references categories
);

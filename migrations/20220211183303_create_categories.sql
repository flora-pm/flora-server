-- migrate:up
create table if not exists categories (
  category_id uuid primary key,
  name text not null,
  slug text not null,
  synopsis text not null
);

create unique index on categories(slug);
create unique index on categories(name);

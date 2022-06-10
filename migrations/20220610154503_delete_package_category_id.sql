alter table package_categories
drop column if exists package_category_id,
add constraint package_categories_pkey primary key (package_id, category_id);
CREATE INDEX user_organisation_organisation_id_fkey ON user_organisation(organisation_id);
CREATE INDEX user_organisation_user_id_fkey ON user_organisation(user_id);
CREATE INDEX packages_owner_id_fkey ON packages(owner_id);
CREATE INDEX package_publishers_package_id_fkey ON package_publishers(package_id);
CREATE INDEX package_publishers_user_id_fkey ON package_publishers(user_id);
CREATE INDEX repository ON releases(repository);
CREATE INDEX requirements_package_id_fkey ON requirements(package_id);
CREATE INDEX package_categories_category_id_fkey ON package_categories(category_id);

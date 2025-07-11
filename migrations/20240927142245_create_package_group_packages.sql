CREATE TABLE IF NOT EXISTS package_group_packages (
    package_group_package_id uuid PRIMARY KEY
  , package_group_id uuid NOT NULL REFERENCES package_groups ON DELETE CASCADE
  , package_id uuid NOT NULL REFERENCES packages
);

CREATE INDEX package_group_packages_package_id_fkey
  ON package_group_packages (package_id);

CREATE UNIQUE INDEX package_group_package
  ON package_group_packages (package_group_id , package_id);

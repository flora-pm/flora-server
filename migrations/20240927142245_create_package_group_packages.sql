CREATE TABLE IF NOT EXISTS package_group_packages (
    package_group_id uuid NOT NULL REFERENCES package_groups (id)
  , package_id uuid NOT NULL REFERENCES packages (package_id)
  , PRIMARY KEY (package_group_id, package_id)
)

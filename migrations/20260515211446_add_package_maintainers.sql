CREATE TABLE IF NOT EXISTS package_maintainers (
    package_maintainer_id uuid PRIMARY KEY
  , package_uploader_id uuid REFERENCES package_uploaders NOT NULL
  , package_id uuid REFERENCES packages NOT NULL
  , created_at timestamptz NOT NULL
  , updated_at timestamptz NOT NULL
)

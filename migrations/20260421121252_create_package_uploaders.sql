CREATE TABLE IF NOT EXISTS package_uploaders (
    package_uploader_id uuid PRIMARY KEY
  , username text NOT NULL
  , package_index_id uuid REFERENCES package_indexes NOT NULL
  , user_id uuid REFERENCES users
);

CREATE UNIQUE INDEX
  ON package_uploaders (username
                      , package_index_id);

CREATE INDEX ON package_uploaders(package_index_id);

CREATE INDEX ON package_uploaders(user_id);

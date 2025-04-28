CREATE TABLE IF NOT EXISTS package_feeds (
    entry_id uuid PRIMARY KEY
  , title text NOT NULL
  , link text
  , content text NOT NULL
  , package_id uuid REFERENCES packages NOT NULL
  , created_at timestamptz NOT NULL
  , updated_at timestamptz NOT NULL
);

CREATE INDEX
  ON package_feeds (updated_at);

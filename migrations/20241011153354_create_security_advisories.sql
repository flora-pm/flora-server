CREATE TABLE IF NOT EXISTS security_advisories (
    advisory_id uuid PRIMARY KEY
  , hsec_id text NOT NULL
  , modified timestamptz NOT NULL
  , published timestamptz NOT NULL
  , capecs integer[] NOT NULL
  , cwes integer[] NOT NULL
  , keywords text[] NOT NULL
  , aliases text[] NOT NULL
  , related text[] NOT NULL
  , advisory_references jsonb NOT NULL
  , pandoc jsonb NOT NULL
  , html text NOT NULL
  , summary text NOT NULL
  , details text NOT NULL
);

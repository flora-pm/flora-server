CREATE TABLE IF NOT EXISTS affected_version_ranges (
    affected_version_id uuid PRIMARY KEY
  , affected_package_id uuid REFERENCES affected_packages NOT NULL
  , introduced_version int[] NOT NULL
  , fixed_version int[]
);

CREATE INDEX affected_version_ranges_affected_package_id_fkey
  ON affected_version_ranges (affected_package_id);

CREATE INDEX affected_version_ranges_introduced_version
  ON affected_version_ranges (introduced_version);

CREATE INDEX affected_version_ranges_fixed_version
  ON affected_version_ranges (fixed_version);

CREATE TABLE IF NOT EXISTS affected_packages (
    affected_package_id uuid PRIMARY KEY
  , advisory_id uuid REFERENCES security_advisories
  , package_id uuid REFERENCES packages NOT NULL
  , cvss text NOT NULL
  , architectures text[]
  , operating_systems text[]
  , declarations text[][]
);

CREATE INDEX affected_packages_advisory_id_fkey
  ON affected_packages (advisory_id);

CREATE INDEX affected_packages_package_id_fkey
  ON affected_packages (package_id);

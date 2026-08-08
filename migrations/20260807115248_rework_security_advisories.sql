DELETE FROM affected_version_ranges;

DELETE FROM affected_packages;

DELETE FROM security_advisories;

CREATE UNIQUE INDEX
  ON security_advisories (hsec_id);

CREATE UNIQUE INDEX
  ON affected_packages (advisory_id
                      , namespace
                      , package_name);

ALTER TABLE affected_packages
    ALTER COLUMN package_id DROP NOT NULL
  , ADD COLUMN namespace text NOT NULL
  , ADD COLUMN affected_component text NOT NULL
  , DROP COLUMN declarations
  , ADD COLUMN declrations jsonb;

ALTER TABLE security_advisories
  DROP COLUMN pandoc;

CREATE UNIQUE INDEX
  ON affected_packages (advisory_id
                      , namespace
                      , affected_component)

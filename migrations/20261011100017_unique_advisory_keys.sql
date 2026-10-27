DELETE FROM affected_version_ranges;
DELETE FROM affected_packages;
DELETE FROM security_advisories;

ALTER TABLE affected_packages
  ALTER COLUMN advisory_id SET NOT NULL;

CREATE UNIQUE INDEX security_advisories_hsec_id_key
  ON security_advisories (hsec_id);

CREATE UNIQUE INDEX affected_packages_advisory_id_package_id_key
  ON affected_packages (advisory_id, package_id);

CREATE UNIQUE INDEX affected_version_ranges_natural_key
  ON affected_version_ranges (affected_package_id, introduced_version, fixed_version) NULLS NOT DISTINCT;

DROP INDEX IF EXISTS affected_packages_advisory_id_fkey;
DROP INDEX IF EXISTS affected_version_ranges_affected_package_id_fkey;

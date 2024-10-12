CREATE TABLE IF NOT EXISTS affected_packages (
  affected_package_id uuid primary key,
  advisory_id uuid references security_advisories,
  package_id uuid references packages not null,
  cvss text not null,
  introduced_version uuid references releases not null,
  fixed_version uuid references releases,
  architectures text[],
  operating_systems text[],
  declarations text[][]
);

CREATE INDEX affected_packages_advisory_id_fkey ON affected_packages(advisory_id);
CREATE INDEX affected_packages_package_id_fkey ON affected_packages(package_id);
CREATE INDEX affected_packages_introduced_version_fkey ON affected_packages(introduced_version);
CREATE INDEX affected_packages_fixed_version_fkey ON affected_packages(fixed_version);

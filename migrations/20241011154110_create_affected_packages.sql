CREATE TABLE IF NOT EXISTS affected_packages (
  affected_package_id uuid primary key,
  advisory_id uuid references security_advisories,
  package_id uuid references packages not null,
  cvss text not null,
  introduced_version uuid references releases not null,
  fixed_version uuid references releases,
  architecture text[],
  os text[],
  declarations text[][]
)

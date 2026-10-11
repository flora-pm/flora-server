synopsis: Make the advisories import repeatable
prs: #1343
significance: significant

description: {

- Advisory, affected-package and version-range upserts now refresh existing rows (keyed on `hsec_id` / `(advisory_id, package_id)` / the full range) instead of duplicating them.
- Migration adding unique constraints on the advisory keys; existing advisory tables are emptied and must be re-provisioned with `flora-cli provision advisories`.

}

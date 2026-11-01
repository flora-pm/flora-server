synopsis: Make the advisories import repeatable
prs: #1343
significance: significant

description: {

- The advisory upsert is keyed on `hsec_id` and version ranges on the full range; affected packages are replaced wholesale, so every upstream `[[affected]]` entry is stored as its own row with its own ranges.
- Migration adding unique constraints on the advisory keys; existing advisory tables are emptied and must be re-provisioned with `flora-cli provision advisories`. Databases that already ran an earlier draft of this migration must be reset (the `(advisory_id, package_id)` unique index was removed).

}

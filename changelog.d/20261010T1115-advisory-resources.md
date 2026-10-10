synopsis: Expose web resources for security advisories
prs: #1331
issues: #555
significance: significant

description: {

- Add `/security/advisories/namespace/<@namespace>` to list all advisories pertaining to a namespace, with pagination.
- Add `/security/advisories/package/<@namespace>/<package>` to list all advisories pertaining to a package, with pagination.
- Add `/security/advisories/<advisory-id>` to view a specific advisory by its HSEC identifier.

}

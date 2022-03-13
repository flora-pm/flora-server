create materialized view latest_versions (
  namespace,
  name,
  synopsis,
  package_id,
  version) as
    select distinct p.namespace, p.name, p.synopsis, p.package_id, max(r.version) as version
    from "packages" as p
      inner join "releases" as r on r."package_id" = p."package_id"
    group by (p.namespace, p.name, p.synopsis, p.package_id);

create index on latest_versions (namespace, name);
create unique index on latest_versions (name, namespace, version);

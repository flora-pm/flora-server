create materialized view latest_versions (
  namespace,
  name,
  synopsis,
  package_id,
  version) as
    select distinct on (p.package_id)
        p.namespace
      , p.name
      , r.metadata ->> 'synopsis' as synopsis
      , p.package_id
      , r.version
    from "packages" as p
      inner join "releases" as r on p."package_id" = r."package_id"
    where p.status = 'fully-imported' 
    group by p.namespace, p.name, synopsis, p.package_id, version
    order by p.package_id, version desc;

create index on latest_versions (namespace, name);
create unique index on latest_versions (name, namespace, version);

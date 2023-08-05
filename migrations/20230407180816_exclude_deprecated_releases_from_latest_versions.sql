-- migrate:up
drop materialized view latest_versions;

create materialized view latest_versions (
  namespace,
  name,
  synopsis,
  package_id,
  version,
  license) as
    select distinct on (p.package_id)
        p.namespace
      , p.name
      , r.synopsis
      , p.package_id
      , r.version
      , r.license
    from "packages" as p
    inner join "releases" as r on p."package_id" = r."package_id"
    where p.status = 'fully-imported' 
      and ((r."deprecated" != true)
        or (r."deprecated" is null)
      )
    group by p.namespace, p.name, synopsis, p.package_id, r.version, license
    order by p.package_id, version desc;

create index on latest_versions (name);
create index on latest_versions (namespace, name);
create unique index on latest_versions (name, namespace, version);


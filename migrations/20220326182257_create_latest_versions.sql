create materialized view latest_versions (
  namespace,
  name,
  synopsis,
  version) as
    select distinct p.namespace, p.name, p.synopsis, max(r.version) as version
    from "packages" as p
      inner join "releases" as r on r."package_name" = p."name"
                                and r."package_namespace" = p."namespace"
    group by (p.namespace, p.name, p.synopsis);

create index on latest_versions (namespace, name);
create unique index on latest_versions (name, namespace, version);

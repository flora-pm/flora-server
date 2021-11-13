create materialized view dependants (
  name,
  namespace,
  dependant_id) as
    select distinct p3.name as name, p3.namespace as namespace, p0.package_id as dependant_id
    from "packages" as p0
    inner join "releases" as r1 on r1."package_id" = p0."package_id"
    inner join "requirements" as r2 on r2."release_id" = r1."release_id"
    inner join "packages" as p3 on p3."package_id" = r2."package_id";

create index on dependants (name, dependant_id);
create unique index on dependants (name, namespace, dependant_id);

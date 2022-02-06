-- Copyright 2015 Six Colors AB
-- Copyright 2021 Flora.pm development team
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

create materialized view dependents (
  name,
  namespace,
  dependent_id) as
  select distinct p4.name as name, p4.namespace as namespace, p0.package_id as package_id
  from packages as p0
    inner join "releases" as r1 on r1."package_id" = p0."package_id"
    inner join "package_components" as pc2 on pc2."release_id" = r1."release_id"
    inner join "requirements" as r3 on r3."package_component_id" = pc2."package_component_id"
    inner join "packages" as p4 on p4."package_id" = r3."package_id";

create index on dependents (name, dependent_id);
create unique index on dependents (name, namespace, dependent_id);

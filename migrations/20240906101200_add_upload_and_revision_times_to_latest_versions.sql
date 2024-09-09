DROP MATERIALIZED VIEW latest_versions;

CREATE MATERIALIZED VIEW latest_versions (namespace, name, synopsis, package_id, version, license, uploaded_at, revised_at)
  AS SELECT DISTINCT ON (p.package_id) p.namespace
                                     , p.name
                                     , r.synopsis
                                     , p.package_id
                                     , r.version
                                     , r.license
                                     , r.uploaded_at
                                     , r.revised_at
     FROM packages AS p
          INNER JOIN releases AS r ON p.package_id = r.package_id
     WHERE p.status = 'fully-imported'
       AND r.deprecated IS DISTINCT FROM TRUE
     GROUP BY p.namespace, p.name, synopsis, p.package_id, r.version, license, r.uploaded_at, r.revised_at
     ORDER BY p.package_id
            , version DESC;

CREATE INDEX 
  ON latest_versions (name);

CREATE INDEX 
  ON latest_versions (namespace
                    , name);

CREATE UNIQUE INDEX 
  ON latest_versions (name
                    , namespace
                    , version)

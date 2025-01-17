DROP MATERIALIZED VIEW latest_versions;

CREATE MATERIALIZED VIEW latest_versions (namespace, name, synopsis, package_id, version, license, uploaded_at, revised_at, executables)
  AS SELECT DISTINCT ON (p0.package_id) p0.namespace
                                      , p0.name
                                      , r1.synopsis
                                      , p0.package_id
                                      , r1.version
                                      , r1.license
                                      , r1.uploaded_at
                                      , r1.revised_at
                                      , array_agg(pc2.component_name) FILTER (WHERE pc2.component_name IS NOT NULL) AS executables

     FROM packages AS p0
          INNER JOIN releases AS r1 ON p0.package_id = r1.package_id
          LEFT OUTER JOIN package_components AS pc2 ON pc2.release_id = r1.release_id
                                             AND pc2.component_type = 'executable'
     WHERE p0.status = 'fully-imported'
       AND r1.deprecated IS DISTINCT FROM TRUE
     GROUP BY p0.namespace, p0.name, synopsis, p0.package_id, r1.version, license, r1.uploaded_at, r1.revised_at
     ORDER BY p0.package_id
            , version DESC

CREATE UNIQUE INDEX
  ON latest_versions ( name
                     , namespace
                     , version
                     )

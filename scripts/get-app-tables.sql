SELECT table_name
FROM information_schema.tables
WHERE table_name <> 'schema_migrations'
  AND table_type = 'BASE TABLE'
  AND EXISTS (SELECT TRUE
              FROM unnest(current_schemas(FALSE)) AS cs
              WHERE cs = table_schema)
ORDER BY table_name ASC

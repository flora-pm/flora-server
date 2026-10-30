CREATE TYPE system_library_kind AS ENUM ('pkgconfig', 'extra-library', 'framework');

CREATE TABLE IF NOT EXISTS system_libraries (
    system_library_id uuid PRIMARY KEY
  , kind system_library_kind NOT NULL
  , name text NOT NULL
  , UNIQUE (kind, name)
);

CREATE TABLE IF NOT EXISTS system_requirements (
    system_requirement_id uuid PRIMARY KEY
  , package_component_id uuid REFERENCES package_components NOT NULL
  , system_library_id uuid REFERENCES system_libraries NOT NULL
  , requirement text
  , condition jsonb
  , UNIQUE NULLS NOT DISTINCT (package_component_id, system_library_id, condition)
);

CREATE INDEX system_requirements_system_library_id_fkey
  ON system_requirements (system_library_id);

CREATE TABLE index_dependencies (
    index_dependency_id uuid PRIMARY KEY
  , dependent uuid REFERENCES package_indexes (package_index_id) NOT NULL
  , dependency uuid REFERENCES package_indexes (package_index_id) NOT NULL
  , priority integer NOT NULL
  , CONSTRAINT positive_priority CHECK (priority > 0)
);

CREATE INDEX index_dependencies_dependent_fkey
  ON index_dependencies (dependent);

CREATE INDEX index_dependencies_dependency_fkey
  ON index_dependencies (dependency)

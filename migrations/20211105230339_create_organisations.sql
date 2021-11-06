CREATE TABLE IF NOT EXISTS organisations (
    organisation_id uuid PRIMARY KEY,
    organisation_name TEXT NOT NULL,
	created_at TIMESTAMPTZ NOT NULL,
	updated_at TIMESTAMPTZ NOT NULL
);

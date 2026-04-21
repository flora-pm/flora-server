ALTER TABLE releases ADD COLUMN uploader_id uuid REFERENCES package_uploaders (package_uploader_id);

CREATE INDEX ON releases(uploader_id);

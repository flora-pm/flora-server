CREATE TABLE IF NOT EXISTS user_organisation (
    user_organisation_id uuid PRIMARY KEY,
    user_id uuid NOT NULL,
    organisation_id uuid NOT NULL,
    is_admin bool NOT NULL,
    CONSTRAINT user_organisation_fk0 FOREIGN KEY ("user_id")
        REFERENCES "users"("user_id"),
    CONSTRAINT user_organisation_fk1 FOREIGN KEY ("organisation_id")
        REFERENCES "organisations"("organisation_id")
);

CREATE INDEX user_organisation_admin ON user_organisation (is_admin);

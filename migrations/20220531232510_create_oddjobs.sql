-- migrate:up
CREATE FUNCTION notify_job_monitor_for_oddjobs ()
    RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM
        pg_notify(
            '"jobs_created_oddjobs"',
            json_build_object(
                'id', NEW.id,
                'run_at', NEW.run_at,
                'locked_at', NEW.locked_at
            )::text
        );
    RETURN new;
END;
$$;

CREATE TABLE oddjobs (
    id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL,
    status text NOT NULL,
    payload jsonb NOT NULL,
    last_error jsonb,
    attempts integer DEFAULT 0 NOT NULL,
    locked_at timestamp with time zone,
    locked_by text,
    CONSTRAINT incorrect_locking_info
    CHECK ((((status <> 'locked'::text)
        AND (locked_at IS NULL)
        AND (locked_by IS NULL))
        OR ((status = 'locked'::text)
        AND (locked_at IS NOT NULL)
        AND (locked_by IS NOT NULL))))
);

CREATE SEQUENCE oddjobs_id_seq
    AS integer START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE oddjobs_id_seq OWNED BY oddjobs.id;

ALTER TABLE ONLY oddjobs
    ALTER COLUMN id SET DEFAULT nextval('oddjobs_id_seq'::regclass);

ALTER TABLE ONLY oddjobs
    ADD CONSTRAINT oddjobs_pkey PRIMARY KEY (id);

CREATE INDEX idx_oddjobs_created_at ON oddjobs USING btree (created_at);

CREATE INDEX idx_oddjobs_locked_at ON oddjobs USING btree (locked_at);

CREATE INDEX idx_oddjobs_locked_by ON oddjobs USING btree (locked_by);

CREATE INDEX idx_oddjobs_run_at ON oddjobs USING btree (run_at);

CREATE INDEX idx_oddjobs_status ON oddjobs USING btree (status);

CREATE INDEX idx_oddjobs_updated_at ON oddjobs USING btree (updated_at);

CREATE TRIGGER trg_notify_job_monitor_for_oddjobs
    AFTER INSERT ON oddjobs
    FOR EACH ROW
    EXECUTE FUNCTION notify_job_monitor_for_oddjobs ();

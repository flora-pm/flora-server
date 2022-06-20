--
-- Name: notify_job_monitor_for_oddjobs(); Type: FUNCTION; Schema: public; Owner: postgres
--
CREATE FUNCTION public.notify_job_monitor_for_oddjobs ()
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

ALTER FUNCTION public.notify_job_monitor_for_oddjobs () OWNER TO postgres;

--
-- Name: oddjobs; Type: TABLE; Schema: public; Owner: postgres
--
CREATE TABLE public.oddjobs (
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

ALTER TABLE public.oddjobs OWNER TO postgres;

--
-- Name: oddjobs_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--
CREATE SEQUENCE public.oddjobs_id_seq
    AS integer START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE public.oddjobs_id_seq OWNER TO postgres;

--
-- Name: oddjobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--
ALTER SEQUENCE public.oddjobs_id_seq OWNED BY public.oddjobs.id;

--
-- Name: oddjobs id; Type: DEFAULT; Schema: public; Owner: postgres
--
ALTER TABLE ONLY public.oddjobs
    ALTER COLUMN id SET DEFAULT nextval('public.oddjobs_id_seq'::regclass);

-- Name: oddjobs oddjobs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--
ALTER TABLE ONLY public.oddjobs
    ADD CONSTRAINT oddjobs_pkey PRIMARY KEY (id);

--
-- Name: idx_oddjobs_created_at; Type: INDEX; Schema: public; Owner: postgres
--
CREATE INDEX idx_oddjobs_created_at ON public.oddjobs USING btree (created_at);

--
-- Name: idx_oddjobs_locked_at; Type: INDEX; Schema: public; Owner: postgres
--
CREATE INDEX idx_oddjobs_locked_at ON public.oddjobs USING btree (locked_at);

--
-- Name: idx_oddjobs_locked_by; Type: INDEX; Schema: public; Owner: postgres
--
CREATE INDEX idx_oddjobs_locked_by ON public.oddjobs USING btree (locked_by);

--
-- Name: idx_oddjobs_run_at; Type: INDEX; Schema: public; Owner: postgres
--
CREATE INDEX idx_oddjobs_run_at ON public.oddjobs USING btree (run_at);

--
-- Name: idx_oddjobs_status; Type: INDEX; Schema: public; Owner: postgres
--
CREATE INDEX idx_oddjobs_status ON public.oddjobs USING btree (status);

--
-- Name: idx_oddjobs_updated_at; Type: INDEX; Schema: public; Owner: postgres
--
CREATE INDEX idx_oddjobs_updated_at ON public.oddjobs USING btree (updated_at);

--
-- Name: oddjobs trg_notify_job_monitor_for_oddjobs; Type: TRIGGER; Schema: public; Owner: postgres
--
CREATE TRIGGER trg_notify_job_monitor_for_oddjobs
    AFTER INSERT ON public.oddjobs
    FOR EACH ROW
    EXECUTE FUNCTION public.notify_job_monitor_for_oddjobs ();

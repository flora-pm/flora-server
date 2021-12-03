SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: packages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.packages (
    package_id uuid NOT NULL,
    namespace text NOT NULL,
    name text NOT NULL,
    synopsis text NOT NULL,
    owner_id uuid,
    metadata jsonb NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


--
-- Name: releases; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.releases (
    release_id uuid NOT NULL,
    package_id uuid,
    version text NOT NULL,
    archive_checksum text NOT NULL,
    created_at timestamp with time zone,
    updated_at timestamp with time zone
);


--
-- Name: requirements; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.requirements (
    requirement_id uuid NOT NULL,
    release_id uuid,
    package_id uuid,
    requirement text
);


--
-- Name: dependents; Type: MATERIALIZED VIEW; Schema: public; Owner: -
--

CREATE MATERIALIZED VIEW public.dependents AS
 SELECT DISTINCT p3.name,
    p3.namespace,
    p0.package_id AS dependent_id
   FROM (((public.packages p0
     JOIN public.releases r1 ON ((r1.package_id = p0.package_id)))
     JOIN public.requirements r2 ON ((r2.release_id = r1.release_id)))
     JOIN public.packages p3 ON ((p3.package_id = r2.package_id)))
  WITH NO DATA;


--
-- Name: downloads; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.downloads (
    download_id integer NOT NULL,
    release_id uuid,
    downloads integer,
    day timestamp with time zone
);


--
-- Name: downloads_download_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.downloads_download_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: downloads_download_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.downloads_download_id_seq OWNED BY public.downloads.download_id;


--
-- Name: organisations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organisations (
    organisation_id uuid NOT NULL,
    organisation_name text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


--
-- Name: package_publishers; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_publishers (
    package_publisher_id uuid NOT NULL,
    package_id uuid NOT NULL,
    user_id uuid NOT NULL
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: user_organisation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_organisation (
    user_organisation_id uuid NOT NULL,
    user_id uuid NOT NULL,
    organisation_id uuid NOT NULL,
    is_admin boolean NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    user_id uuid NOT NULL,
    username text,
    display_name text,
    email text,
    password text,
    created_at timestamp with time zone,
    updated_at timestamp with time zone
);


--
-- Name: downloads download_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.downloads ALTER COLUMN download_id SET DEFAULT nextval('public.downloads_download_id_seq'::regclass);


--
-- Name: downloads downloads_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.downloads
    ADD CONSTRAINT downloads_pkey PRIMARY KEY (download_id);


--
-- Name: organisations organisations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organisations
    ADD CONSTRAINT organisations_pkey PRIMARY KEY (organisation_id);


--
-- Name: package_publishers package_publishers_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_publishers
    ADD CONSTRAINT package_publishers_pkey PRIMARY KEY (package_publisher_id);


--
-- Name: packages packages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.packages
    ADD CONSTRAINT packages_pkey PRIMARY KEY (package_id);


--
-- Name: releases releases_package_id_version_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.releases
    ADD CONSTRAINT releases_package_id_version_key UNIQUE (package_id, version);


--
-- Name: releases releases_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.releases
    ADD CONSTRAINT releases_pkey PRIMARY KEY (release_id);


--
-- Name: requirements requirements_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.requirements
    ADD CONSTRAINT requirements_pkey PRIMARY KEY (requirement_id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: user_organisation user_organisation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_organisation
    ADD CONSTRAINT user_organisation_pkey PRIMARY KEY (user_organisation_id);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (user_id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: dependents_name_dependent_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX dependents_name_dependent_id_idx ON public.dependents USING btree (name, dependent_id);


--
-- Name: dependents_name_namespace_dependent_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX dependents_name_namespace_dependent_id_idx ON public.dependents USING btree (name, namespace, dependent_id);


--
-- Name: downloads_day_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX downloads_day_idx ON public.downloads USING btree (day);


--
-- Name: downloads_release_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX downloads_release_id_idx ON public.downloads USING btree (release_id);


--
-- Name: packages_lower_lower1_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX packages_lower_lower1_idx ON public.packages USING btree (lower(name), lower(namespace));


--
-- Name: releases_package_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX releases_package_id_idx ON public.releases USING btree (package_id);


--
-- Name: requirements_release_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX requirements_release_id_idx ON public.requirements USING btree (release_id);


--
-- Name: user_organisation_admin; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_organisation_admin ON public.user_organisation USING btree (is_admin);


--
-- Name: users_lower_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_lower_idx ON public.users USING btree (lower(username));


--
-- Name: downloads downloads_release_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.downloads
    ADD CONSTRAINT downloads_release_id_fkey FOREIGN KEY (release_id) REFERENCES public.releases(release_id);


--
-- Name: package_publishers package_publishers_package_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_publishers
    ADD CONSTRAINT package_publishers_package_id_fkey FOREIGN KEY (package_id) REFERENCES public.packages(package_id);


--
-- Name: package_publishers package_publishers_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_publishers
    ADD CONSTRAINT package_publishers_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(user_id);


--
-- Name: packages packages_owner_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.packages
    ADD CONSTRAINT packages_owner_id_fkey FOREIGN KEY (owner_id) REFERENCES public.users(user_id);


--
-- Name: releases releases_package_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.releases
    ADD CONSTRAINT releases_package_id_fkey FOREIGN KEY (package_id) REFERENCES public.packages(package_id);


--
-- Name: requirements requirements_package_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.requirements
    ADD CONSTRAINT requirements_package_id_fkey FOREIGN KEY (package_id) REFERENCES public.packages(package_id);


--
-- Name: requirements requirements_release_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.requirements
    ADD CONSTRAINT requirements_release_id_fkey FOREIGN KEY (release_id) REFERENCES public.releases(release_id);


--
-- Name: user_organisation user_organisation_fk0; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_organisation
    ADD CONSTRAINT user_organisation_fk0 FOREIGN KEY (user_id) REFERENCES public.users(user_id);


--
-- Name: user_organisation user_organisation_fk1; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_organisation
    ADD CONSTRAINT user_organisation_fk1 FOREIGN KEY (organisation_id) REFERENCES public.organisations(organisation_id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20211105225100'),
    ('20211105230339'),
    ('20211105230340'),
    ('20211106003401'),
    ('20211106120712'),
    ('20211106123053'),
    ('20211106123253'),
    ('20211106140016'),
    ('20211113195849');

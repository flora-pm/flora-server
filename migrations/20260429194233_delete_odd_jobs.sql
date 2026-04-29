DROP TABLE IF EXISTS oddjobs;

DROP FUNCTION IF EXISTS notify_job_monitor_for_oddjobs;

DROP SEQUENCE IF EXISTS oddjobs_id_seq;

DROP TRIGGER IF EXISTS trg_notify_job_monitor_for_oddjobsb;

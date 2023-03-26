-- migrate:up

alter table packages
    add metadata jsonb default '{}'; -- { deprecation }

-- migrate:down
-- alter table packages
--     drop metadata;

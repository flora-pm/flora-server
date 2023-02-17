-- migrate:up

alter table packages
    add metadata jsonb; -- { deprecation }

-- migrate:down
-- alter table packages
--     drop metadata;

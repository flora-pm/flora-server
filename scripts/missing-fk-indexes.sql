-- ® Marc Cousin 2024
DO
$$
DECLARE
    numversion int;
    indkeysfilter varchar;
    indexquery varchar;
    missing_indexes varchar;
BEGIN
    SELECT INTO numversion setting FROM pg_settings WHERE name = 'server_version_num';
    -- manage covering indexes in PG 11
    IF numversion >= 110000 THEN
        RAISE DEBUG 'numversion %',numversion;
        indkeysfilter := '((indkey::int4[])[0:indnkeyatts-1])[1:array_upper(conkey,1)]';
    ELSE
        indkeysfilter := '((indkey::int4[]))[0:array_upper(conkey,1) - 1]';
    END IF;


    indexquery := format('
with not_indexed_constraints as (
        select conname, conrelid::regclass as tablename, conkey
        from pg_constraint
        where contype = ''f''
          and not exists (
                 select 1
                 from pg_index
                 where indrelid=conrelid
                   and %s @> conkey::int4[]
                   and %s <@ conkey::int4[]
                   and indpred is null
              )
          and not exists (
                 select 1 from pg_depend
                 where objid = conrelid and classid = ''pg_class''::regclass and deptype = ''e''
          )
     ),
     unnested_constraints as (
        select conname, tablename, unnest.* FROM not_indexed_constraints,unnest(conkey) with ordinality),
     missing_indexes as (
SELECT ''CREATE INDEX CONCURRENTLY '' || conname || '' ON '' || tablename::text || ''('' ||
       string_agg(quote_ident(attname::text), '','' order by ordinality) || '');'' as indexes
from unnested_constraints
join  pg_attribute on (unnested_constraints.tablename=pg_attribute.attrelid
                   and pg_attribute.attnum=unnested_constraints.unnest)
group by tablename,conname)
     SELECT string_agg(indexes,E''\n'') as indexes from missing_indexes', indkeysfilter, indkeysfilter);

    EXECUTE indexquery INTO missing_indexes;
    IF length(missing_indexes) > 0 THEN
        RAISE 'Missing FK indexes: %',missing_indexes;
    END IF;
END;
$$ LANGUAGE plpgsql;

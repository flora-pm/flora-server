-- © Marc Cousin
WITH -- get predicates (WHERE clause) definition in text format (ugly but the parsed version can differ even if the predicate is the same)
     -- ignore functional indexes at the same time, that would make this query very ugly
     indexdata1 AS (SELECT *
                         , ((regexp_match(pg_get_indexdef(indexrelid)
                                        , 'WHERE (.*)$')))[1] AS preddef
                    FROM pg_index
                    WHERE indexprs IS NULL)
     -- add the rest of metadata and do the join
   , indexdata2 AS (SELECT t1.*
                         , pg_get_indexdef(t1.indexrelid) AS contained
                         , pg_get_indexdef(t2.indexrelid) AS contains
                         , array_to_string(t1.indkey, '+') AS colindex
                         , array_to_string(t2.indkey, '+') AS colotherindex
                         , t2.indexrelid AS other_index
                         , t2.indisunique AS other_indisunique
                         , t2.preddef AS other_preddef
                    -- cross join all indexes on the same table to try all combination (except oneself)
                    FROM indexdata1 AS t1
                         INNER JOIN indexdata1 AS t2 ON t1.indrelid = t2.indrelid
                                                    AND t1.indexrelid <> t2.indexrelid)

  SELECT contained
       , contains
  FROM indexdata2
    -- The indexes are the same or the "other" is larger than us
  WHERE (colotherindex = colindex
      OR colotherindex LIKE colindex || '+%')
    -- and we have the same predicate
    AND other_preddef IS NOT DISTINCT FROM preddef
    -- and either the index is not unique, or we're both unique with the same definition
    AND (NOT indisunique)
      OR (
              indisunique
          AND other_indisunique
          AND colindex = colotherindex
      );

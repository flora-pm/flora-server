CREATE TABLE IF NOT EXISTS security_advisories (
  security_advisory_id uuid primary key,
  hsec_id text not null,
  modified timestamptz not null,
  published timestamptz not null,
  capecs int[] not null,
  cwes int[] not null,
  keywords text[] not null,
  aliases text[] not null,
  related text[] not null,
  references jsonb not null,
  pandoc jsonb not null,
  html text not null,
  summary text not null,
  details text not null
)

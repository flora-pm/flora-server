create table persistent_sessions (
  persistent_session_id uuid primary key,
  user_id uuid references users,
  session_data jsonb not null,
  created_at timestamptz not null
);

create unique index on persistent_sessions(persistent_session_id, user_id);
create index on persistent_sessions(user_id);

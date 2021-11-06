CREATE TABLE users (
  user_id UUID PRIMARY KEY,
  username TEXT unique,
  display_name TEXT,
  email TEXT unique,
  password TEXT,
  created_at timestamptz,
  updated_at timestamptz
);

create unique index on users (lower(username));

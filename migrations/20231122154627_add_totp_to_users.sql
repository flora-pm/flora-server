alter table users
  add column totp_key text,
  add column totp_enabled boolean not null;

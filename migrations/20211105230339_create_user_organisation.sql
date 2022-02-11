create table if not exists user_organisation (
    user_organisation_id uuid primary key,
    user_id uuid references users,
    organisation_id uuid references organisations,
    is_admin bool not null
);

create index user_organisation_admin on user_organisation (is_admin);

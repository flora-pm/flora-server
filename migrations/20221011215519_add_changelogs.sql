alter type readme_status
  rename to import_status;

alter table releases
  add column changelog text,
  add column changelog_status import_status;

alter table releases
  add constraint consistent_readme_status
    check (
         ((readme_status = 'imported' or readme_status = 'inexistent')
            and readme is not null)
      or (readme_status = 'not-imported' and readme is null)
    ),
  add constraint consistent_changelog_status
    check (
         ((changelog_status = 'imported' or changelog_status = 'inexistent')
            and changelog is not null)
      or (changelog_status = 'not-imported' and changelog is null)
    );

create index on releases(changelog_status);

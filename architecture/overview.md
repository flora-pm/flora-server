# Project architecture

## Layers

### Database model layer

The files of our data model follow a predictable layout.

`Flora.Model` contains directories where the database entities live. They have standardised files within them:

```
.
└─ Flora.Model.*
   ├── Guard.hs  ← monadic guards for easy access to resources
   ├── Query.hs  ← read-only database queries
   ├── Types.hs  ← type definitions, class instances, smart constructors
   └── Update.hs ← read-write database statements
```

### Business logic layer

Business logic lives under `Flora.Domain`, with in particular:

```
.
└─ Flora.Domain
   ├── Flora.Domain.Package ← package/release resolution
   ├── Flora.Domain.Release ← pure release rules, e.g. `latestViableRelease`
   ├── Flora.Domain.Search  ← search over the model (own `flora-search` library)
   └── Flora.Domain.Import  ← the package import pipeline
```

These modules use the database layer to expose re-usable functions and logic, which can be shared
between the Pages and API controllers

The import pipeline has its own document: [The package import pipeline](./import-pipeline.md).

### Web layer

Route definitions and implementation are located in the `FloraWeb` namespace, with in particular:

```
.
└─ FloraWeb
   ├── FloraWeb.API   ← JSON API
   ├── FloraWeb.Atom  ← Atom Feed
   └── FloraWeb.Pages ← HTML pages
```

## Wrapped entity IDs

We never use raw UUIDs for primary keys, but we wrap those UUIDs in smart constructors that will either generate one randomly,
or in certain cases generate the ID deterministically, based on immutable characteristics of the entity in question.

## Effects

Since Flora is a piece of software with lots of interactions with the outside world, we model those through an effect system,
which allows us to tag functions' type signatures. Effects are provided by most of the third-party libraries that we use
(logging, filesystem, etc.) and we have defined our own for database interactions (See the [Database](#database) section for more information).

## Database

### Schema migrations

Application schema migrations are kept as raw SQL in the `migrations/` directory. They are applied through the `flora-migrate` executable.
Arbiter – the job queue – generates its own schema migrations, which are applied in `flora-migrate` too.

### Read-Only and Read-Write operations

We differentiate read-only and read-write operations in the codebase through the usage of two different effects in type signatures: `ReadDB` and `WriteDB`.

Interpreting the `ReadDB` effect gives us access to read-only transactions, that are enforced in PostgreSQL by setting the transaction mode to "READ ONLY".
See [SET TRANSACTION][SET TRANSACTION] for more details.

The two effects do not mean two connections. `withReadWritePool` discharges both
`WriteDB` *and* `ReadDB` against a single pooled connection, so a read issued
from a write path joins that path's transaction rather than drawing a second
connection from the pool. Whatever you run inside one `withReadWritePool` block
costs exactly one connection, however many reads it does.

[SET TRANSACTION]: https://www.postgresql.org/docs/current/sql-set-transaction.html


> -- | The sql dialect type which is used by the parser and the pretty
> -- printer so has its own module.
> module Database.HsSqlPpp.Dialect
>     (Dialect
>     ,postgresDialect
>     ,sqlServerDialect
>     ,oracleDialect) where

> import Database.HsSqlPpp.Internals.Dialect

make sort of compatible with 0.6.x

> postgresDialect :: Dialect
> postgresDialect = PostgreSQLDialect

> sqlServerDialect :: Dialect
> sqlServerDialect = SQLServerDialect

> oracleDialect :: Dialect
> oracleDialect = OracleDialect

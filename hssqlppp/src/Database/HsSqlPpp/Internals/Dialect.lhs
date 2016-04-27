
> -- | The sql dialect type which is used by the parser and the pretty
> -- printer so has its own module.
> module Database.HsSqlPpp.Internals.Dialect where

more dialect options will be added here

> -- | The dialect of SQL to use.
> data Dialect = PostgreSQLDialect
>              | SQLServerDialect
>              | OracleDialect
>                deriving (Show,Eq)

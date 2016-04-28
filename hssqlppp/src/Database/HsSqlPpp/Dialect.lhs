
> -- | The sql dialect type which is used by the parser and the pretty
> -- printer so has its own module.
> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.Dialect
>     (Dialect(..)
>     ,SyntaxFlavour(..)
>     ,ansiDialect
>     ,postgresDialect
>     ,sqlServerDialect
>     ,oracleDialect
>     ,canonicalizeTypeName
>     ,ansiTypeNameToDialect
>     ) where

> import Database.HsSqlPpp.Internals.Dialect
> import Data.Data
> import Data.Text (Text)

make sort of compatible with 0.6.x

> data SyntaxFlavour = Ansi | Postgres | SqlServer | Oracle
>                      deriving (Eq,Show,Data,Typeable)

> ansiDialect :: Dialect
> ansiDialect = PostgreSQLDialect


> postgresDialect :: Dialect
> postgresDialect = PostgreSQLDialect

> sqlServerDialect :: Dialect
> sqlServerDialect = SQLServerDialect

> oracleDialect :: Dialect
> oracleDialect = OracleDialect

> ansiTypeNameToDialect :: Dialect -> Text -> Maybe Text
> ansiTypeNameToDialect = error "ansiTypeNameToDialect not supported in this version"

> canonicalizeTypeName :: Dialect -> Text -> Text
> canonicalizeTypeName = error "ansiTypeNameToDialect not supported in this version"

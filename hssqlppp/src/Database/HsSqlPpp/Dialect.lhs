
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
> --import Data.Data
> import Data.Text (Text)

> ansiDialect :: Dialect
> ansiDialect = emptyDialect {diName = "ansi", diSyntaxFlavour=Postgres}

> sqlServerDialect :: Dialect
> sqlServerDialect = emptyDialect {diName = "sqlserver", diSyntaxFlavour=SqlServer}

> oracleDialect :: Dialect
> oracleDialect = emptyDialect {diName = "oracle", diSyntaxFlavour=Oracle}

> ansiTypeNameToDialect :: Dialect -> Text -> Maybe Text
> ansiTypeNameToDialect = error "ansiTypeNameToDialect not supported in this version"

> canonicalizeTypeName :: Dialect -> Text -> Text
> canonicalizeTypeName = error "canonicalizeTypeName not supported in this version"

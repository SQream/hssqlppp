
This is the public module for the type checking functionality.

> {- | Contains functions for typechecking sql asts.
> -}
> module Database.HsSqlPpp.TypeCheck
>     (
>      -- * typechecking/ annotation functions
>      typeCheckStatements
>     ,typeCheckParameterizedStatement
>     ,typeCheckQueryExpr
>     ,typeCheckScalarExpr
>     ,typeCheckScalarExprEnv
>     ,TypeCheckingFlags(..)
>     ,module Database.HsSqlPpp.Dialect
>     ,defaultTypeCheckingFlags
>     ) where
>
> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Dialect
> --import Database.HsSqlPpp.Internals.TypeChecking.Utils
> --import Database.HsSqlPpp.Internals.AstAnnotation

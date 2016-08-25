
The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Roles (roles) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> roles :: Item
> roles =
>   Group "roleTests"
>   [Group "SET ROLE"
>     [s "SET ROLE my_role;"
>      [SetRole ea $ RoleName $ Nmc "my_role"]
>     ]
>   ,Group "RESET ROLE"
>     [s "RESET ROLE;"
>      [ResetRole ea]
>     ]
>   ,Group "CREATE ROLE"
>     [s "CREATE ROLE my_role;"
>      [CreateRole ea $ RoleName $ Nmc "my_role"]
>     ]
>   ,Group "ALTER ROLE"
>     [s "ALTER ROLE my_role RENAME TO other_role;"
>      [AlterRole ea (RoleName $ Nmc "my_role") (RoleName $ Nmc "other_role")]
>     ]
>   ,Group "DROP ROLE"
>     [s "DROP ROLE my_role;"
>      [DropSomething ea Role Require [name "my_role"] Restrict]
>     ]
>    ]
>  where
>    s = Stmt


The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.AlterDefault (alterDefault) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> alterDefault :: Item
> alterDefault =
>   Group "alterDefaultTest"
>   [Group "Role"
>     [Stmt "ALTER DEFAULT ROLE FOR my_role TO other_role;"
>      [AlterDefaultRole ea (RoleName $ Nmc "my_role") (RoleName $ Nmc "other_role")]
>     ]
>   ,Group "Schema"
>     [Stmt "ALTER DEFAULT SCHEMA FOR my_role TO a_schema;"
>      [AlterDefaultSchema ea (RoleName $ Nmc "my_role") (name "a_schema")]
>     ]
>   ,Group "Current Schema"
>     [Stmt "ALTER CURRENT DEFAULT SCHEMA TO a_schema;"
>      [AlterCurrentDefaultSchema ea (name "a_schema")]
>     ]

>     ,Group "Alter Default Permissions"
>       [Stmt "ALTER DEFAULT PERMISSIONS FOR DATABASES GRANT USAGE, SET_PERMISSIONS TO role1, role2, role3;"
>        [AlterDefaultPermissions ea [] [] [Databases] [PrivUsage, PrivSetPermissions] [RoleName $ Nmc "role1", RoleName $ Nmc "role2", RoleName $ Nmc "role3"]]

>       ,Stmt "ALTER DEFAULT PERMISSIONS IN my_schema FOR VIEWS GRANT SELECT, INSERT, DDL TO role1, role2, role3;"
>        [AlterDefaultPermissions ea [] [name "my_schema"] [Views] [PrivSelect, PrivInsert, PrivDDL] [RoleName $ Nmc "role1", RoleName $ Nmc "role2", RoleName $ Nmc "role3"]]

>       ,Stmt "ALTER DEFAULT PERMISSIONS IN my_schema FOR TABLES GRANT SELECT, INSERT, DDL TO CURRENT_ROLE, role2, role3;"
>        [AlterDefaultPermissions ea [] [name "my_schema"] [Tables] [PrivSelect, PrivInsert, PrivDDL] [CurrentRole, RoleName $ Nmc "role2", RoleName $ Nmc "role3"]]

>       ,Stmt "ALTER DEFAULT PERMISSIONS FOR role1, role2, CURRENT_ROLE IN my_schema FOR TABLES GRANT SELECT, INSERT, DDL TO SESSION_ROLE, role2, role3;"
>        [AlterDefaultPermissions ea [RoleName $ Nmc "role1", RoleName $ Nmc "role2", CurrentRole] [name "my_schema"] [Tables] [PrivSelect, PrivInsert, PrivDDL] [SessionRole, RoleName $ Nmc "role2", RoleName $ Nmc "role3"]]
>       ]

>    ]


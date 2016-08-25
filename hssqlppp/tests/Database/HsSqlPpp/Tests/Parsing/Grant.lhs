

The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Grant (grantRevoke) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> grantRevoke :: Item
> grantRevoke = Group "grantRevokeTest"

>   [Group "Grant Role"

>     [Stmt "GRANT role1, role2 TO role3, current_role, session_role WITH INHERIT;"
>      [GrantRole ea [RoleName $ Nmc "role1", RoleName $ Nmc "role2"] [RoleName $ Nmc "role3", CurrentRole, SessionRole] Inherit]

>     ,Stmt "GRANT current_role TO role1;"
>      [GrantRole ea [CurrentRole] [RoleName $ Nmc "role1"] Inherit]

>     ,Stmt "GRANT role1 TO role2 WITH NOINHERIT;"
>      [GrantRole ea [RoleName $ Nmc "role1"] [RoleName $ Nmc "role2"] NoInherit]
>     ]

>   ,Group "Revoke Role"

>     [Stmt "REVOKE role1, role2 FROM role3, current_role, session_role;"
>      [RevokeRole ea [RoleName $ Nmc "role1", RoleName $ Nmc "role2"] [RoleName $ Nmc "role3", CurrentRole, SessionRole]]

>     ,Stmt "REVOKE current_role FROM role1;"
>      [RevokeRole ea [CurrentRole] [RoleName $ Nmc "role1"]]
>     ]

>   ,Group "Grant attrs"

>     [Stmt "GRANT SUPERUSER, ROLEADMIN, LOGIN, PASSWORD pass1234, CONNECTION_LIMIT 555 TO role3, current_role, session_role;"
>      [GrantPermissionCluster ea [PrivSuperUser, PrivRoleAdmin, PrivLogin, PrivPassword "pass1234", PrivConnectionLimit 555] [RoleName $ Nmc "role3", CurrentRole, SessionRole]]

>     ,Stmt "REVOKE SUPERUSER, ROLEADMIN, LOGIN, PASSWORD, CONNECTION_LIMIT FROM role3, current_role, session_role;"
>      [RevokePermissionCluster ea [PrivSuperUser, PrivRoleAdmin, PrivLogin, PrivPassword "", PrivConnectionLimit 0] [RoleName $ Nmc "role3", CurrentRole, SessionRole]]

>     ]

>   ]

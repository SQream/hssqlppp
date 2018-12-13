

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

>     [Stmt "GRANT SUPERUSER, ROLEADMIN, LOGIN, PASSWORD 'pass1234', CONNECTION_LIMIT 555, CREATE FUNCTION TO role3, current_role, session_role;"
>      [GrantPermissionCluster ea [PrivSuperUser, PrivRoleAdmin, PrivLogin, PrivPassword "pass1234", PrivConnectionLimit 555, PrivCreateFunction] [RoleName $ Nmc "role3", CurrentRole, SessionRole]]

>     ,Stmt "REVOKE SUPERUSER, ROLEADMIN, LOGIN, PASSWORD, CONNECTION_LIMIT, CREATE FUNCTION FROM role3, current_role, session_role;"
>      [RevokePermissionCluster ea [PrivSuperUser, PrivRoleAdmin, PrivLogin, PrivPassword "", PrivConnectionLimit 0, PrivCreateFunction] [RoleName $ Nmc "role3", CurrentRole, SessionRole]]

>     ]

>   ,Group "Grant Table"

>     [Stmt "GRANT SELECT, INSERT, DELETE, DDL ON TABLE s1.t1,s2.t2,t3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivSelect, PrivInsert, PrivDelete, PrivDDL]
>        (PrivTable [name2 "s1" "t1", name2 "s2" "t2", name "t3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "GRANT ALL ON ALL TABLES IN SCHEMA s1, s2 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivAll]
>        (PrivAllTablesInSchema [name "s1", name "s2"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Revoke Table"

>     [Stmt "REVOKE SELECT, INSERT, DELETE, DDL ON TABLE s1.t1,s2.t2,t3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivSelect, PrivInsert, PrivDelete, PrivDDL]
>        (PrivTable [name2 "s1" "t1", name2 "s2" "t2", name "t3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "REVOKE ALL ON ALL TABLES IN SCHEMA s1, s2 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivAllTablesInSchema [name "s1", name "s2"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "REVOKE ALL PERMISSIONS ON ALL TABLES IN SCHEMA s1, s2 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivAllTablesInSchema [name "s1", name "s2"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Grant View"

>     [Stmt "GRANT SELECT, DDL ON VIEW s1.v1,s2.v2,v3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivSelect, PrivDDL]
>        (PrivView [name2 "s1" "v1", name2 "s2" "v2", name "v3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "GRANT ALL ON ALL VIEWS IN SCHEMA s1, s2 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivAll]
>        (PrivAllViewsInSchema [name "s1", name "s2"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "GRANT ALL PERMISSIONS ON ALL VIEWS IN SCHEMA s1, s2 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivAll]
>        (PrivAllViewsInSchema [name "s1", name "s2"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Revoke View"

>     [Stmt "REVOKE SELECT, DDL ON VIEW s1.v1,s2.v2,v3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivSelect, PrivDDL]
>        (PrivView [name2 "s1" "v1", name2 "s2" "v2", name "v3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "REVOKE ALL ON ALL VIEWS IN SCHEMA s1, s2 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivAllViewsInSchema [name "s1", name "s2"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ]


>   ,Group "Grant Saved Query"

>     [Stmt "GRANT SELECT, DDL ON SAVED_QUERY sq1,sq2,sq3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivSelect, PrivDDL]
>        (PrivSavedQuery [name "sq1", name "sq2", name "sq3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ]

>   ,Group "Revoke Saved Query"

>     [Stmt "REVOKE SELECT, DDL ON SAVED_QUERY sq1,sq2,sq3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivSelect, PrivDDL]
>        (PrivSavedQuery [name "sq1", name "sq2", name "sq3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ]


>   ,Group "Grant Database"

>     [Stmt "GRANT CREATE, CONNECT, DDL, SET_PERMISSIONS, SUPERUSER ON DATABASE db1,db2,db3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivCreate, PrivConnect, PrivDDL, PrivSetPermissions, PrivSuperUser]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "GRANT ALL ON DATABASE db1,db2,db3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivAll]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "GRANT SUPERUSER ON DATABASE db1,db2,db3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivSuperUser]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "REVOKE SUPERUSER ON DATABASE db1,db2,db3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivSuperUser]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Revoke Database"

>     [Stmt "REVOKE CREATE, CONNECT, DDL, SET_PERMISSIONS, SUPERUSER ON DATABASE db1,db2,db3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivCreate, PrivConnect, PrivDDL, PrivSetPermissions, PrivSuperUser]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "REVOKE ALL ON DATABASE db1,db2,db3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]

>     ,Stmt "REVOKE ALL PERMISSIONS ON DATABASE db1,db2,db3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivDB [name "db1", name "db2", name "db3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Grant Schema"

>     [Stmt "GRANT CREATE, DDL, USAGE, SET_PERMISSIONS, SUPERUSER ON SCHEMA s1,s2,s3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivCreate, PrivDDL, PrivUsage, PrivSetPermissions, PrivSuperUser]
>        (PrivSchema [name "s1", name "s2", name "s3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "GRANT ALL ON SCHEMA s1,s2,s3 TO role1, current_role, session_role;"
>      [GrantPermission ea
>        [PrivAll]
>        (PrivSchema [name "s1", name "s2", name "s3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Revoke Schema"

>     [Stmt "REVOKE CREATE, DDL, USAGE, SET_PERMISSIONS, SUPERUSER ON SCHEMA s1,s2,s3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivCreate, PrivDDL, PrivUsage, PrivSetPermissions, PrivSuperUser]
>        (PrivSchema [name "s1", name "s2", name "s3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "REVOKE ALL ON SCHEMA s1,s2,s3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivSchema [name "s1", name "s2", name "s3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ,Stmt "REVOKE ALL PERMISSIONS ON SCHEMA s1,s2,s3 FROM role1, current_role, session_role;"
>      [RevokePermission ea
>        [PrivAll]
>        (PrivSchema [name "s1", name "s2", name "s3"])
>        [RoleName $ Nmc "role1", CurrentRole, SessionRole]
>      ]
>     ]

>   ,Group "Grant Function"

>     [Stmt "GRANT ALL ON FUNCTION f1,f2 TO role1;"
>      [GrantPermission ea
>        [PrivAll]
>        (PrivFunction [name "f1",name "f2"])
>        [RoleName $ Nmc "role1"]
>      ]
>     ,Stmt "GRANT EXECUTE ON ALL FUNCTIONS TO role1;"
>      [GrantPermission ea
>        [PrivExecute]
>        PrivAllFunctions
>        [RoleName $ Nmc "role1"]
>      ]
>     ,Stmt "REVOKE DDL,EXECUTE ON FUNCTION f1 FROM role1,current_role;"
>      [RevokePermission ea
>        [PrivDDL,PrivExecute]
>        (PrivFunction [name "f1"])
>        [RoleName $ Nmc "role1", CurrentRole]
>      ]
>     ]



>   ]



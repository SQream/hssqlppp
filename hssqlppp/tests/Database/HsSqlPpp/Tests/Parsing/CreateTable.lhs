
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.CreateTable (createTableTests) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> createTableTests :: Item
> createTableTests =
>   Group "create table" [
>      Group "simple tables" [
>       s "create table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [(createTable "test")
>         [att "fielda" "text"
>         ,att "fieldb" "int"]
>       ]
>      ,s "create table tbl (\n\
>         \  fld boolean default false);"
>       [createTable "tbl"
>        [AttributeDef ea (Nmc "fld") (st "boolean")
>                          (Just lFalse) [] []]]
>
>      ,s "create table tbl as select 1;"
>       [CreateTableAs ea (name "tbl") NoReplace
>        (makeSelect
>         {selSelectList = sl [si $ num "1"]})]
>
>      ,s "create table tbl  (\n\
>         \  fld int not null identity(1,1));"
>       [createTable "tbl"
>        [AttributeDef ea (Nmc "fld") (st "int")
>                          Nothing [NotNullConstraint ea "",
>                                   IdentityConstraint ea "" (Just(1,1))] []]]
>      ,s "create table tbl  (\n\
>         \  fld int not null identity(-1,-1));"
>       [createTable "tbl"
>        [AttributeDef ea (Nmc "fld") (st "int")
>                          Nothing [NotNullConstraint ea "",
>                                   IdentityConstraint ea "" (Just(-1,-1))] []]]
>
>      ,s "create table tbl  (\n\
>         \  fld int not null identity);"
>       [createTable "tbl"
>        [AttributeDef ea (Nmc "fld") (st "int")
>                          Nothing [NotNullConstraint ea "",
>                                   IdentityConstraint ea "" Nothing] []]]
>
>      ,s "alter table a rename to b;"
>       [AlterTable ea (name "a") $ RenameTable ea (name "b")]
>      ,s "alter table a rename column b to c;"
>       [AlterTable ea (name "a") $ RenameColumn ea (Nmc "b") (Nmc "c")]
>
>      ,s "alter table a add column b int;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AddColumn ea $ att "b" "int"]]
>
>      ,s "alter table a drop column b;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [DropColumn ea (Nmc "b")]]
>
>      ,s "alter table a alter column b set data type int;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetDataType ea (st "int")]]
>
>      ,s "alter table a alter column b set data type int;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetDataType ea (st "int")]]
>
>      ,s "alter table a alter column b set default 1;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetDefault ea (num "1")]]
>      ,s "alter table a alter column b drop default;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ DropDefault ea]]
>
>      ,s "alter table a alter column b set not null;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ SetNotNull ea]]
>      ,s "alter table a alter column b drop not null;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AlterColumn ea (Nmc "b") $ DropNotNull ea]]
>
>      ,s "alter table a add column b int,drop column c;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AddColumn ea $ att "b" "int"
>                                                        ,DropColumn ea (Nmc "c")]]
>
>      ,s "alter table a drop column b;"
>       [AlterTable ea (name "a") $ AlterTableActions ea [DropColumn ea (Nmc "b")]]
>      ,s "alter table a add constraint unique(b);"
>       [AlterTable ea (name "a") $ AlterTableActions ea [AddConstraint ea (UniqueConstraint ea "" [Nmc "b"])]]
>      ]
>     ,Group "constraints" [
>       Group "nulls" [
>       s "create table t1 (\n\
>         \ a text null\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "a") (st "text")
>                            Nothing [NullConstraint ea ""] []]]
>      ,s "create table t1 (\n\
>         \ a text not null\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "a") (st "text")
>                                         Nothing [NotNullConstraint ea ""] []]]
>      ]
>
>      ,Group "unique" [
>       s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ unique (x,y)\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [UniqueConstraint ea "" [Nmc "x",Nmc "y"]]]

test arbitrary ordering

>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ unique (x),\n\
>         \ y int\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [UniqueConstraint ea "" [Nmc "x"]]]


test partitioning parser

>      ,s "create table t1(\n\
>         \ x int,\n\
>         \ ts datetime\n\
>         \ )\n\
>         \ partition by range(ts)\n\
>         \  (\n\
>         \   every 5 months\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "ts" "datetime"]
>          `setTablePartition`
>          (Just (TablePartitionDef ea (Nmc "ts") 5 Month))]


unique row

>      ,s "create table t1 (\n\
>         \ x int unique\n\
>         \);"
>         [createTable "t1"
>          [AttributeDef ea (Nmc "x") (st "int") Nothing
>           [RowUniqueConstraint ea ""] []]]
>
>      ,s "create table t1 (\n\
>         \ x int unique not null\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowUniqueConstraint ea ""
>                            ,NotNullConstraint ea ""] []]]

quick sanity check

>      ,s "create table t1 (\n\
>         \ x int not null unique\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [NotNullConstraint ea ""
>                            ,RowUniqueConstraint ea ""] []]]
>      ]
>
>      ,Group "primary key" [
>       s "create table t1 (\n\
>         \ x int primary key\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowPrimaryKeyConstraint ea ""] []]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ primary key (x,y)\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [PrimaryKeyConstraint ea "" [Nmc "x", Nmc "y"]]]
>      ]
>
>      ,Group "check" [
>       s "create table t (\n\
>         \f text check (f in('a', 'b'))\n\
>         \);"
>         [createTable "t"
>          [AttributeDef ea (Nmc "f") (st "text") Nothing
>           [RowCheckConstraint ea "" (InPredicate ea
>                                      (ei "f") True
>                                      (InList ea [stringQ "a", stringQ "b"]))] []]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ check (x>y)\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [CheckConstraint ea "" (binop ">" (ei "x") (ei "y"))]]
>      ]
>
>      ,Group "misc" [
>       s "create table t (\n\
>         \f text not null unique check (f in('a', 'b'))\n\
>         \);"
>         [createTable "t"
>          [AttributeDef ea (Nmc "f") (st "text") Nothing
>           [NotNullConstraint ea ""
>            ,RowUniqueConstraint ea ""
>            ,RowCheckConstraint ea "" (InPredicate ea
>                                    (ei "f") True
>                                    (InList ea [stringQ "a"
>                                               ,stringQ "b"]))] []]]
>      ]

>      ,Group "references" [
>       s "create table t1 (\n\
>         \ x int references t2\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Restrict Restrict] []]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2(y)\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") (Just $ Nmc "y")
>                             Restrict Restrict] []]]
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") []
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") [Nmc "z", Nmc "w"]
>           Restrict Restrict]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Cascade Restrict] []]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on update cascade\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Restrict Cascade] []]]
>
>      ,s "create table t1 (\n\
>         \ x int references t2 on delete cascade on update cascade\n\
>         \);"
>         [createTable "t1" [AttributeDef ea (Nmc "x") (st "int") Nothing
>                            [RowReferenceConstraint ea "" (name "t2") Nothing
>                             Cascade Cascade] []]]
>
>      ,s "create table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2 on update cascade on delete cascade\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") []
>           Cascade Cascade]]
>      ,s "create or replace table test (\n\
>         \  fielda text,\n\
>         \  fieldb int\n\
>         \);"
>       [createTable "test" [att "fielda" "text"
>                           ,att "fieldb" "int"]
>       `setTableReplace` Replace]
>
>      ,s "create or replace table tbl as select 1;"
>       [CreateTableAs ea (name "tbl") Replace
>        (makeSelect
>         {selSelectList = sl [si $ num "1"]})]
>
>      ,s "create or replace table t1 (\n\
>         \ x int,\n\
>         \ y int,\n\
>         \ foreign key (x,y) references t2(z,w)\n\
>         \);"
>         [createTable "t1" [att "x" "int"
>                           ,att "y" "int"]
>          `setTableCons`
>          [ReferenceConstraint ea "" [Nmc "x", Nmc "y"] (name "t2") [Nmc "z", Nmc "w"]
>           Restrict Restrict]
>          `setTableReplace` Replace]
>      ]
>      ]

-- >      ,Group "options"
-- >      [Stmt "create table t (\n\
-- >            \    a int not null with options(test)\n\
-- >            \);"
-- >       [CreateTable ea
-- >        (name "test")
-- >        [att "a" "int"
-- >        ]
-- >        []
-- >        Nothing
-- >        NoReplace]
-- >      ]

>      ]

>  where
>    s = Stmt

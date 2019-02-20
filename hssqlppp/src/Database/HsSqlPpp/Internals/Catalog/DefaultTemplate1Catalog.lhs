


This file is auto generated, to regenerate run
make regenDefaultTemplate1Catalog. You will need postgresql
installed to do this.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
>      (defaultTemplate1Catalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.OdbcCatalog
> --import Database.HsSqlPpp.Internals.TypesInternal
> -- | The catalog from a default template1 database in roughly the
> -- latest postgres. 'select version()' from the dbms this catalog
> -- was generated from: 'PostgreSQL 9.4.2 on x86_64-unknown-linux-gnu, compiled by gcc-4.9.real (Debian 4.9.2-17) 4.9.2, 64-bit'.
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      flip updateCatalog defaultCatalog (


    
>        [CatCreateScalarType "abstime", CatCreateScalarType "aclitem",
>         CatCreateScalarType "bit", CatCreateScalarType "bool",
>         CatCreateScalarType "box", CatCreateScalarType "bpchar",
>         CatCreateScalarType "bytea", CatCreateScalarType "char",
>         CatCreateScalarType "cid", CatCreateScalarType "cidr",
>         CatCreateScalarType "circle", CatCreateScalarType "date",
>         CatCreateScalarType "float4", CatCreateScalarType "float8",
>         CatCreateScalarType "gtsvector", CatCreateScalarType "inet",
>         CatCreateScalarType "int2", CatCreateScalarType "int2vector",
>         CatCreateScalarType "int4", CatCreateScalarType "int8",
>         CatCreateScalarType "interval", CatCreateScalarType "line",
>         CatCreateScalarType "lseg", CatCreateScalarType "macaddr",
>         CatCreateScalarType "money", CatCreateScalarType "name",
>         CatCreateScalarType "numeric", CatCreateScalarType "oid",
>         CatCreateScalarType "oidvector", CatCreateScalarType "path",
>         CatCreateScalarType "pg_node_tree", CatCreateScalarType "point",
>         CatCreateScalarType "polygon", CatCreateScalarType "refcursor",
>         CatCreateScalarType "regclass", CatCreateScalarType "regconfig",
>         CatCreateScalarType "regdictionary", CatCreateScalarType "regoper",
>         CatCreateScalarType "regoperator", CatCreateScalarType "regproc",
>         CatCreateScalarType "regprocedure", CatCreateScalarType "regtype",
>         CatCreateScalarType "reltime", CatCreateScalarType "smgr",
>         CatCreateScalarType "text", CatCreateScalarType "tid",
>         CatCreateScalarType "time", CatCreateScalarType "timestamp",
>         CatCreateScalarType "timestamptz", CatCreateScalarType "timetz",
>         CatCreateScalarType "tinterval", CatCreateScalarType "tsquery",
>         CatCreateScalarType "tsvector",
>         CatCreateScalarType "txid_snapshot", CatCreateScalarType "unknown",
>         CatCreateScalarType "uuid", CatCreateScalarType "varbit",
>         CatCreateScalarType "varchar", CatCreateScalarType "xid",
>         CatCreateScalarType "xml",
>         CatCreateDomainType "information_schema.yes_or_no" "varchar",
>         CatCreateCast "information_schema.yes_or_no" "varchar"
>           ImplicitCastContext,
>         CatCreateDomainType "information_schema.time_stamp" "timestamptz",
>         CatCreateCast "information_schema.time_stamp" "timestamptz"
>           ImplicitCastContext,
>         CatCreateDomainType "information_schema.sql_identifier" "varchar",
>         CatCreateCast "information_schema.sql_identifier" "varchar"
>           ImplicitCastContext,
>         CatCreateDomainType "information_schema.character_data" "varchar",
>         CatCreateCast "information_schema.character_data" "varchar"
>           ImplicitCastContext,
>         CatCreateDomainType "information_schema.cardinal_number" "int4",
>         CatCreateCast "information_schema.cardinal_number" "int4"
>           ImplicitCastContext,
>         CatCreateArrayType "_xml" "xml",
>         CatCreateTypeCategoryEntry "_xml" ("A", False),
>         CatCreateArrayType "_xid" "xid",
>         CatCreateTypeCategoryEntry "_xid" ("A", False),
>         CatCreateArrayType "_varchar" "varchar",
>         CatCreateTypeCategoryEntry "_varchar" ("A", False),
>         CatCreateArrayType "_varbit" "varbit",
>         CatCreateTypeCategoryEntry "_varbit" ("A", False),
>         CatCreateArrayType "_uuid" "uuid",
>         CatCreateTypeCategoryEntry "_uuid" ("A", False),
>         CatCreateArrayType "_txid_snapshot" "txid_snapshot",
>         CatCreateTypeCategoryEntry "_txid_snapshot" ("A", False),
>         CatCreateArrayType "_tsvector" "tsvector",
>         CatCreateTypeCategoryEntry "_tsvector" ("A", False),
>         CatCreateArrayType "_tsquery" "tsquery",
>         CatCreateTypeCategoryEntry "_tsquery" ("A", False),
>         CatCreateArrayType "_tinterval" "tinterval",
>         CatCreateTypeCategoryEntry "_tinterval" ("A", False),
>         CatCreateArrayType "_timetz" "timetz",
>         CatCreateTypeCategoryEntry "_timetz" ("A", False),
>         CatCreateArrayType "_timestamptz" "timestamptz",
>         CatCreateTypeCategoryEntry "_timestamptz" ("A", False),
>         CatCreateArrayType "_timestamp" "timestamp",
>         CatCreateTypeCategoryEntry "_timestamp" ("A", False),
>         CatCreateArrayType "_time" "time",
>         CatCreateTypeCategoryEntry "_time" ("A", False),
>         CatCreateArrayType "_tid" "tid",
>         CatCreateTypeCategoryEntry "_tid" ("A", False),
>         CatCreateArrayType "_text" "text",
>         CatCreateTypeCategoryEntry "_text" ("A", False),
>         CatCreateArrayType "_reltime" "reltime",
>         CatCreateTypeCategoryEntry "_reltime" ("A", False),
>         CatCreateArrayType "_regtype" "regtype",
>         CatCreateTypeCategoryEntry "_regtype" ("A", False),
>         CatCreateArrayType "_regprocedure" "regprocedure",
>         CatCreateTypeCategoryEntry "_regprocedure" ("A", False),
>         CatCreateArrayType "_regproc" "regproc",
>         CatCreateTypeCategoryEntry "_regproc" ("A", False),
>         CatCreateArrayType "_regoperator" "regoperator",
>         CatCreateTypeCategoryEntry "_regoperator" ("A", False),
>         CatCreateArrayType "_regoper" "regoper",
>         CatCreateTypeCategoryEntry "_regoper" ("A", False),
>         CatCreateArrayType "_regdictionary" "regdictionary",
>         CatCreateTypeCategoryEntry "_regdictionary" ("A", False),
>         CatCreateArrayType "_regconfig" "regconfig",
>         CatCreateTypeCategoryEntry "_regconfig" ("A", False),
>         CatCreateArrayType "_regclass" "regclass",
>         CatCreateTypeCategoryEntry "_regclass" ("A", False),
>         CatCreateArrayType "_refcursor" "refcursor",
>         CatCreateTypeCategoryEntry "_refcursor" ("A", False),
>         CatCreateArrayType "_record" "record",
>         CatCreateTypeCategoryEntry "_record" ("A", False),
>         CatCreateArrayType "_polygon" "polygon",
>         CatCreateTypeCategoryEntry "_polygon" ("A", False),
>         CatCreateArrayType "_point" "point",
>         CatCreateTypeCategoryEntry "_point" ("A", False),
>         CatCreateArrayType "_path" "path",
>         CatCreateTypeCategoryEntry "_path" ("A", False),
>         CatCreateArrayType "_oidvector" "oidvector",
>         CatCreateTypeCategoryEntry "_oidvector" ("A", False),
>         CatCreateArrayType "_oid" "oid",
>         CatCreateTypeCategoryEntry "_oid" ("A", False),
>         CatCreateArrayType "_numeric" "numeric",
>         CatCreateTypeCategoryEntry "_numeric" ("A", False),
>         CatCreateArrayType "_name" "name",
>         CatCreateTypeCategoryEntry "_name" ("A", False),
>         CatCreateArrayType "_money" "money",
>         CatCreateTypeCategoryEntry "_money" ("A", False),
>         CatCreateArrayType "_macaddr" "macaddr",
>         CatCreateTypeCategoryEntry "_macaddr" ("A", False),
>         CatCreateArrayType "_lseg" "lseg",
>         CatCreateTypeCategoryEntry "_lseg" ("A", False),
>         CatCreateArrayType "_line" "line",
>         CatCreateTypeCategoryEntry "_line" ("A", False),
>         CatCreateArrayType "_interval" "interval",
>         CatCreateTypeCategoryEntry "_interval" ("A", False),
>         CatCreateArrayType "_int8" "int8",
>         CatCreateTypeCategoryEntry "_int8" ("A", False),
>         CatCreateArrayType "_int4" "int4",
>         CatCreateTypeCategoryEntry "_int4" ("A", False),
>         CatCreateArrayType "_int2vector" "int2vector",
>         CatCreateTypeCategoryEntry "_int2vector" ("A", False),
>         CatCreateArrayType "_int2" "int2",
>         CatCreateTypeCategoryEntry "_int2" ("A", False),
>         CatCreateArrayType "_inet" "inet",
>         CatCreateTypeCategoryEntry "_inet" ("A", False),
>         CatCreateArrayType "_gtsvector" "gtsvector",
>         CatCreateTypeCategoryEntry "_gtsvector" ("A", False),
>         CatCreateArrayType "_float8" "float8",
>         CatCreateTypeCategoryEntry "_float8" ("A", False),
>         CatCreateArrayType "_float4" "float4",
>         CatCreateTypeCategoryEntry "_float4" ("A", False),
>         CatCreateArrayType "_date" "date",
>         CatCreateTypeCategoryEntry "_date" ("A", False),
>         CatCreateArrayType "_cstring" "cstring",
>         CatCreateTypeCategoryEntry "_cstring" ("A", False),
>         CatCreateArrayType "_circle" "circle",
>         CatCreateTypeCategoryEntry "_circle" ("A", False),
>         CatCreateArrayType "_cidr" "cidr",
>         CatCreateTypeCategoryEntry "_cidr" ("A", False),
>         CatCreateArrayType "_cid" "cid",
>         CatCreateTypeCategoryEntry "_cid" ("A", False),
>         CatCreateArrayType "_char" "char",
>         CatCreateTypeCategoryEntry "_char" ("A", False),
>         CatCreateArrayType "_bytea" "bytea",
>         CatCreateTypeCategoryEntry "_bytea" ("A", False),
>         CatCreateArrayType "_bpchar" "bpchar",
>         CatCreateTypeCategoryEntry "_bpchar" ("A", False),
>         CatCreateArrayType "_box" "box",
>         CatCreateTypeCategoryEntry "_box" ("A", False),
>         CatCreateArrayType "_bool" "bool",
>         CatCreateTypeCategoryEntry "_bool" ("A", False),
>         CatCreateArrayType "_bit" "bit",
>         CatCreateTypeCategoryEntry "_bit" ("A", False),
>         CatCreateArrayType "_aclitem" "aclitem",
>         CatCreateTypeCategoryEntry "_aclitem" ("A", False),
>         CatCreateArrayType "_abstime" "abstime",
>         CatCreateTypeCategoryEntry "_abstime" ("A", False),
>         CatCreatePrefixOp "!!" "int8" "numeric" True,
>         CatCreatePrefixOp "!!" "tsquery" "tsquery" True,
>         CatCreatePrefixOp "#" "path" "int4" True,
>         CatCreatePrefixOp "#" "polygon" "int4" True,
>         CatCreatePrefixOp "+" "int8" "int8" True,
>         CatCreatePrefixOp "+" "int2" "int2" True,
>         CatCreatePrefixOp "+" "int4" "int4" True,
>         CatCreatePrefixOp "+" "float4" "float4" True,
>         CatCreatePrefixOp "+" "float8" "float8" True,
>         CatCreatePrefixOp "+" "numeric" "numeric" True,
>         CatCreatePrefixOp "-" "int8" "int8" True,
>         CatCreatePrefixOp "-" "int2" "int2" True,
>         CatCreatePrefixOp "-" "int4" "int4" True,
>         CatCreatePrefixOp "-" "float4" "float4" True,
>         CatCreatePrefixOp "-" "float8" "float8" True,
>         CatCreatePrefixOp "-" "interval" "interval" True,
>         CatCreatePrefixOp "-" "numeric" "numeric" True,
>         CatCreatePrefixOp "?-" "lseg" "bool" True,
>         CatCreatePrefixOp "?-" "line" "bool" True,
>         CatCreatePrefixOp "?|" "lseg" "bool" True,
>         CatCreatePrefixOp "?|" "line" "bool" True,
>         CatCreatePrefixOp "@" "int8" "int8" True,
>         CatCreatePrefixOp "@" "int2" "int2" True,
>         CatCreatePrefixOp "@" "int4" "int4" True,
>         CatCreatePrefixOp "@" "float4" "float4" True,
>         CatCreatePrefixOp "@" "float8" "float8" True,
>         CatCreatePrefixOp "@" "numeric" "numeric" True,
>         CatCreatePrefixOp "@-@" "lseg" "float8" True,
>         CatCreatePrefixOp "@-@" "path" "float8" True,
>         CatCreatePrefixOp "@@" "lseg" "point" True,
>         CatCreatePrefixOp "@@" "path" "point" True,
>         CatCreatePrefixOp "@@" "box" "point" True,
>         CatCreatePrefixOp "@@" "polygon" "point" True,
>         CatCreatePrefixOp "@@" "circle" "point" True,
>         CatCreatePrefixOp "|" "tinterval" "abstime" True,
>         CatCreatePrefixOp "|/" "float8" "float8" True,
>         CatCreatePrefixOp "||/" "float8" "float8" True,
>         CatCreatePrefixOp "~" "int8" "int8" True,
>         CatCreatePrefixOp "~" "int2" "int2" True,
>         CatCreatePrefixOp "~" "int4" "int4" True,
>         CatCreatePrefixOp "~" "inet" "inet" True,
>         CatCreatePrefixOp "~" "bit" "bit" True,
>         CatCreatePostfixOp "!" "int8" "numeric" True,
>         CatCreateBinaryOp "!~" "name" "text" "bool" True,
>         CatCreateBinaryOp "!~" "text" "text" "bool" True,
>         CatCreateBinaryOp "!~" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "!~*" "name" "text" "bool" True,
>         CatCreateBinaryOp "!~*" "text" "text" "bool" True,
>         CatCreateBinaryOp "!~*" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "!~~" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp "!~~" "name" "text" "bool" True,
>         CatCreateBinaryOp "!~~" "text" "text" "bool" True,
>         CatCreateBinaryOp "!~~" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "!~~*" "name" "text" "bool" True,
>         CatCreateBinaryOp "!~~*" "text" "text" "bool" True,
>         CatCreateBinaryOp "!~~*" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "#" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "#" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "#" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "#" "lseg" "lseg" "point" True,
>         CatCreateBinaryOp "#" "box" "box" "box" True,
>         CatCreateBinaryOp "#" "line" "line" "point" True,
>         CatCreateBinaryOp "#" "bit" "bit" "bit" True,
>         CatCreateBinaryOp "##" "point" "lseg" "point" True,
>         CatCreateBinaryOp "##" "point" "box" "point" True,
>         CatCreateBinaryOp "##" "point" "line" "point" True,
>         CatCreateBinaryOp "##" "lseg" "lseg" "point" True,
>         CatCreateBinaryOp "##" "lseg" "box" "point" True,
>         CatCreateBinaryOp "##" "lseg" "line" "point" True,
>         CatCreateBinaryOp "##" "line" "lseg" "point" True,
>         CatCreateBinaryOp "##" "line" "box" "point" True,
>         CatCreateBinaryOp "#<" "tinterval" "reltime" "bool" True,
>         CatCreateBinaryOp "#<=" "tinterval" "reltime" "bool" True,
>         CatCreateBinaryOp "#<>" "tinterval" "reltime" "bool" True,
>         CatCreateBinaryOp "#=" "tinterval" "reltime" "bool" True,
>         CatCreateBinaryOp "#>" "tinterval" "reltime" "bool" True,
>         CatCreateBinaryOp "#>=" "tinterval" "reltime" "bool" True,
>         CatCreateBinaryOp "%" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "%" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "%" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "%" "numeric" "numeric" "numeric" True,
>         CatCreateBinaryOp "&" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "&" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "&" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "&" "inet" "inet" "inet" True,
>         CatCreateBinaryOp "&" "bit" "bit" "bit" True,
>         CatCreateBinaryOp "&&" "box" "box" "bool" True,
>         CatCreateBinaryOp "&&" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "&&" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "&&" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "&&" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "&&" "tsquery" "tsquery" "tsquery" True,
>         CatCreateBinaryOp "&<" "box" "box" "bool" True,
>         CatCreateBinaryOp "&<" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "&<" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "&<|" "box" "box" "bool" True,
>         CatCreateBinaryOp "&<|" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "&<|" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "&>" "box" "box" "bool" True,
>         CatCreateBinaryOp "&>" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "&>" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "*" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "*" "int8" "int2" "int8" True,
>         CatCreateBinaryOp "*" "int8" "int4" "int8" True,
>         CatCreateBinaryOp "*" "int2" "int8" "int8" True,
>         CatCreateBinaryOp "*" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "*" "int2" "int4" "int4" True,
>         CatCreateBinaryOp "*" "int2" "money" "money" True,
>         CatCreateBinaryOp "*" "int4" "int8" "int8" True,
>         CatCreateBinaryOp "*" "int4" "int2" "int4" True,
>         CatCreateBinaryOp "*" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "*" "int4" "money" "money" True,
>         CatCreateBinaryOp "*" "point" "point" "point" True,
>         CatCreateBinaryOp "*" "path" "point" "path" True,
>         CatCreateBinaryOp "*" "box" "point" "box" True,
>         CatCreateBinaryOp "*" "float4" "float4" "float4" True,
>         CatCreateBinaryOp "*" "float4" "float8" "float8" True,
>         CatCreateBinaryOp "*" "float4" "money" "money" True,
>         CatCreateBinaryOp "*" "float8" "float4" "float8" True,
>         CatCreateBinaryOp "*" "float8" "float8" "float8" True,
>         CatCreateBinaryOp "*" "float8" "money" "money" True,
>         CatCreateBinaryOp "*" "float8" "interval" "interval" True,
>         CatCreateBinaryOp "*" "circle" "point" "circle" True,
>         CatCreateBinaryOp "*" "money" "int2" "money" True,
>         CatCreateBinaryOp "*" "money" "int4" "money" True,
>         CatCreateBinaryOp "*" "money" "float4" "money" True,
>         CatCreateBinaryOp "*" "money" "float8" "money" True,
>         CatCreateBinaryOp "*" "interval" "float8" "interval" True,
>         CatCreateBinaryOp "*" "numeric" "numeric" "numeric" True,
>         CatCreateBinaryOp "+" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "+" "int8" "int2" "int8" True,
>         CatCreateBinaryOp "+" "int8" "int4" "int8" True,
>         CatCreateBinaryOp "+" "int8" "inet" "inet" True,
>         CatCreateBinaryOp "+" "int2" "int8" "int8" True,
>         CatCreateBinaryOp "+" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "+" "int2" "int4" "int4" True,
>         CatCreateBinaryOp "+" "int4" "int8" "int8" True,
>         CatCreateBinaryOp "+" "int4" "int2" "int4" True,
>         CatCreateBinaryOp "+" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "+" "int4" "date" "date" True,
>         CatCreateBinaryOp "+" "point" "point" "point" True,
>         CatCreateBinaryOp "+" "path" "point" "path" True,
>         CatCreateBinaryOp "+" "path" "path" "path" True,
>         CatCreateBinaryOp "+" "box" "point" "box" True,
>         CatCreateBinaryOp "+" "float4" "float4" "float4" True,
>         CatCreateBinaryOp "+" "float4" "float8" "float8" True,
>         CatCreateBinaryOp "+" "float8" "float4" "float8" True,
>         CatCreateBinaryOp "+" "float8" "float8" "float8" True,
>         CatCreateBinaryOp "+" "abstime" "reltime" "abstime" True,
>         CatCreateBinaryOp "+" "circle" "point" "circle" True,
>         CatCreateBinaryOp "+" "money" "money" "money" True,
>         CatCreateBinaryOp "+" "inet" "int8" "inet" True,
>         CatCreateBinaryOp "+" "_aclitem" "aclitem" "_aclitem" True,
>         CatCreateBinaryOp "+" "date" "int4" "date" True,
>         CatCreateBinaryOp "+" "date" "time" "timestamp" True,
>         CatCreateBinaryOp "+" "date" "interval" "timestamp" True,
>         CatCreateBinaryOp "+" "date" "timetz" "timestamptz" True,
>         CatCreateBinaryOp "+" "time" "date" "timestamp" True,
>         CatCreateBinaryOp "+" "time" "interval" "time" True,
>         CatCreateBinaryOp "+" "timestamp" "interval" "timestamp" True,
>         CatCreateBinaryOp "+" "timestamptz" "interval" "timestamptz" True,
>         CatCreateBinaryOp "+" "interval" "date" "timestamp" True,
>         CatCreateBinaryOp "+" "interval" "time" "time" True,
>         CatCreateBinaryOp "+" "interval" "timestamp" "timestamp" True,
>         CatCreateBinaryOp "+" "interval" "timestamptz" "timestamptz" True,
>         CatCreateBinaryOp "+" "interval" "interval" "interval" True,
>         CatCreateBinaryOp "+" "interval" "timetz" "timetz" True,
>         CatCreateBinaryOp "+" "timetz" "date" "timestamptz" True,
>         CatCreateBinaryOp "+" "timetz" "interval" "timetz" True,
>         CatCreateBinaryOp "+" "numeric" "numeric" "numeric" True,
>         CatCreateBinaryOp "-" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "-" "int8" "int2" "int8" True,
>         CatCreateBinaryOp "-" "int8" "int4" "int8" True,
>         CatCreateBinaryOp "-" "int2" "int8" "int8" True,
>         CatCreateBinaryOp "-" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "-" "int2" "int4" "int4" True,
>         CatCreateBinaryOp "-" "int4" "int8" "int8" True,
>         CatCreateBinaryOp "-" "int4" "int2" "int4" True,
>         CatCreateBinaryOp "-" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "-" "point" "point" "point" True,
>         CatCreateBinaryOp "-" "path" "point" "path" True,
>         CatCreateBinaryOp "-" "box" "point" "box" True,
>         CatCreateBinaryOp "-" "float4" "float4" "float4" True,
>         CatCreateBinaryOp "-" "float4" "float8" "float8" True,
>         CatCreateBinaryOp "-" "float8" "float4" "float8" True,
>         CatCreateBinaryOp "-" "float8" "float8" "float8" True,
>         CatCreateBinaryOp "-" "abstime" "reltime" "abstime" True,
>         CatCreateBinaryOp "-" "circle" "point" "circle" True,
>         CatCreateBinaryOp "-" "money" "money" "money" True,
>         CatCreateBinaryOp "-" "inet" "int8" "inet" True,
>         CatCreateBinaryOp "-" "inet" "inet" "int8" True,
>         CatCreateBinaryOp "-" "_aclitem" "aclitem" "_aclitem" True,
>         CatCreateBinaryOp "-" "date" "int4" "date" True,
>         CatCreateBinaryOp "-" "date" "date" "int4" True,
>         CatCreateBinaryOp "-" "date" "interval" "timestamp" True,
>         CatCreateBinaryOp "-" "time" "time" "interval" True,
>         CatCreateBinaryOp "-" "time" "interval" "time" True,
>         CatCreateBinaryOp "-" "timestamp" "timestamp" "interval" True,
>         CatCreateBinaryOp "-" "timestamp" "interval" "timestamp" True,
>         CatCreateBinaryOp "-" "timestamptz" "timestamptz" "interval" True,
>         CatCreateBinaryOp "-" "timestamptz" "interval" "timestamptz" True,
>         CatCreateBinaryOp "-" "interval" "interval" "interval" True,
>         CatCreateBinaryOp "-" "timetz" "interval" "timetz" True,
>         CatCreateBinaryOp "-" "numeric" "numeric" "numeric" True,
>         CatCreateBinaryOp "/" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "/" "int8" "int2" "int8" True,
>         CatCreateBinaryOp "/" "int8" "int4" "int8" True,
>         CatCreateBinaryOp "/" "int2" "int8" "int8" True,
>         CatCreateBinaryOp "/" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "/" "int2" "int4" "int4" True,
>         CatCreateBinaryOp "/" "int4" "int8" "int8" True,
>         CatCreateBinaryOp "/" "int4" "int2" "int4" True,
>         CatCreateBinaryOp "/" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "/" "point" "point" "point" True,
>         CatCreateBinaryOp "/" "path" "point" "path" True,
>         CatCreateBinaryOp "/" "box" "point" "box" True,
>         CatCreateBinaryOp "/" "float4" "float4" "float4" True,
>         CatCreateBinaryOp "/" "float4" "float8" "float8" True,
>         CatCreateBinaryOp "/" "float8" "float4" "float8" True,
>         CatCreateBinaryOp "/" "float8" "float8" "float8" True,
>         CatCreateBinaryOp "/" "circle" "point" "circle" True,
>         CatCreateBinaryOp "/" "money" "int2" "money" True,
>         CatCreateBinaryOp "/" "money" "int4" "money" True,
>         CatCreateBinaryOp "/" "money" "float4" "money" True,
>         CatCreateBinaryOp "/" "money" "float8" "money" True,
>         CatCreateBinaryOp "/" "money" "money" "float8" True,
>         CatCreateBinaryOp "/" "interval" "float8" "interval" True,
>         CatCreateBinaryOp "/" "numeric" "numeric" "numeric" True,
>         CatCreateBinaryOp "<" "bool" "bool" "bool" True,
>         CatCreateBinaryOp "<" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp "<" "char" "char" "bool" True,
>         CatCreateBinaryOp "<" "name" "name" "bool" True,
>         CatCreateBinaryOp "<" "int8" "int8" "bool" True,
>         CatCreateBinaryOp "<" "int8" "int2" "bool" True,
>         CatCreateBinaryOp "<" "int8" "int4" "bool" True,
>         CatCreateBinaryOp "<" "int2" "int8" "bool" True,
>         CatCreateBinaryOp "<" "int2" "int2" "bool" True,
>         CatCreateBinaryOp "<" "int2" "int4" "bool" True,
>         CatCreateBinaryOp "<" "int4" "int8" "bool" True,
>         CatCreateBinaryOp "<" "int4" "int2" "bool" True,
>         CatCreateBinaryOp "<" "int4" "int4" "bool" True,
>         CatCreateBinaryOp "<" "text" "text" "bool" True,
>         CatCreateBinaryOp "<" "oid" "oid" "bool" True,
>         CatCreateBinaryOp "<" "tid" "tid" "bool" True,
>         CatCreateBinaryOp "<" "oidvector" "oidvector" "bool" True,
>         CatCreateBinaryOp "<" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "<" "path" "path" "bool" True,
>         CatCreateBinaryOp "<" "box" "box" "bool" True,
>         CatCreateBinaryOp "<" "float4" "float4" "bool" True,
>         CatCreateBinaryOp "<" "float4" "float8" "bool" True,
>         CatCreateBinaryOp "<" "float8" "float4" "bool" True,
>         CatCreateBinaryOp "<" "float8" "float8" "bool" True,
>         CatCreateBinaryOp "<" "abstime" "abstime" "bool" True,
>         CatCreateBinaryOp "<" "reltime" "reltime" "bool" True,
>         CatCreateBinaryOp "<" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "<" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "<" "money" "money" "bool" True,
>         CatCreateBinaryOp "<" "macaddr" "macaddr" "bool" True,
>         CatCreateBinaryOp "<" "inet" "inet" "bool" True,
>         CatCreateBinaryOp "<" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "<" "date" "date" "bool" True,
>         --CatCreateBinaryOp "<" "date" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<" "date" "timestamptz" "bool" True,
>         CatCreateBinaryOp "<" "time" "time" "bool" True,
>         --CatCreateBinaryOp "<" "timestamp" "date" "bool" True,
>         CatCreateBinaryOp "<" "timestamp" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<" "timestamp" "timestamptz" "bool" True,
>         --CatCreateBinaryOp "<" "timestamptz" "date" "bool" True,
>         --CatCreateBinaryOp "<" "timestamptz" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<" "timestamptz" "timestamptz" "bool" True,
>         CatCreateBinaryOp "<" "interval" "interval" "bool" True,
>         CatCreateBinaryOp "<" "timetz" "timetz" "bool" True,
>         CatCreateBinaryOp "<" "bit" "bit" "bool" True,
>         CatCreateBinaryOp "<" "varbit" "varbit" "bool" True,
>         CatCreateBinaryOp "<" "numeric" "numeric" "bool" True,
>         CatCreateBinaryOp "<" "record" "record" "bool" True,
>         CatCreateBinaryOp "<" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "<" "uuid" "uuid" "bool" True,
>         CatCreateBinaryOp "<" "anyenum" "anyenum" "bool" True,
>         CatCreateBinaryOp "<" "tsvector" "tsvector" "bool" True,
>         CatCreateBinaryOp "<" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp "<#>" "abstime" "abstime" "tinterval" True,
>         CatCreateBinaryOp "<->" "point" "point" "float8" True,
>         CatCreateBinaryOp "<->" "point" "lseg" "float8" True,
>         CatCreateBinaryOp "<->" "point" "path" "float8" True,
>         CatCreateBinaryOp "<->" "point" "box" "float8" True,
>         CatCreateBinaryOp "<->" "point" "line" "float8" True,
>         CatCreateBinaryOp "<->" "point" "circle" "float8" True,
>         CatCreateBinaryOp "<->" "lseg" "lseg" "float8" True,
>         CatCreateBinaryOp "<->" "lseg" "box" "float8" True,
>         CatCreateBinaryOp "<->" "lseg" "line" "float8" True,
>         CatCreateBinaryOp "<->" "path" "path" "float8" True,
>         CatCreateBinaryOp "<->" "box" "box" "float8" True,
>         CatCreateBinaryOp "<->" "polygon" "polygon" "float8" True,
>         CatCreateBinaryOp "<->" "line" "box" "float8" True,
>         CatCreateBinaryOp "<->" "line" "line" "float8" True,
>         CatCreateBinaryOp "<->" "circle" "polygon" "float8" True,
>         CatCreateBinaryOp "<->" "circle" "circle" "float8" True,
>         CatCreateBinaryOp "<<" "int8" "int4" "int8" True,
>         CatCreateBinaryOp "<<" "int2" "int4" "int2" True,
>         CatCreateBinaryOp "<<" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "<<" "point" "point" "bool" True,
>         CatCreateBinaryOp "<<" "box" "box" "bool" True,
>         CatCreateBinaryOp "<<" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "<<" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "<<" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "<<" "inet" "inet" "bool" True,
>         CatCreateBinaryOp "<<" "bit" "int4" "bit" True,
>         CatCreateBinaryOp "<<=" "inet" "inet" "bool" True,
>         CatCreateBinaryOp "<<|" "box" "box" "bool" True,
>         CatCreateBinaryOp "<<|" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "<<|" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "<=" "bool" "bool" "bool" True,
>         CatCreateBinaryOp "<=" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp "<=" "char" "char" "bool" True,
>         CatCreateBinaryOp "<=" "name" "name" "bool" True,
>         CatCreateBinaryOp "<=" "int8" "int8" "bool" True,
>         CatCreateBinaryOp "<=" "int8" "int2" "bool" True,
>         CatCreateBinaryOp "<=" "int8" "int4" "bool" True,
>         CatCreateBinaryOp "<=" "int2" "int8" "bool" True,
>         CatCreateBinaryOp "<=" "int2" "int2" "bool" True,
>         CatCreateBinaryOp "<=" "int2" "int4" "bool" True,
>         CatCreateBinaryOp "<=" "int4" "int8" "bool" True,
>         CatCreateBinaryOp "<=" "int4" "int2" "bool" True,
>         CatCreateBinaryOp "<=" "int4" "int4" "bool" True,
>         CatCreateBinaryOp "<=" "text" "text" "bool" True,
>         CatCreateBinaryOp "<=" "oid" "oid" "bool" True,
>         CatCreateBinaryOp "<=" "tid" "tid" "bool" True,
>         CatCreateBinaryOp "<=" "oidvector" "oidvector" "bool" True,
>         CatCreateBinaryOp "<=" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "<=" "path" "path" "bool" True,
>         CatCreateBinaryOp "<=" "box" "box" "bool" True,
>         CatCreateBinaryOp "<=" "float4" "float4" "bool" True,
>         CatCreateBinaryOp "<=" "float4" "float8" "bool" True,
>         CatCreateBinaryOp "<=" "float8" "float4" "bool" True,
>         CatCreateBinaryOp "<=" "float8" "float8" "bool" True,
>         CatCreateBinaryOp "<=" "abstime" "abstime" "bool" True,
>         CatCreateBinaryOp "<=" "reltime" "reltime" "bool" True,
>         CatCreateBinaryOp "<=" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "<=" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "<=" "money" "money" "bool" True,
>         CatCreateBinaryOp "<=" "macaddr" "macaddr" "bool" True,
>         CatCreateBinaryOp "<=" "inet" "inet" "bool" True,
>         CatCreateBinaryOp "<=" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "<=" "date" "date" "bool" True,
>         --CatCreateBinaryOp "<=" "date" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<=" "date" "timestamptz" "bool" True,
>         CatCreateBinaryOp "<=" "time" "time" "bool" True,
>         --CatCreateBinaryOp "<=" "timestamp" "date" "bool" True,
>         CatCreateBinaryOp "<=" "timestamp" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<=" "timestamp" "timestamptz" "bool" True,
>         --CatCreateBinaryOp "<=" "timestamptz" "date" "bool" True,
>         --CatCreateBinaryOp "<=" "timestamptz" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<=" "timestamptz" "timestamptz" "bool" True,
>         CatCreateBinaryOp "<=" "interval" "interval" "bool" True,
>         CatCreateBinaryOp "<=" "timetz" "timetz" "bool" True,
>         CatCreateBinaryOp "<=" "bit" "bit" "bool" True,
>         CatCreateBinaryOp "<=" "varbit" "varbit" "bool" True,
>         CatCreateBinaryOp "<=" "numeric" "numeric" "bool" True,
>         CatCreateBinaryOp "<=" "record" "record" "bool" True,
>         CatCreateBinaryOp "<=" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "<=" "uuid" "uuid" "bool" True,
>         CatCreateBinaryOp "<=" "anyenum" "anyenum" "bool" True,
>         CatCreateBinaryOp "<=" "tsvector" "tsvector" "bool" True,
>         CatCreateBinaryOp "<=" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp "<>" "bool" "bool" "bool" True,
>         CatCreateBinaryOp "<>" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp "<>" "char" "char" "bool" True,
>         CatCreateBinaryOp "<>" "name" "name" "bool" True,
>         CatCreateBinaryOp "<>" "int8" "int8" "bool" True,
>         CatCreateBinaryOp "<>" "int8" "int2" "bool" True,
>         CatCreateBinaryOp "<>" "int8" "int4" "bool" True,
>         CatCreateBinaryOp "<>" "int2" "int8" "bool" True,
>         CatCreateBinaryOp "<>" "int2" "int2" "bool" True,
>         CatCreateBinaryOp "<>" "int2" "int4" "bool" True,
>         CatCreateBinaryOp "<>" "int4" "int8" "bool" True,
>         CatCreateBinaryOp "<>" "int4" "int2" "bool" True,
>         CatCreateBinaryOp "<>" "int4" "int4" "bool" True,
>         CatCreateBinaryOp "<>" "text" "text" "bool" True,
>         CatCreateBinaryOp "<>" "oid" "oid" "bool" True,
>         CatCreateBinaryOp "<>" "tid" "tid" "bool" True,
>         CatCreateBinaryOp "<>" "oidvector" "oidvector" "bool" True,
>         CatCreateBinaryOp "<>" "point" "point" "bool" True,
>         CatCreateBinaryOp "<>" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "<>" "float4" "float4" "bool" True,
>         CatCreateBinaryOp "<>" "float4" "float8" "bool" True,
>         CatCreateBinaryOp "<>" "float8" "float4" "bool" True,
>         CatCreateBinaryOp "<>" "float8" "float8" "bool" True,
>         CatCreateBinaryOp "<>" "abstime" "abstime" "bool" True,
>         CatCreateBinaryOp "<>" "reltime" "reltime" "bool" True,
>         CatCreateBinaryOp "<>" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "<>" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "<>" "money" "money" "bool" True,
>         CatCreateBinaryOp "<>" "macaddr" "macaddr" "bool" True,
>         CatCreateBinaryOp "<>" "inet" "inet" "bool" True,
>         CatCreateBinaryOp "<>" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "<>" "date" "date" "bool" True,
>         --CatCreateBinaryOp "<>" "date" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<>" "date" "timestamptz" "bool" True,
>         CatCreateBinaryOp "<>" "time" "time" "bool" True,
>         --CatCreateBinaryOp "<>" "timestamp" "date" "bool" True,
>         CatCreateBinaryOp "<>" "timestamp" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<>" "timestamp" "timestamptz" "bool" True,
>         --CatCreateBinaryOp "<>" "timestamptz" "date" "bool" True,
>         --CatCreateBinaryOp "<>" "timestamptz" "timestamp" "bool" True,
>         --CatCreateBinaryOp "<>" "timestamptz" "timestamptz" "bool" True,
>         CatCreateBinaryOp "<>" "interval" "interval" "bool" True,
>         CatCreateBinaryOp "<>" "timetz" "timetz" "bool" True,
>         CatCreateBinaryOp "<>" "bit" "bit" "bool" True,
>         CatCreateBinaryOp "<>" "varbit" "varbit" "bool" True,
>         CatCreateBinaryOp "<>" "numeric" "numeric" "bool" True,
>         CatCreateBinaryOp "<>" "record" "record" "bool" True,
>         CatCreateBinaryOp "<>" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "<>" "uuid" "uuid" "bool" True,
>         CatCreateBinaryOp "<>" "anyenum" "anyenum" "bool" True,
>         CatCreateBinaryOp "<>" "tsvector" "tsvector" "bool" True,
>         CatCreateBinaryOp "<>" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp "<?>" "abstime" "tinterval" "bool" True,
>         CatCreateBinaryOp "<@" "point" "lseg" "bool" True,
>         CatCreateBinaryOp "<@" "point" "path" "bool" True,
>         CatCreateBinaryOp "<@" "point" "box" "bool" True,
>         CatCreateBinaryOp "<@" "point" "polygon" "bool" True,
>         CatCreateBinaryOp "<@" "point" "line" "bool" True,
>         CatCreateBinaryOp "<@" "point" "circle" "bool" True,
>         CatCreateBinaryOp "<@" "lseg" "box" "bool" True,
>         CatCreateBinaryOp "<@" "lseg" "line" "bool" True,
>         CatCreateBinaryOp "<@" "box" "box" "bool" True,
>         CatCreateBinaryOp "<@" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "<@" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "<@" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "<@" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp "<^" "point" "point" "bool" True,
>         CatCreateBinaryOp "<^" "box" "box" "bool" True,
>         CatCreateBinaryOp "=" "bool" "bool" "bool" True,
>         CatCreateBinaryOp "=" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp "=" "char" "char" "bool" True,
>         CatCreateBinaryOp "=" "name" "name" "bool" True,
>         CatCreateBinaryOp "=" "int8" "int8" "bool" True,
>         CatCreateBinaryOp "=" "int8" "int2" "bool" True,
>         CatCreateBinaryOp "=" "int8" "int4" "bool" True,
>         CatCreateBinaryOp "=" "int2" "int8" "bool" True,
>         CatCreateBinaryOp "=" "int2" "int2" "bool" True,
>         CatCreateBinaryOp "=" "int2" "int4" "bool" True,
>         CatCreateBinaryOp "=" "int2vector" "int2vector" "bool" True,
>         CatCreateBinaryOp "=" "int4" "int8" "bool" True,
>         CatCreateBinaryOp "=" "int4" "int2" "bool" True,
>         CatCreateBinaryOp "=" "int4" "int4" "bool" True,
>         CatCreateBinaryOp "=" "text" "text" "bool" True,
>         CatCreateBinaryOp "=" "oid" "oid" "bool" True,
>         CatCreateBinaryOp "=" "tid" "tid" "bool" True,
>         CatCreateBinaryOp "=" "xid" "int4" "bool" True,
>         CatCreateBinaryOp "=" "xid" "xid" "bool" True,
>         CatCreateBinaryOp "=" "cid" "cid" "bool" True,
>         CatCreateBinaryOp "=" "oidvector" "oidvector" "bool" True,
>         CatCreateBinaryOp "=" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "=" "path" "path" "bool" True,
>         CatCreateBinaryOp "=" "box" "box" "bool" True,
>         CatCreateBinaryOp "=" "line" "line" "bool" True,
>         CatCreateBinaryOp "=" "float4" "float4" "bool" True,
>         CatCreateBinaryOp "=" "float4" "float8" "bool" True,
>         CatCreateBinaryOp "=" "float8" "float4" "bool" True,
>         CatCreateBinaryOp "=" "float8" "float8" "bool" True,
>         CatCreateBinaryOp "=" "abstime" "abstime" "bool" True,
>         CatCreateBinaryOp "=" "reltime" "reltime" "bool" True,
>         CatCreateBinaryOp "=" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "=" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "=" "money" "money" "bool" True,
>         CatCreateBinaryOp "=" "macaddr" "macaddr" "bool" True,
>         CatCreateBinaryOp "=" "inet" "inet" "bool" True,
>         CatCreateBinaryOp "=" "aclitem" "aclitem" "bool" True,
>         CatCreateBinaryOp "=" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "=" "date" "date" "bool" True,
>         --CatCreateBinaryOp "=" "date" "timestamp" "bool" True,
>         --CatCreateBinaryOp "=" "date" "timestamptz" "bool" True,
>         CatCreateBinaryOp "=" "time" "time" "bool" True,
>         --CatCreateBinaryOp "=" "timestamp" "date" "bool" True,
>         CatCreateBinaryOp "=" "timestamp" "timestamp" "bool" True,
>         --CatCreateBinaryOp "=" "timestamp" "timestamptz" "bool" True,
>         --CatCreateBinaryOp "=" "timestamptz" "date" "bool" True,
>         --CatCreateBinaryOp "=" "timestamptz" "timestamp" "bool" True,
>         --CatCreateBinaryOp "=" "timestamptz" "timestamptz" "bool" True,
>         CatCreateBinaryOp "=" "interval" "interval" "bool" True,
>         CatCreateBinaryOp "=" "timetz" "timetz" "bool" True,
>         CatCreateBinaryOp "=" "bit" "bit" "bool" True,
>         CatCreateBinaryOp "=" "varbit" "varbit" "bool" True,
>         CatCreateBinaryOp "=" "numeric" "numeric" "bool" True,
>         CatCreateBinaryOp "=" "record" "record" "bool" True,
>         CatCreateBinaryOp "=" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "=" "uuid" "uuid" "bool" True,
>         CatCreateBinaryOp "=" "anyenum" "anyenum" "bool" True,
>         CatCreateBinaryOp "=" "tsvector" "tsvector" "bool" True,
>         CatCreateBinaryOp "=" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp ">" "bool" "bool" "bool" True,
>         CatCreateBinaryOp ">" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp ">" "char" "char" "bool" True,
>         CatCreateBinaryOp ">" "name" "name" "bool" True,
>         CatCreateBinaryOp ">" "int8" "int8" "bool" True,
>         CatCreateBinaryOp ">" "int8" "int2" "bool" True,
>         CatCreateBinaryOp ">" "int8" "int4" "bool" True,
>         CatCreateBinaryOp ">" "int2" "int8" "bool" True,
>         CatCreateBinaryOp ">" "int2" "int2" "bool" True,
>         CatCreateBinaryOp ">" "int2" "int4" "bool" True,
>         CatCreateBinaryOp ">" "int4" "int8" "bool" True,
>         CatCreateBinaryOp ">" "int4" "int2" "bool" True,
>         CatCreateBinaryOp ">" "int4" "int4" "bool" True,
>         CatCreateBinaryOp ">" "text" "text" "bool" True,
>         CatCreateBinaryOp ">" "oid" "oid" "bool" True,
>         CatCreateBinaryOp ">" "tid" "tid" "bool" True,
>         CatCreateBinaryOp ">" "oidvector" "oidvector" "bool" True,
>         CatCreateBinaryOp ">" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp ">" "path" "path" "bool" True,
>         CatCreateBinaryOp ">" "box" "box" "bool" True,
>         CatCreateBinaryOp ">" "float4" "float4" "bool" True,
>         CatCreateBinaryOp ">" "float4" "float8" "bool" True,
>         CatCreateBinaryOp ">" "float8" "float4" "bool" True,
>         CatCreateBinaryOp ">" "float8" "float8" "bool" True,
>         CatCreateBinaryOp ">" "abstime" "abstime" "bool" True,
>         CatCreateBinaryOp ">" "reltime" "reltime" "bool" True,
>         CatCreateBinaryOp ">" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp ">" "circle" "circle" "bool" True,
>         CatCreateBinaryOp ">" "money" "money" "bool" True,
>         CatCreateBinaryOp ">" "macaddr" "macaddr" "bool" True,
>         CatCreateBinaryOp ">" "inet" "inet" "bool" True,
>         CatCreateBinaryOp ">" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp ">" "date" "date" "bool" True,
>         --CatCreateBinaryOp ">" "date" "timestamp" "bool" True,
>         --CatCreateBinaryOp ">" "date" "timestamptz" "bool" True,
>         CatCreateBinaryOp ">" "time" "time" "bool" True,
>         --CatCreateBinaryOp ">" "timestamp" "date" "bool" True,
>         CatCreateBinaryOp ">" "timestamp" "timestamp" "bool" True,
>         --CatCreateBinaryOp ">" "timestamp" "timestamptz" "bool" True,
>         --CatCreateBinaryOp ">" "timestamptz" "date" "bool" True,
>         --CatCreateBinaryOp ">" "timestamptz" "timestamp" "bool" True,
>         --CatCreateBinaryOp ">" "timestamptz" "timestamptz" "bool" True,
>         CatCreateBinaryOp ">" "interval" "interval" "bool" True,
>         CatCreateBinaryOp ">" "timetz" "timetz" "bool" True,
>         CatCreateBinaryOp ">" "bit" "bit" "bool" True,
>         CatCreateBinaryOp ">" "varbit" "varbit" "bool" True,
>         CatCreateBinaryOp ">" "numeric" "numeric" "bool" True,
>         CatCreateBinaryOp ">" "record" "record" "bool" True,
>         CatCreateBinaryOp ">" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp ">" "uuid" "uuid" "bool" True,
>         CatCreateBinaryOp ">" "anyenum" "anyenum" "bool" True,
>         CatCreateBinaryOp ">" "tsvector" "tsvector" "bool" True,
>         CatCreateBinaryOp ">" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp ">=" "bool" "bool" "bool" True,
>         CatCreateBinaryOp ">=" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp ">=" "char" "char" "bool" True,
>         CatCreateBinaryOp ">=" "name" "name" "bool" True,
>         CatCreateBinaryOp ">=" "int8" "int8" "bool" True,
>         CatCreateBinaryOp ">=" "int8" "int2" "bool" True,
>         CatCreateBinaryOp ">=" "int8" "int4" "bool" True,
>         CatCreateBinaryOp ">=" "int2" "int8" "bool" True,
>         CatCreateBinaryOp ">=" "int2" "int2" "bool" True,
>         CatCreateBinaryOp ">=" "int2" "int4" "bool" True,
>         CatCreateBinaryOp ">=" "int4" "int8" "bool" True,
>         CatCreateBinaryOp ">=" "int4" "int2" "bool" True,
>         CatCreateBinaryOp ">=" "int4" "int4" "bool" True,
>         CatCreateBinaryOp ">=" "text" "text" "bool" True,
>         CatCreateBinaryOp ">=" "oid" "oid" "bool" True,
>         CatCreateBinaryOp ">=" "tid" "tid" "bool" True,
>         CatCreateBinaryOp ">=" "oidvector" "oidvector" "bool" True,
>         CatCreateBinaryOp ">=" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp ">=" "path" "path" "bool" True,
>         CatCreateBinaryOp ">=" "box" "box" "bool" True,
>         CatCreateBinaryOp ">=" "float4" "float4" "bool" True,
>         CatCreateBinaryOp ">=" "float4" "float8" "bool" True,
>         CatCreateBinaryOp ">=" "float8" "float4" "bool" True,
>         CatCreateBinaryOp ">=" "float8" "float8" "bool" True,
>         CatCreateBinaryOp ">=" "abstime" "abstime" "bool" True,
>         CatCreateBinaryOp ">=" "reltime" "reltime" "bool" True,
>         CatCreateBinaryOp ">=" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp ">=" "circle" "circle" "bool" True,
>         CatCreateBinaryOp ">=" "money" "money" "bool" True,
>         CatCreateBinaryOp ">=" "macaddr" "macaddr" "bool" True,
>         CatCreateBinaryOp ">=" "inet" "inet" "bool" True,
>         CatCreateBinaryOp ">=" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp ">=" "date" "date" "bool" True,
>         --CatCreateBinaryOp ">=" "date" "timestamp" "bool" True,
>         --CatCreateBinaryOp ">=" "date" "timestamptz" "bool" True,
>         CatCreateBinaryOp ">=" "time" "time" "bool" True,
>         --CatCreateBinaryOp ">=" "timestamp" "date" "bool" True,
>         CatCreateBinaryOp ">=" "timestamp" "timestamp" "bool" True,
>         --CatCreateBinaryOp ">=" "timestamp" "timestamptz" "bool" True,
>         --CatCreateBinaryOp ">=" "timestamptz" "date" "bool" True,
>         --CatCreateBinaryOp ">=" "timestamptz" "timestamp" "bool" True,
>         --CatCreateBinaryOp ">=" "timestamptz" "timestamptz" "bool" True,
>         CatCreateBinaryOp ">=" "interval" "interval" "bool" True,
>         CatCreateBinaryOp ">=" "timetz" "timetz" "bool" True,
>         CatCreateBinaryOp ">=" "bit" "bit" "bool" True,
>         CatCreateBinaryOp ">=" "varbit" "varbit" "bool" True,
>         CatCreateBinaryOp ">=" "numeric" "numeric" "bool" True,
>         CatCreateBinaryOp ">=" "record" "record" "bool" True,
>         CatCreateBinaryOp ">=" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp ">=" "uuid" "uuid" "bool" True,
>         CatCreateBinaryOp ">=" "anyenum" "anyenum" "bool" True,
>         CatCreateBinaryOp ">=" "tsvector" "tsvector" "bool" True,
>         CatCreateBinaryOp ">=" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp ">>" "int8" "int4" "int8" True,
>         CatCreateBinaryOp ">>" "int2" "int4" "int2" True,
>         CatCreateBinaryOp ">>" "int4" "int4" "int4" True,
>         CatCreateBinaryOp ">>" "point" "point" "bool" True,
>         CatCreateBinaryOp ">>" "box" "box" "bool" True,
>         CatCreateBinaryOp ">>" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp ">>" "circle" "circle" "bool" True,
>         CatCreateBinaryOp ">>" "inet" "inet" "bool" True,
>         CatCreateBinaryOp ">>" "bit" "int4" "bit" True,
>         CatCreateBinaryOp ">>=" "inet" "inet" "bool" True,
>         CatCreateBinaryOp ">^" "point" "point" "bool" True,
>         CatCreateBinaryOp ">^" "box" "box" "bool" True,
>         CatCreateBinaryOp "?#" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "?#" "lseg" "box" "bool" True,
>         CatCreateBinaryOp "?#" "lseg" "line" "bool" True,
>         CatCreateBinaryOp "?#" "path" "path" "bool" True,
>         CatCreateBinaryOp "?#" "box" "box" "bool" True,
>         CatCreateBinaryOp "?#" "line" "box" "bool" True,
>         CatCreateBinaryOp "?#" "line" "line" "bool" True,
>         CatCreateBinaryOp "?-" "point" "point" "bool" True,
>         CatCreateBinaryOp "?-|" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "?-|" "line" "line" "bool" True,
>         CatCreateBinaryOp "?|" "point" "point" "bool" True,
>         CatCreateBinaryOp "?||" "lseg" "lseg" "bool" True,
>         CatCreateBinaryOp "?||" "line" "line" "bool" True,
>         CatCreateBinaryOp "@>" "path" "point" "bool" True,
>         CatCreateBinaryOp "@>" "box" "point" "bool" True,
>         CatCreateBinaryOp "@>" "box" "box" "bool" True,
>         CatCreateBinaryOp "@>" "polygon" "point" "bool" True,
>         CatCreateBinaryOp "@>" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "@>" "circle" "point" "bool" True,
>         CatCreateBinaryOp "@>" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "@>" "_aclitem" "aclitem" "bool" True,
>         CatCreateBinaryOp "@>" "anyarray" "anyarray" "bool" True,
>         CatCreateBinaryOp "@>" "tsquery" "tsquery" "bool" True,
>         CatCreateBinaryOp "@@" "text" "text" "bool" True,
>         CatCreateBinaryOp "@@" "text" "tsquery" "bool" True,
>         CatCreateBinaryOp "@@" "tsvector" "tsquery" "bool" True,
>         CatCreateBinaryOp "@@" "tsquery" "tsvector" "bool" True,
>         CatCreateBinaryOp "@@@" "tsvector" "tsquery" "bool" True,
>         CatCreateBinaryOp "@@@" "tsquery" "tsvector" "bool" True,
>         CatCreateBinaryOp "^" "float8" "float8" "float8" True,
>         CatCreateBinaryOp "^" "numeric" "numeric" "numeric" True,
>         CatCreateBinaryOp "|" "int8" "int8" "int8" True,
>         CatCreateBinaryOp "|" "int2" "int2" "int2" True,
>         CatCreateBinaryOp "|" "int4" "int4" "int4" True,
>         CatCreateBinaryOp "|" "inet" "inet" "inet" True,
>         CatCreateBinaryOp "|" "bit" "bit" "bit" True,
>         CatCreateBinaryOp "|&>" "box" "box" "bool" True,
>         CatCreateBinaryOp "|&>" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "|&>" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "|>>" "box" "box" "bool" True,
>         CatCreateBinaryOp "|>>" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "|>>" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "||" "bytea" "bytea" "bytea" True,
>         CatCreateBinaryOp "||" "text" "text" "text" True,
>         CatCreateBinaryOp "||" "text" "anynonarray" "text" True,
>         CatCreateBinaryOp "||" "varbit" "varbit" "varbit" True,
>         CatCreateBinaryOp "||" "anyarray" "anyarray" "anyarray" True,
>         CatCreateBinaryOp "||" "anyarray" "anyelement" "anyarray" True,
>         CatCreateBinaryOp "||" "anyelement" "anyarray" "anyarray" True,
>         CatCreateBinaryOp "||" "anynonarray" "text" "text" True,
>         CatCreateBinaryOp "||" "tsvector" "tsvector" "tsvector" True,
>         CatCreateBinaryOp "||" "tsquery" "tsquery" "tsquery" True,
>         CatCreateBinaryOp "~" "name" "text" "bool" True,
>         CatCreateBinaryOp "~" "text" "text" "bool" True,
>         CatCreateBinaryOp "~" "path" "point" "bool" True,
>         CatCreateBinaryOp "~" "box" "box" "bool" True,
>         CatCreateBinaryOp "~" "polygon" "point" "bool" True,
>         CatCreateBinaryOp "~" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "~" "circle" "point" "bool" True,
>         CatCreateBinaryOp "~" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "~" "_aclitem" "aclitem" "bool" True,
>         CatCreateBinaryOp "~" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "~*" "name" "text" "bool" True,
>         CatCreateBinaryOp "~*" "text" "text" "bool" True,
>         CatCreateBinaryOp "~*" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "~<=~" "text" "text" "bool" True,
>         CatCreateBinaryOp "~<=~" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "~<~" "text" "text" "bool" True,
>         CatCreateBinaryOp "~<~" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "~=" "point" "point" "bool" True,
>         CatCreateBinaryOp "~=" "box" "box" "bool" True,
>         CatCreateBinaryOp "~=" "polygon" "polygon" "bool" True,
>         CatCreateBinaryOp "~=" "tinterval" "tinterval" "bool" True,
>         CatCreateBinaryOp "~=" "circle" "circle" "bool" True,
>         CatCreateBinaryOp "~>=~" "text" "text" "bool" True,
>         CatCreateBinaryOp "~>=~" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "~>~" "text" "text" "bool" True,
>         CatCreateBinaryOp "~>~" "bpchar" "bpchar" "bool" True,
>         CatCreateBinaryOp "~~" "bytea" "bytea" "bool" True,
>         CatCreateBinaryOp "~~" "name" "text" "bool" True,
>         CatCreateBinaryOp "~~" "text" "text" "bool" True,
>         CatCreateBinaryOp "~~" "bpchar" "text" "bool" True,
>         CatCreateBinaryOp "~~*" "name" "text" "bool" True,
>         CatCreateBinaryOp "~~*" "text" "text" "bool" True,
>         CatCreateBinaryOp "~~*" "bpchar" "text" "bool" True,
>         CatCreateFunction "abbrev" ["inet"] False "text" True,
>         CatCreateFunction "abbrev" ["cidr"] False "text" True,
>         CatCreateFunction "abs" ["float4"] False "float4" True,
>         CatCreateFunction "abs" ["float8"] False "float8" True,
>         CatCreateFunction "abs" ["int8"] False "int8" True,
>         CatCreateFunction "abs" ["int4"] False "int4" True,
>         CatCreateFunction "abs" ["int2"] False "int2" True,
>         CatCreateFunction "abs" ["numeric"] False "numeric" True,
>         CatCreateFunction "abstime" ["timestamptz"] False "abstime" True,
>         CatCreateFunction "abstime" ["timestamp"] False "abstime" True,
>         CatCreateFunction "abstimeeq" ["abstime", "abstime"] False "bool" True,
>         CatCreateFunction "abstimege" ["abstime", "abstime"] False "bool" True,
>         CatCreateFunction "abstimegt" ["abstime", "abstime"] False "bool" True,
>         CatCreateFunction "abstimein" ["cstring"] False "abstime" True,
>         CatCreateFunction "abstimele" ["abstime", "abstime"] False "bool" True,
>         CatCreateFunction "abstimelt" ["abstime", "abstime"] False "bool" True,
>         CatCreateFunction "abstimene" ["abstime", "abstime"] False "bool" True,
>         CatCreateFunction "abstimeout" ["abstime"] False "cstring" True,
>         CatCreateFunction "abstimerecv" ["internal"] False "abstime" True,
>         CatCreateFunction "abstimesend" ["abstime"] False "bytea" True,
>         CatCreateFunction "aclcontains" ["aclitem", "_aclitem"] False "bool" True,
>         CatCreateFunction "aclexplode" ["_aclitem"] True "record" True,
>         CatCreateFunction "aclinsert" ["_aclitem", "aclitem"] False "_aclitem" True,
>         CatCreateFunction "aclitemeq" ["aclitem", "aclitem"] False "bool" True,
>         CatCreateFunction "aclitemin" ["cstring"] False "aclitem" True,
>         CatCreateFunction "aclitemout" ["aclitem"] False "cstring" True,
>         CatCreateFunction "aclremove" ["_aclitem", "aclitem"] False "_aclitem" True,
>         CatCreateFunction "acos" ["float8"] False "float8" True,
>         CatCreateFunction "age" ["xid"] False "int4" True,
>         CatCreateFunction "age" ["timestamptz", "timestamptz"] False "interval" True,
>         CatCreateFunction "age" ["timestamptz"] False "interval" True,
>         CatCreateFunction "age" ["timestamp", "timestamp"] False "interval" True,
>         CatCreateFunction "age" ["timestamp"] False "interval" True,
>         CatCreateFunction "any_in" ["cstring"] False "any" True,
>         CatCreateFunction "any_out" ["any"] False "cstring" True,
>         CatCreateFunction "anyarray_in" ["cstring"] False "anyarray" True,
>         CatCreateFunction "anyarray_out" ["anyarray"] False "cstring" True,
>         CatCreateFunction "anyarray_recv" ["internal"] False "anyarray" True,
>         CatCreateFunction "anyarray_send" ["anyarray"] False "bytea" True,
>         CatCreateFunction "anyelement_in" ["cstring"] False "anyelement" True,
>         CatCreateFunction "anyelement_out" ["anyelement"] False "cstring" True,
>         CatCreateFunction "anyenum_in" ["cstring"] False "anyenum" True,
>         CatCreateFunction "anyenum_out" ["anyenum"] False "cstring" True,
>         CatCreateFunction "anynonarray_in" ["cstring"] False "anynonarray" True,
>         CatCreateFunction "anynonarray_out" ["anynonarray"] False "cstring" True,
>         CatCreateFunction "anytextcat" ["anynonarray", "text"] False "text" True,
>         CatCreateFunction "area" ["box"] False "float8" True,
>         CatCreateFunction "area" ["path"] False "float8" True,
>         CatCreateFunction "area" ["circle"] False "float8" True,
>         CatCreateFunction "areajoinsel"
>           ["internal", "int2", "internal", "oid", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "areasel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "array_agg_finalfn" ["internal"] False "anyarray" True,
>         CatCreateFunction "array_agg_transfn" ["internal", "anyelement"] False "internal" True,
>         CatCreateFunction "array_append" ["anyarray", "anyelement"] False "anyarray" True,
>         CatCreateFunction "array_cat" ["anyarray", "anyarray"] False "anyarray" True,
>         CatCreateFunction "array_dims" ["anyarray"] False "text" True,
>         CatCreateFunction "array_eq" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "array_fill" ["_int4", "anyelement"] False "anyarray" True,
>         CatCreateFunction "array_fill" ["anyelement", "_int4", "_int4"] False "anyarray" True,
>         CatCreateFunction "array_ge" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "array_gt" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "array_in" ["cstring", "oid", "int4"] False "anyarray" True,
>         CatCreateFunction "array_larger" ["anyarray", "anyarray"] False "anyarray" True,
>         CatCreateFunction "array_le" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "array_length" ["anyarray", "int4"] False "int4" True,
>         CatCreateFunction "array_lower" ["anyarray", "int4"] False "int4" True,
>         CatCreateFunction "array_lt" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "array_ndims" ["anyarray"] False "int4" True,
>         CatCreateFunction "array_ne" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "array_out" ["anyarray"] False "cstring" True,
>         CatCreateFunction "array_prepend" ["anyelement", "anyarray"] False "anyarray" True,
>         CatCreateFunction "array_recv" ["internal", "oid", "int4"] False "anyarray" True,
>         CatCreateFunction "array_send" ["anyarray"] False "bytea" True,
>         CatCreateFunction "array_smaller" ["anyarray", "anyarray"] False "anyarray" True,
>         CatCreateFunction "array_to_string" ["anyarray", "text", "text"] False "text" True,
>         CatCreateFunction "array_to_string" ["text", "anyarray"] False "text" True,
>         CatCreateFunction "array_upper" ["anyarray", "int4"] False "int4" True,
>         CatCreateFunction "arraycontained" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "arraycontains" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "arrayoverlap" ["anyarray", "anyarray"] False "bool" True,
>         CatCreateFunction "ascii" ["text"] False "int4" True,
>         CatCreateFunction "ascii_to_mic"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "ascii_to_utf8"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "asin" ["float8"] False "float8" True,
>         CatCreateFunction "atan" ["float8"] False "float8" True,
>         CatCreateFunction "atan2" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "big5_to_euc_tw"
>           ["int4", "internal", "cstring", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "big5_to_mic"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "big5_to_utf8"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "bit" ["int4", "int4"] False "bit" True,
>         CatCreateFunction "bit" ["bit", "int4", "bool"] False "bit" True,
>         CatCreateFunction "bit" ["int8", "int4"] False "bit" True,
>         CatCreateFunction "bit_in" ["cstring", "oid", "int4"] False "bit" True,
>         CatCreateFunction "bit_length" ["bytea"] False "int4" True,
>         CatCreateFunction "bit_length" ["text"] False "int4" True,
>         CatCreateFunction "bit_length" ["bit"] False "int4" True,
>         CatCreateFunction "bit_out" ["bit"] False "cstring" True,
>         CatCreateFunction "bit_recv" ["oid", "int4", "internal"] False "bit" True,
>         CatCreateFunction "bit_send" ["bit"] False "bytea" True,
>         CatCreateFunction "bitand" ["bit", "bit"] False "bit" True,
>         CatCreateFunction "bitcat" ["varbit", "varbit"] False "varbit" True,
>         CatCreateFunction "bitcmp" ["bit", "bit"] False "int4" True,
>         CatCreateFunction "biteq" ["bit", "bit"] False "bool" True,
>         CatCreateFunction "bitge" ["bit", "bit"] False "bool" True,
>         CatCreateFunction "bitgt" ["bit", "bit"] False "bool" True,
>         CatCreateFunction "bitle" ["bit", "bit"] False "bool" True,
>         CatCreateFunction "bitlt" ["bit", "bit"] False "bool" True,
>         CatCreateFunction "bitne" ["bit", "bit"] False "bool" True,
>         CatCreateFunction "bitnot" ["bit"] False "bit" True,
>         CatCreateFunction "bitor" ["bit", "bit"] False "bit" True,
>         CatCreateFunction "bitshiftleft" ["bit", "int4"] False "bit" True,
>         CatCreateFunction "bitshiftright" ["bit", "int4"] False "bit" True,
>         CatCreateFunction "bittypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "bittypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "bitxor" ["bit", "bit"] False "bit" True,
>         CatCreateFunction "bool" ["int4"] False "bool" True,
>         CatCreateFunction "booland_statefunc" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "booleq" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boolge" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boolgt" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boolin" ["cstring"] False "bool" True,
>         CatCreateFunction "boolle" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boollt" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boolne" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boolor_statefunc" ["bool", "bool"] False "bool" True,
>         CatCreateFunction "boolout" ["bool"] False "cstring" True,
>         CatCreateFunction "boolrecv" ["internal"] False "bool" True,
>         CatCreateFunction "boolsend" ["bool"] False "bytea" True,
>         CatCreateFunction "box" ["point", "point"] False "box" True,
>         CatCreateFunction "box" ["polygon"] False "box" True,
>         CatCreateFunction "box" ["circle"] False "box" True,
>         CatCreateFunction "box_above" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_above_eq" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_add" ["box", "point"] False "box" True,
>         CatCreateFunction "box_below" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_below_eq" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_center" ["box"] False "point" True,
>         CatCreateFunction "box_contain" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_contain_pt" ["box", "point"] False "bool" True,
>         CatCreateFunction "box_contained" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_distance" ["box", "box"] False "float8" True,
>         CatCreateFunction "box_div" ["box", "point"] False "box" True,
>         CatCreateFunction "box_eq" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_ge" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_gt" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_in" ["cstring"] False "box" True,
>         CatCreateFunction "box_intersect" ["box", "box"] False "box" True,
>         CatCreateFunction "box_le" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_left" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_lt" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_mul" ["box", "point"] False "box" True,
>         CatCreateFunction "box_out" ["box"] False "cstring" True,
>         CatCreateFunction "box_overabove" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_overbelow" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_overlap" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_overleft" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_overright" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_recv" ["internal"] False "box" True,
>         CatCreateFunction "box_right" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_same" ["box", "box"] False "bool" True,
>         CatCreateFunction "box_send" ["box"] False "bytea" True,
>         CatCreateFunction "box_sub" ["point", "box"] False "box" True,
>         CatCreateFunction "bpchar" ["name"] False "bpchar" True,
>         CatCreateFunction "bpchar" ["bpchar", "int4", "bool"] False "bpchar" True,
>         CatCreateFunction "bpchar" ["char"] False "bpchar" True,
>         CatCreateFunction "bpchar_larger" ["bpchar", "bpchar"] False "bpchar" True,
>         CatCreateFunction "bpchar_pattern_ge" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpchar_pattern_gt" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpchar_pattern_le" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpchar_pattern_lt" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpchar_smaller" ["bpchar", "bpchar"] False "bpchar" True,
>         CatCreateFunction "bpcharcmp" ["bpchar", "bpchar"] False "int4" True,
>         CatCreateFunction "bpchareq" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharge" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpchargt" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpchariclike" ["bpchar", "text"] False "bool" True,
>         CatCreateFunction "bpcharicnlike" ["bpchar", "text"] False "bool" True,
>         CatCreateFunction "bpcharicregexeq" ["text", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharicregexne" ["text", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharin" ["cstring", "oid", "int4"] False "bpchar" True,
>         CatCreateFunction "bpcharle" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharlike" ["bpchar", "text"] False "bool" True,
>         CatCreateFunction "bpcharlt" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharne" ["bpchar", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharnlike" ["bpchar", "text"] False "bool" True,
>         CatCreateFunction "bpcharout" ["bpchar"] False "cstring" True,
>         CatCreateFunction "bpcharrecv" ["internal", "oid", "int4"] False "bpchar" True,
>         CatCreateFunction "bpcharregexeq" ["bpchar", "text"] False "bool" True,
>         CatCreateFunction "bpcharregexne" ["text", "bpchar"] False "bool" True,
>         CatCreateFunction "bpcharsend" ["bpchar"] False "bytea" True,
>         CatCreateFunction "bpchartypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "bpchartypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "broadcast" ["inet"] False "inet" True,
>         CatCreateFunction "btabstimecmp" ["abstime", "abstime"] False "int4" True,
>         CatCreateFunction "btarraycmp" ["anyarray", "anyarray"] False "int4" True,
>         CatCreateFunction "btbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "btboolcmp" ["bool", "bool"] False "int4" True,
>         CatCreateFunction "btbpchar_pattern_cmp" ["bpchar", "bpchar"] False "int4" True,
>         CatCreateFunction "btbuild" ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "btbuildempty" ["internal"] False "void" True,
>         CatCreateFunction "btbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "btcharcmp" ["char", "char"] False "int4" True,
>         CatCreateFunction "btcostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "btendscan" ["internal"] False "void" True,
>         CatCreateFunction "btfloat48cmp" ["float4", "float8"] False "int4" True,
>         CatCreateFunction "btfloat4cmp" ["float4", "float4"] False "int4" True,
>         CatCreateFunction "btfloat84cmp" ["float8", "float4"] False "int4" True,
>         CatCreateFunction "btfloat8cmp" ["float8", "float8"] False "int4" True,
>         CatCreateFunction "btgetbitmap" ["internal", "internal"] False "int8" True,
>         CatCreateFunction "btgettuple" ["internal", "internal"] False "bool" True,
>         CatCreateFunction "btinsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "btint24cmp" ["int4", "int2"] False "int4" True,
>         CatCreateFunction "btint28cmp" ["int2", "int8"] False "int4" True,
>         CatCreateFunction "btint2cmp" ["int2", "int2"] False "int4" True,
>         CatCreateFunction "btint42cmp" ["int4", "int2"] False "int4" True,
>         CatCreateFunction "btint48cmp" ["int4", "int8"] False "int4" True,
>         CatCreateFunction "btint4cmp" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "btint82cmp" ["int8", "int2"] False "int4" True,
>         CatCreateFunction "btint84cmp" ["int4", "int8"] False "int4" True,
>         CatCreateFunction "btint8cmp" ["int8", "int8"] False "int4" True,
>         CatCreateFunction "btmarkpos" ["internal"] False "void" True,
>         CatCreateFunction "btnamecmp" ["name", "name"] False "int4" True,
>         CatCreateFunction "btoidcmp" ["oid", "oid"] False "int4" True,
>         CatCreateFunction "btoidvectorcmp" ["oidvector", "oidvector"] False "int4" True,
>         CatCreateFunction "btoptions" ["bool", "_text"] False "bytea" True,
>         CatCreateFunction "btrecordcmp" ["record", "record"] False "int4" True,
>         CatCreateFunction "btreltimecmp" ["reltime", "reltime"] False "int4" True,
>         CatCreateFunction "btrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "btrestrpos" ["internal"] False "void" True,
>         CatCreateFunction "btrim" ["text", "text"] False "text" True,
>         CatCreateFunction "btrim" ["text"] False "text" True,
>         CatCreateFunction "btrim" ["bytea", "bytea"] False "bytea" True,
>         CatCreateFunction "bttext_pattern_cmp" ["text", "text"] False "int4" True,
>         CatCreateFunction "bttextcmp" ["text", "text"] False "int4" True,
>         CatCreateFunction "bttidcmp" ["tid", "tid"] False "int4" True,
>         CatCreateFunction "bttintervalcmp" ["tinterval", "tinterval"] False "int4" True,
>         CatCreateFunction "btvacuumcleanup" ["internal", "internal"] False "internal" True,
>         CatCreateFunction "byteacat" ["bytea", "bytea"] False "bytea" True,
>         CatCreateFunction "byteacmp" ["bytea", "bytea"] False "int4" True,
>         CatCreateFunction "byteaeq" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "byteage" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "byteagt" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "byteain" ["cstring"] False "bytea" True,
>         CatCreateFunction "byteale" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "bytealike" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "bytealt" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "byteane" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "byteanlike" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "byteaout" ["bytea"] False "cstring" True,
>         CatCreateFunction "bytearecv" ["internal"] False "bytea" True,
>         CatCreateFunction "byteasend" ["bytea"] False "bytea" True,
>         CatCreateFunction "cash_cmp" ["money", "money"] False "int4" True,
>         CatCreateFunction "cash_div_cash" ["money", "money"] False "float8" True,
>         CatCreateFunction "cash_div_flt4" ["money", "float4"] False "money" True,
>         CatCreateFunction "cash_div_flt8" ["money", "float8"] False "money" True,
>         CatCreateFunction "cash_div_int2" ["money", "int2"] False "money" True,
>         CatCreateFunction "cash_div_int4" ["money", "int4"] False "money" True,
>         CatCreateFunction "cash_eq" ["money", "money"] False "bool" True,
>         CatCreateFunction "cash_ge" ["money", "money"] False "bool" True,
>         CatCreateFunction "cash_gt" ["money", "money"] False "bool" True,
>         CatCreateFunction "cash_in" ["cstring"] False "money" True,
>         CatCreateFunction "cash_le" ["money", "money"] False "bool" True,
>         CatCreateFunction "cash_lt" ["money", "money"] False "bool" True,
>         CatCreateFunction "cash_mi" ["money", "money"] False "money" True,
>         CatCreateFunction "cash_mul_flt4" ["money", "float4"] False "money" True,
>         CatCreateFunction "cash_mul_flt8" ["money", "float8"] False "money" True,
>         CatCreateFunction "cash_mul_int2" ["int2", "money"] False "money" True,
>         CatCreateFunction "cash_mul_int4" ["money", "int4"] False "money" True,
>         CatCreateFunction "cash_ne" ["money", "money"] False "bool" True,
>         CatCreateFunction "cash_out" ["money"] False "cstring" True,
>         CatCreateFunction "cash_pl" ["money", "money"] False "money" True,
>         CatCreateFunction "cash_recv" ["internal"] False "money" True,
>         CatCreateFunction "cash_send" ["money"] False "bytea" True,
>         CatCreateFunction "cash_words" ["money"] False "text" True,
>         CatCreateFunction "cashlarger" ["money", "money"] False "money" True,
>         CatCreateFunction "cashsmaller" ["money", "money"] False "money" True,
>         CatCreateFunction "cbrt" ["float8"] False "float8" True,
>         CatCreateFunction "ceil" ["numeric"] False "numeric" True,
>         CatCreateFunction "ceil" ["float8"] False "float8" True,
>         CatCreateFunction "ceiling" ["numeric"] False "numeric" True,
>         CatCreateFunction "ceiling" ["float8"] False "float8" True,
>         CatCreateFunction "center" ["box"] False "point" True,
>         CatCreateFunction "center" ["circle"] False "point" True,
>         CatCreateFunction "char" ["int4"] False "char" True,
>         CatCreateFunction "char" ["text"] False "char" True,
>         CatCreateFunction "char_length" ["bpchar"] False "int4" True,
>         CatCreateFunction "char_length" ["text"] False "int4" True,
>         CatCreateFunction "character_length" ["bpchar"] False "int4" True,
>         CatCreateFunction "character_length" ["text"] False "int4" True,
>         CatCreateFunction "chareq" ["char", "char"] False "bool" True,
>         CatCreateFunction "charge" ["char", "char"] False "bool" True,
>         CatCreateFunction "chargt" ["char", "char"] False "bool" True,
>         CatCreateFunction "charin" ["cstring"] False "char" True,
>         CatCreateFunction "charle" ["char", "char"] False "bool" True,
>         CatCreateFunction "charlt" ["char", "char"] False "bool" True,
>         CatCreateFunction "charne" ["char", "char"] False "bool" True,
>         CatCreateFunction "charout" ["char"] False "cstring" True,
>         CatCreateFunction "charrecv" ["internal"] False "char" True,
>         CatCreateFunction "charsend" ["char"] False "bytea" True,
>         CatCreateFunction "chr" ["int4"] False "text" True,
>         CatCreateFunction "cideq" ["cid", "cid"] False "bool" True,
>         CatCreateFunction "cidin" ["cstring"] False "cid" True,
>         CatCreateFunction "cidout" ["cid"] False "cstring" True,
>         CatCreateFunction "cidr" ["inet"] False "cidr" True,
>         CatCreateFunction "cidr_in" ["cstring"] False "cidr" True,
>         CatCreateFunction "cidr_out" ["cidr"] False "cstring" True,
>         CatCreateFunction "cidr_recv" ["internal"] False "cidr" True,
>         CatCreateFunction "cidr_send" ["cidr"] False "bytea" True,
>         CatCreateFunction "cidrecv" ["internal"] False "cid" True,
>         CatCreateFunction "cidsend" ["cid"] False "bytea" True,
>         CatCreateFunction "circle" ["point", "float8"] False "circle" True,
>         CatCreateFunction "circle" ["polygon"] False "circle" True,
>         CatCreateFunction "circle" ["box"] False "circle" True,
>         CatCreateFunction "circle_above" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_add_pt" ["circle", "point"] False "circle" True,
>         CatCreateFunction "circle_below" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_center" ["circle"] False "point" True,
>         CatCreateFunction "circle_contain" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_contain_pt" ["circle", "point"] False "bool" True,
>         CatCreateFunction "circle_contained" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_distance" ["circle", "circle"] False "float8" True,
>         CatCreateFunction "circle_div_pt" ["circle", "point"] False "circle" True,
>         CatCreateFunction "circle_eq" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_ge" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_gt" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_in" ["cstring"] False "circle" True,
>         CatCreateFunction "circle_le" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_left" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_lt" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_mul_pt" ["circle", "point"] False "circle" True,
>         CatCreateFunction "circle_ne" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_out" ["circle"] False "cstring" True,
>         CatCreateFunction "circle_overabove" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_overbelow" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_overlap" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_overleft" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_overright" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_recv" ["internal"] False "circle" True,
>         CatCreateFunction "circle_right" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_same" ["circle", "circle"] False "bool" True,
>         CatCreateFunction "circle_send" ["circle"] False "bytea" True,
>         CatCreateFunction "circle_sub_pt" ["point", "circle"] False "circle" True,
>         CatCreateFunction "close_lb" ["line", "box"] False "point" True,
>         CatCreateFunction "close_ls" ["line", "lseg"] False "point" True,
>         CatCreateFunction "close_lseg" ["lseg", "lseg"] False "point" True,
>         CatCreateFunction "close_pb" ["box", "point"] False "point" True,
>         CatCreateFunction "close_pl" ["point", "line"] False "point" True,
>         CatCreateFunction "close_ps" ["point", "lseg"] False "point" True,
>         CatCreateFunction "close_sb" ["box", "lseg"] False "point" True,
>         CatCreateFunction "close_sl" ["lseg", "line"] False "point" True,
>         CatCreateFunction "col_description" ["oid", "int4"] False "text" True,
>         CatCreateFunction "contjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "contsel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "convert" ["bytea", "name", "name"] False
>           "bytea"
>           True,
>         CatCreateFunction "convert_from" ["name", "bytea"] False "text" True,
>         CatCreateFunction "convert_to" ["text", "name"] False "bytea" True,
>         CatCreateFunction "cos" ["float8"] False "float8" True,
>         CatCreateFunction "cot" ["float8"] False "float8" True,
>         CatCreateFunction "cstring_in" ["cstring"] False "cstring" True,
>         CatCreateFunction "cstring_out" ["cstring"] False "cstring" True,
>         CatCreateFunction "cstring_recv" ["internal"] False "cstring" True,
>         CatCreateFunction "cstring_send" ["cstring"] False "bytea" True,
>         CatCreateFunction "current_schemas" ["bool"] False "_name" True,
>         CatCreateFunction "current_setting" ["text"] False "text" True,
>         CatCreateFunction "currtid" ["oid", "tid"] False "tid" True,
>         CatCreateFunction "currtid2" ["text", "tid"] False "tid" True,
>         CatCreateFunction "currval" ["regclass"] False "int8" True,
>         CatCreateFunction "cursor_to_xml"
>           ["refcursor", "int4", "bool", "bool", "text"]
>           False
>           "xml"
>           True,
>         CatCreateFunction "cursor_to_xmlschema"
>           ["refcursor", "bool", "bool", "text"]
>           False
>           "xml"
>           True,
>         CatCreateFunction "database_to_xml" ["bool", "bool", "text"] False
>           "xml"
>           True,
>         CatCreateFunction "database_to_xml_and_xmlschema"
>           ["text", "bool", "bool"]
>           False
>           "xml"
>           True,
>         CatCreateFunction "database_to_xmlschema" ["bool", "bool", "text"]
>           False
>           "xml"
>           True,
>         CatCreateFunction "date" ["timestamptz"] False "date" True,
>         CatCreateFunction "date" ["abstime"] False "date" True,
>         CatCreateFunction "date" ["timestamp"] False "date" True,
>         CatCreateFunction "date_cmp" ["date", "date"] False "int4" True,
>         CatCreateFunction "date_cmp_timestamp" ["date", "timestamp"] False "int4" True,
>         CatCreateFunction "date_cmp_timestamptz" ["date", "timestamptz"] False "int4" True,
>         CatCreateFunction "date_eq" ["date", "date"] False "bool" True,
>         CatCreateFunction "date_eq_timestamp" ["timestamp", "date"] False "bool" True,
>         CatCreateFunction "date_eq_timestamptz" ["date", "timestamptz"] False "bool" True,
>         CatCreateFunction "date_ge" ["date", "date"] False "bool" True,
>         CatCreateFunction "date_ge_timestamp" ["timestamp", "date"] False "bool" True,
>         CatCreateFunction "date_ge_timestamptz" ["date", "timestamptz"] False "bool" True,
>         CatCreateFunction "date_gt" ["date", "date"] False "bool" True,
>         CatCreateFunction "date_gt_timestamp" ["date", "timestamp"] False "bool" True,
>         CatCreateFunction "date_gt_timestamptz" ["date", "timestamptz"] False "bool" True,
>         CatCreateFunction "date_in" ["cstring"] False "date" True,
>         CatCreateFunction "date_larger" ["date", "date"] False "date" True,
>         CatCreateFunction "date_le" ["date", "date"] False "bool" True,
>         CatCreateFunction "date_le_timestamp" ["date", "timestamp"] False "bool" True,
>         CatCreateFunction "date_le_timestamptz" ["date", "timestamptz"] False "bool" True,
>         CatCreateFunction "date_lt" ["date", "date"] False "bool" True,
>         CatCreateFunction "date_lt_timestamp" ["timestamp", "date"] False "bool" True,
>         CatCreateFunction "date_lt_timestamptz" ["date", "timestamptz"] False "bool" True,
>         CatCreateFunction "date_mi" ["date", "date"] False "int4" True,
>         CatCreateFunction "date_mi_interval" ["date", "interval"] False "timestamp" True,
>         CatCreateFunction "date_mii" ["date", "int4"] False "date" True,
>         CatCreateFunction "date_ne" ["date", "date"] False "bool" True,
>         CatCreateFunction "date_ne_timestamp" ["date", "timestamp"] False "bool" True,
>         CatCreateFunction "date_ne_timestamptz" ["timestamptz", "date"] False "bool" True,
>         CatCreateFunction "date_out" ["date"] False "cstring" True,
>         CatCreateFunction "date_part" ["timestamptz", "text"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "interval"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "timetz"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "abstime"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "reltime"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "date"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "time"] False "float8" True,
>         CatCreateFunction "date_part" ["text", "timestamp"] False "float8" True,
>         CatCreateFunction "date_pl_interval" ["interval", "date"] False "timestamp" True,
>         CatCreateFunction "date_pli" ["date", "int4"] False "date" True,
>         CatCreateFunction "date_recv" ["internal"] False "date" True,
>         CatCreateFunction "date_send" ["date"] False "bytea" True,
>         CatCreateFunction "date_smaller" ["date", "date"] False "date" True,
>         CatCreateFunction "date_trunc" ["text", "timestamptz"] False "timestamptz" True,
>         CatCreateFunction "date_trunc" ["interval", "text"] False "interval" True,
>         CatCreateFunction "date_trunc" ["text", "timestamp"] False "timestamp" True,
>         CatCreateFunction "datetime_pl" ["date", "time"] False "timestamp" True,
>         CatCreateFunction "datetimetz_pl" ["date", "timetz"] False "timestamptz" True,
>         CatCreateFunction "dcbrt" ["float8"] False "float8" True,
>         CatCreateFunction "decode" ["text", "text"] False "bytea" True,
>         CatCreateFunction "degrees" ["float8"] False "float8" True,
>         CatCreateFunction "dexp" ["float8"] False "float8" True,
>         CatCreateFunction "diagonal" ["box"] False "lseg" True,
>         CatCreateFunction "diameter" ["circle"] False "float8" True,
>         CatCreateFunction "dispell_init" ["internal"] False "internal" True,
>         CatCreateFunction "dispell_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "dist_cpoly" ["polygon", "circle"] False "float8" True,
>         CatCreateFunction "dist_lb" ["box", "line"] False "float8" True,
>         CatCreateFunction "dist_pb" ["box", "point"] False "float8" True,
>         CatCreateFunction "dist_pc" ["point", "circle"] False "float8" True,
>         CatCreateFunction "dist_pl" ["line", "point"] False "float8" True,
>         CatCreateFunction "dist_ppath" ["point", "path"] False "float8" True,
>         CatCreateFunction "dist_ps" ["point", "lseg"] False "float8" True,
>         CatCreateFunction "dist_sb" ["lseg", "box"] False "float8" True,
>         CatCreateFunction "dist_sl" ["lseg", "line"] False "float8" True,
>         CatCreateFunction "div" ["numeric", "numeric"] False "numeric" True,
>         CatCreateFunction "dlog1" ["float8"] False "float8" True,
>         CatCreateFunction "dlog10" ["float8"] False "float8" True,
>         CatCreateFunction "domain_in" ["cstring", "oid", "int4"] False "any" True,
>         CatCreateFunction "domain_recv" ["internal", "oid", "int4"] False "any" True,
>         CatCreateFunction "dpow" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "dround" ["float8"] False "float8" True,
>         CatCreateFunction "dsimple_init" ["internal"] False "internal" True,
>         CatCreateFunction "dsimple_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "dsnowball_init" ["internal"] False "internal" True,
>         CatCreateFunction "dsnowball_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "dsqrt" ["float8"] False "float8" True,
>         CatCreateFunction "dsynonym_init" ["internal"] False "internal" True,
>         CatCreateFunction "dsynonym_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "dtrunc" ["float8"] False "float8" True,
>         CatCreateFunction "encode" ["bytea", "text"] False "text" True,
>         CatCreateFunction "enum_cmp" ["anyenum", "anyenum"] False "int4" True,
>         CatCreateFunction "enum_eq" ["anyenum", "anyenum"] False "bool" True,
>         CatCreateFunction "enum_first" ["anyenum"] False "anyenum" True,
>         CatCreateFunction "enum_ge" ["anyenum", "anyenum"] False "bool" True,
>         CatCreateFunction "enum_gt" ["anyenum", "anyenum"] False "bool" True,
>         CatCreateFunction "enum_in" ["oid", "cstring"] False "anyenum" True,
>         CatCreateFunction "enum_larger" ["anyenum", "anyenum"] False "anyenum" True,
>         CatCreateFunction "enum_last" ["anyenum"] False "anyenum" True,
>         CatCreateFunction "enum_le" ["anyenum", "anyenum"] False "bool" True,
>         CatCreateFunction "enum_lt" ["anyenum", "anyenum"] False "bool" True,
>         CatCreateFunction "enum_ne" ["anyenum", "anyenum"] False "bool" True,
>         CatCreateFunction "enum_out" ["anyenum"] False "cstring" True,
>         CatCreateFunction "enum_range" ["anyenum", "anyenum"] False "anyarray" True,
>         CatCreateFunction "enum_range" ["anyenum"] False "anyarray" True,
>         CatCreateFunction "enum_recv" ["oid", "cstring"] False "anyenum" True,
>         CatCreateFunction "enum_send" ["anyenum"] False "bytea" True,
>         CatCreateFunction "enum_smaller" ["anyenum", "anyenum"] False "anyenum" True,
>         CatCreateFunction "eqjoinsel"
>           ["oid", "internal", "internal", "int2", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "eqsel" ["oid", "internal", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "euc_cn_to_mic"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_cn_to_utf8"
>           ["int4", "internal", "int4", "cstring", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_jis_2004_to_shift_jis_2004"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_jis_2004_to_utf8"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_jp_to_mic"
>           ["int4", "cstring", "int4", "int4", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_jp_to_sjis"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_jp_to_utf8"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_kr_to_mic"
>           ["cstring", "internal", "int4", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_kr_to_utf8"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_tw_to_big5"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_tw_to_mic"
>           ["int4", "internal", "cstring", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "euc_tw_to_utf8"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "exp" ["float8"] False "float8" True,
>         CatCreateFunction "exp" ["numeric"] False "numeric" True,
>         CatCreateFunction "factorial" ["int8"] False "numeric" True,
>         CatCreateFunction "family" ["inet"] False "int4" True,
>         CatCreateFunction "fdw_handler_in" ["cstring"] False "fdw_handler" True,
>         CatCreateFunction "fdw_handler_out" ["fdw_handler"] False "cstring" True,
>         CatCreateFunction "float4" ["int2"] False "float4" True,
>         CatCreateFunction "float4" ["float8"] False "float4" True,
>         CatCreateFunction "float4" ["int4"] False "float4" True,
>         CatCreateFunction "float4" ["int8"] False "float4" True,
>         CatCreateFunction "float4" ["numeric"] False "float4" True,
>         CatCreateFunction "float48div" ["float4", "float8"] False "float8" True,
>         CatCreateFunction "float48eq" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float48ge" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float48gt" ["float8", "float4"] False "bool" True,
>         CatCreateFunction "float48le" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float48lt" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float48mi" ["float8", "float4"] False "float8" True,
>         CatCreateFunction "float48mul" ["float4", "float8"] False "float8" True,
>         CatCreateFunction "float48ne" ["float8", "float4"] False "bool" True,
>         CatCreateFunction "float48pl" ["float4", "float8"] False "float8" True,
>         CatCreateFunction "float4_accum" ["_float8", "float4"] False "_float8" True,
>         CatCreateFunction "float4abs" ["float4"] False "float4" True,
>         CatCreateFunction "float4div" ["float4", "float4"] False "float4" True,
>         CatCreateFunction "float4eq" ["float4", "float4"] False "bool" True,
>         CatCreateFunction "float4ge" ["float4", "float4"] False "bool" True,
>         CatCreateFunction "float4gt" ["float4", "float4"] False "bool" True,
>         CatCreateFunction "float4in" ["cstring"] False "float4" True,
>         CatCreateFunction "float4larger" ["float4", "float4"] False "float4" True,
>         CatCreateFunction "float4le" ["float4", "float4"] False "bool" True,
>         CatCreateFunction "float4lt" ["float4", "float4"] False "bool" True,
>         CatCreateFunction "float4mi" ["float4", "float4"] False "float4" True,
>         CatCreateFunction "float4mul" ["float4", "float4"] False "float4" True,
>         CatCreateFunction "float4ne" ["float4", "float4"] False "bool" True,
>         CatCreateFunction "float4out" ["float4"] False "cstring" True,
>         CatCreateFunction "float4pl" ["float4", "float4"] False "float4" True,
>         CatCreateFunction "float4recv" ["internal"] False "float4" True,
>         CatCreateFunction "float4send" ["float4"] False "bytea" True,
>         CatCreateFunction "float4smaller" ["float4", "float4"] False "float4" True,
>         CatCreateFunction "float4um" ["float4"] False "float4" True,
>         CatCreateFunction "float4up" ["float4"] False "float4" True,
>         CatCreateFunction "float8" ["int2"] False "float8" True,
>         CatCreateFunction "float8" ["float4"] False "float8" True,
>         CatCreateFunction "float8" ["int4"] False "float8" True,
>         CatCreateFunction "float8" ["int8"] False "float8" True,
>         CatCreateFunction "float8" ["numeric"] False "float8" True,
>         CatCreateFunction "float84div" ["float4", "float8"] False "float8" True,
>         CatCreateFunction "float84eq" ["float8", "float4"] False "bool" True,
>         CatCreateFunction "float84ge" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float84gt" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float84le" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float84lt" ["float8", "float4"] False "bool" True,
>         CatCreateFunction "float84mi" ["float8", "float4"] False "float8" True,
>         CatCreateFunction "float84mul" ["float8", "float4"] False "float8" True,
>         CatCreateFunction "float84ne" ["float4", "float8"] False "bool" True,
>         CatCreateFunction "float84pl" ["float4", "float8"] False "float8" True,
>         CatCreateFunction "float8_accum" ["_float8", "float8"] False "_float8" True,
>         CatCreateFunction "float8_avg" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_corr" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_covar_pop" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_covar_samp" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_accum"
>           ["_float8", "float8", "float8"]
>           False
>           "_float8"
>           True,
>         CatCreateFunction "float8_regr_avgx" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_avgy" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_intercept" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_r2" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_slope" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_sxx" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_sxy" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_regr_syy" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_stddev_pop" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_stddev_samp" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_var_pop" ["_float8"] False "float8" True,
>         CatCreateFunction "float8_var_samp" ["_float8"] False "float8" True,
>         CatCreateFunction "float8abs" ["float8"] False "float8" True,
>         CatCreateFunction "float8div" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "float8eq" ["float8", "float8"] False "bool" True,
>         CatCreateFunction "float8ge" ["float8", "float8"] False "bool" True,
>         CatCreateFunction "float8gt" ["float8", "float8"] False "bool" True,
>         CatCreateFunction "float8in" ["cstring"] False "float8" True,
>         CatCreateFunction "float8larger" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "float8le" ["float8", "float8"] False "bool" True,
>         CatCreateFunction "float8lt" ["float8", "float8"] False "bool" True,
>         CatCreateFunction "float8mi" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "float8mul" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "float8ne" ["float8", "float8"] False "bool" True,
>         CatCreateFunction "float8out" ["float8"] False "cstring" True,
>         CatCreateFunction "float8pl" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "float8recv" ["internal"] False "float8" True,
>         CatCreateFunction "float8send" ["float8"] False "bytea" True,
>         CatCreateFunction "float8smaller" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "float8um" ["float8"] False "float8" True,
>         CatCreateFunction "float8up" ["float8"] False "float8" True,
>         CatCreateFunction "floor" ["numeric"] False "numeric" True,
>         CatCreateFunction "floor" ["float8"] False "float8" True,
>         CatCreateFunction "flt4_mul_cash" ["float4", "money"] False "money" True,
>         CatCreateFunction "flt8_mul_cash" ["money", "float8"] False "money" True,
>         CatCreateFunction "fmgr_c_validator" ["oid"] False "void" True,
>         CatCreateFunction "fmgr_internal_validator" ["oid"] False "void" True,
>         CatCreateFunction "fmgr_sql_validator" ["oid"] False "void" True,
>         CatCreateFunction "format" ["text"] False "text" True,
>         CatCreateFunction "format_type" ["oid", "int4"] False "text" True,
>         CatCreateFunction "gb18030_to_utf8"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "gbk_to_utf8"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "generate_series"
>           ["interval", "timestamp", "timestamp"]
>           True
>           "timestamp"
>           True,
>         CatCreateFunction "generate_series"
>           ["timestamptz", "timestamptz", "interval"]
>           True
>           "timestamptz"
>           True,
>         CatCreateFunction "generate_series" ["int4", "int4", "int4"] True "int4" True,
>         CatCreateFunction "generate_series" ["int4", "int4"] True "int4" True,
>         CatCreateFunction "generate_series" ["int8", "int8", "int8"] True "int8" True,
>         CatCreateFunction "generate_series" ["int8", "int8"] True "int8" True,
>         CatCreateFunction "generate_subscripts"
>           ["anyarray", "int4", "bool"]
>           True
>           "int4"
>           True,
>         CatCreateFunction "generate_subscripts" ["anyarray", "int4"] True "int4" True,
>         CatCreateFunction "get_bit" ["bytea", "int4"] False "int4" True,
>         CatCreateFunction "get_bit" ["bit", "int4"] False "int4" True,
>         CatCreateFunction "get_byte" ["bytea", "int4"] False "int4" True,
>         CatCreateFunction "gin_cmp_prefix"
>           ["text", "text", "int2", "internal"]
>           False
>           "int4"
>           True,
>         CatCreateFunction "gin_cmp_tslexeme" ["text", "text"] False "int4" True,
>         CatCreateFunction "gin_extract_tsquery"
>           ["internal", "int2", "internal", "tsquery", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gin_extract_tsquery"
>           ["internal", "tsquery", "internal", "int2", "internal", "internal",
>            "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "gin_extract_tsvector" ["internal", "tsvector"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gin_extract_tsvector"
>           ["internal", "tsvector", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gin_tsquery_consistent"
>           ["internal", "internal", "internal", "tsquery", "int2", "int4"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gin_tsquery_consistent"
>           ["internal", "tsquery", "internal", "internal", "int2", "int4",
>            "internal", "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "ginarrayconsistent"
>           ["internal", "internal", "int2", "int4", "internal", "internal",
>            "anyarray", "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "ginarrayextract"
>           ["internal", "anyarray", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "ginarrayextract" ["anyarray", "internal"] False
>           "internal"
>           True,
>         CatCreateFunction "ginbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "ginbuild" ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "ginbuildempty" ["internal"] False "void" True,
>         CatCreateFunction "ginbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gincostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "ginendscan" ["internal"] False "void" True,
>         CatCreateFunction "gingetbitmap" ["internal", "internal"] False
>           "int8"
>           True,
>         CatCreateFunction "gininsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool" True,
>         CatCreateFunction "ginmarkpos" ["internal"] False "void" True,
>         CatCreateFunction "ginoptions" ["_text", "bool"] False "bytea" True,
>         CatCreateFunction "ginqueryarrayextract"
>           ["internal", "anyarray", "internal", "int2", "internal",
>            "internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "ginrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "ginrestrpos" ["internal"] False "void" True,
>         CatCreateFunction "ginvacuumcleanup" ["internal", "internal"] False "internal" True,
>         CatCreateFunction "gist_box_compress" ["internal"] False "internal" True,
>         CatCreateFunction "gist_box_consistent"
>           ["internal", "int4", "internal", "box", "oid"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gist_box_decompress" ["internal"] False "internal" True,
>         CatCreateFunction "gist_box_penalty"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gist_box_picksplit" ["internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gist_box_same" ["box", "box", "internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gist_box_union" ["internal", "internal"] False
>           "box"
>           True,
>         CatCreateFunction "gist_circle_compress" ["internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gist_circle_consistent"
>           ["int4", "internal", "internal", "circle", "oid"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gist_point_compress" ["internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gist_point_consistent"
>           ["internal", "point", "int4", "internal", "oid"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gist_point_distance"
>           ["int4", "internal", "point", "oid"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "gist_poly_compress" ["internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gist_poly_consistent"
>           ["polygon", "oid", "internal", "int4", "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gistbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gistbuild" ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gistbuildempty" ["internal"] False "void" True,
>         CatCreateFunction "gistbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gistcostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "gistendscan" ["internal"] False "void" True,
>         CatCreateFunction "gistgetbitmap" ["internal", "internal"] False
>           "int8"
>           True,
>         CatCreateFunction "gistgettuple" ["internal", "internal"] False
>           "bool"
>           True,
>         CatCreateFunction "gistinsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gistmarkpos" ["internal"] False "void" True,
>         CatCreateFunction "gistoptions" ["_text", "bool"] False "bytea" True,
>         CatCreateFunction "gistrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "gistrestrpos" ["internal"] False "void" True,
>         CatCreateFunction "gistvacuumcleanup" ["internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gtsquery_compress" ["internal"] False "internal" True,
>         CatCreateFunction "gtsquery_consistent"
>           ["internal", "oid", "int4", "internal", "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gtsquery_decompress" ["internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gtsquery_penalty"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gtsquery_picksplit" ["internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gtsquery_same" ["int8", "internal", "int8"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gtsquery_union" ["internal", "internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gtsvector_compress" ["internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gtsvector_consistent"
>           ["oid", "internal", "internal", "gtsvector", "int4"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "gtsvector_decompress" ["internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gtsvector_penalty"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gtsvector_picksplit" ["internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "gtsvector_same"
>           ["internal", "gtsvector", "gtsvector"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "gtsvector_union" ["internal", "internal"] False
>           "internal"
>           True,
>         CatCreateFunction "gtsvectorin" ["cstring"] False "gtsvector" True,
>         CatCreateFunction "gtsvectorout" ["gtsvector"] False "cstring" True,
>         CatCreateFunction "has_any_column_privilege"
>           ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_any_column_privilege"
>           ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_any_column_privilege"
>           ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_any_column_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_any_column_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_any_column_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["name", "text", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["name", "text", "int2", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["name", "oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["name", "oid", "int2", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["oid", "text", "text", "text"]
>           False
>           "bool" True,
>         CatCreateFunction "has_column_privilege"
>           ["oid", "text", "int2", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["oid", "oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege"
>           ["oid", "oid", "int2", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege" ["text", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege" ["text", "int2", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege" ["text", "text", "oid"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_column_privilege" ["int2", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_database_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_database_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_database_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_database_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_database_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_database_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_foreign_data_wrapper_privilege"
>           ["oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_function_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_function_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_function_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_function_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_function_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_function_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_language_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_language_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_language_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_language_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_language_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_language_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_schema_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_schema_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_schema_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_schema_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_schema_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_schema_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_sequence_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_sequence_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_sequence_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_sequence_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_sequence_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_sequence_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_server_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_server_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_server_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_server_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_server_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_server_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_table_privilege" ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_table_privilege" ["name", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_table_privilege" ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_table_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_table_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_table_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_tablespace_privilege"
>           ["name", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_tablespace_privilege"
>           ["oid", "text", "name"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_tablespace_privilege"
>           ["oid", "text", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_tablespace_privilege" ["oid", "oid", "text"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "has_tablespace_privilege" ["text", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "has_tablespace_privilege" ["oid", "text"] False
>           "bool"
>           True,
>         CatCreateFunction "hash_aclitem" ["aclitem"] False "int4" True,
>         CatCreateFunction "hash_array" ["anyarray"] False "int4" True,
>         CatCreateFunction "hash_numeric" ["numeric"] False "int4" True,
>         CatCreateFunction "hashbeginscan"
>           ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "hashbpchar" ["bpchar"] False "int4" True,
>         CatCreateFunction "hashbuild" ["internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "hashbuildempty" ["internal"] False "void" True,
>         CatCreateFunction "hashbulkdelete"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "hashchar" ["char"] False "int4" True,
>         CatCreateFunction "hashcostestimate"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "hashendscan" ["internal"] False "void" True,
>         CatCreateFunction "hashenum" ["anyenum"] False "int4" True,
>         CatCreateFunction "hashfloat4" ["float4"] False "int4" True,
>         CatCreateFunction "hashfloat8" ["float8"] False "int4" True,
>         CatCreateFunction "hashgetbitmap" ["internal", "internal"] False
>           "int8"
>           True,
>         CatCreateFunction "hashgettuple" ["internal", "internal"] False
>           "bool"
>           True,
>         CatCreateFunction "hashinet" ["inet"] False "int4" True,
>         CatCreateFunction "hashinsert"
>           ["internal", "internal", "internal", "internal", "internal",
>            "internal"]
>           False
>           "bool"
>           True,
>         CatCreateFunction "hashint2" ["int2"] False "int4" True,
>         CatCreateFunction "hashint2vector" ["int2vector"] False "int4" True,
>         CatCreateFunction "hashint4" ["int4"] False "int4" True,
>         CatCreateFunction "hashint8" ["int8"] False "int4" True,
>         CatCreateFunction "hashmacaddr" ["macaddr"] False "int4" True,
>         CatCreateFunction "hashmarkpos" ["internal"] False "void" True,
>         CatCreateFunction "hashname" ["name"] False "int4" True,
>         CatCreateFunction "hashoid" ["oid"] False "int4" True,
>         CatCreateFunction "hashoidvector" ["oidvector"] False "int4" True,
>         CatCreateFunction "hashoptions" ["_text", "bool"] False "bytea" True,
>         CatCreateFunction "hashrescan"
>           ["internal", "internal", "internal", "internal", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "hashrestrpos" ["internal"] False "void" True,
>         CatCreateFunction "hashtext" ["text"] False "int4" True,
>         CatCreateFunction "hashvacuumcleanup" ["internal", "internal"]
>           False
>           "internal"
>           True,
>         CatCreateFunction "hashvarlena" ["internal"] False "int4" True,
>         CatCreateFunction "height" ["box"] False "float8" True,
>         CatCreateFunction "host" ["inet"] False "text" True,
>         CatCreateFunction "hostmask" ["inet"] False "inet" True,
>         CatCreateFunction "iclikejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "iclikesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "icnlikejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "icnlikesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "icregexeqjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "icregexeqsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "icregexnejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "icregexnesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8"
>           True,
>         CatCreateFunction "inet_in" ["cstring"] False "inet" True,
>         CatCreateFunction "inet_out" ["inet"] False "cstring" True,
>         CatCreateFunction "inet_recv" ["internal"] False "inet" True,
>         CatCreateFunction "inet_send" ["inet"] False "bytea" True,
>         CatCreateFunction "inetand" ["inet", "inet"] False "inet" True,
>         CatCreateFunction "inetmi" ["inet", "inet"] False "int8" True,
>         CatCreateFunction "inetmi_int8" ["inet", "int8"] False "inet" True,
>         CatCreateFunction "inetnot" ["inet"] False "inet" True,
>         CatCreateFunction "inetor" ["inet", "inet"] False "inet" True,
>         CatCreateFunction "inetpl" ["inet", "int8"] False "inet" True,
>         CatCreateFunction "initcap" ["text"] False "text" True,
>         CatCreateFunction "int2" ["float8"] False "int2" True,
>         CatCreateFunction "int2" ["float4"] False "int2" True,
>         CatCreateFunction "int2" ["int4"] False "int2" True,
>         CatCreateFunction "int2" ["int8"] False "int2" True,
>         CatCreateFunction "int2" ["numeric"] False "int2" True,
>         CatCreateFunction "int24div" ["int2", "int4"] False "int4" True,
>         CatCreateFunction "int24eq" ["int4", "int2"] False "bool" True,
>         CatCreateFunction "int24ge" ["int2", "int4"] False "bool" True,
>         CatCreateFunction "int24gt" ["int4", "int2"] False "bool" True,
>         CatCreateFunction "int24le" ["int2", "int4"] False "bool" True,
>         CatCreateFunction "int24lt" ["int2", "int4"] False "bool" True,
>         CatCreateFunction "int24mi" ["int2", "int4"] False "int4" True,
>         CatCreateFunction "int24mul" ["int2", "int4"] False "int4" True,
>         CatCreateFunction "int24ne" ["int4", "int2"] False "bool" True,
>         CatCreateFunction "int24pl" ["int2", "int4"] False "int4" True,
>         CatCreateFunction "int28div" ["int2", "int8"] False "int8" True,
>         CatCreateFunction "int28eq" ["int2", "int8"] False "bool" True,
>         CatCreateFunction "int28ge" ["int2", "int8"] False "bool" True,
>         CatCreateFunction "int28gt" ["int2", "int8"] False "bool" True,
>         CatCreateFunction "int28le" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int28lt" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int28mi" ["int2", "int8"] False "int8" True,
>         CatCreateFunction "int28mul" ["int8", "int2"] False "int8" True,
>         CatCreateFunction "int28ne" ["int2", "int8"] False "bool" True,
>         CatCreateFunction "int28pl" ["int2", "int8"] False "int8" True,
>         CatCreateFunction "int2_accum" ["_numeric", "int2"] False
>           "_numeric"
>           True,
>         CatCreateFunction "int2_avg_accum" ["_int8", "int2"] False "_int8" True,
>         CatCreateFunction "int2_mul_cash" ["int2", "money"] False "money" True,
>         CatCreateFunction "int2_sum" ["int8", "int2"] False "int8" True,
>         CatCreateFunction "int2abs" ["int2"] False "int2" True,
>         CatCreateFunction "int2and" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2div" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2eq" ["int2", "int2"] False "bool" True,
>         CatCreateFunction "int2ge" ["int2", "int2"] False "bool" True,
>         CatCreateFunction "int2gt" ["int2", "int2"] False "bool" True,
>         CatCreateFunction "int2in" ["cstring"] False "int2" True,
>         CatCreateFunction "int2larger" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2le" ["int2", "int2"] False "bool" True,
>         CatCreateFunction "int2lt" ["int2", "int2"] False "bool" True,
>         CatCreateFunction "int2mi" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2mod" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2mul" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2ne" ["int2", "int2"] False "bool" True,
>         CatCreateFunction "int2not" ["int2"] False "int2" True,
>         CatCreateFunction "int2or" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2out" ["int2"] False "cstring" True,
>         CatCreateFunction "int2pl" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2recv" ["internal"] False "int2" True,
>         CatCreateFunction "int2send" ["int2"] False "bytea" True,
>         CatCreateFunction "int2shl" ["int2", "int4"] False "int2" True,
>         CatCreateFunction "int2shr" ["int2", "int4"] False "int2" True,
>         CatCreateFunction "int2smaller" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int2um" ["int2"] False "int2" True,
>         CatCreateFunction "int2up" ["int2"] False "int2" True,
>         CatCreateFunction "int2vectoreq" ["int2vector", "int2vector"] False
>           "bool"
>           True,
>         CatCreateFunction "int2vectorin" ["cstring"] False "int2vector" True,
>         CatCreateFunction "int2vectorout" ["int2vector"] False "cstring" True,
>         CatCreateFunction "int2vectorrecv" ["internal"] False "int2vector" True,
>         CatCreateFunction "int2vectorsend" ["int2vector"] False "bytea" True,
>         CatCreateFunction "int2xor" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "int4" ["char"] False "int4" True,
>         CatCreateFunction "int4" ["int2"] False "int4" True,
>         CatCreateFunction "int4" ["float8"] False "int4" True,
>         CatCreateFunction "int4" ["float4"] False "int4" True,
>         CatCreateFunction "int4" ["int8"] False "int4" True,
>         CatCreateFunction "int4" ["bit"] False "int4" True,
>         CatCreateFunction "int4" ["numeric"] False "int4" True,
>         CatCreateFunction "int4" ["bool"] False "int4" True,
>         CatCreateFunction "int42div" ["int4", "int2"] False "int4" True,
>         CatCreateFunction "int42eq" ["int4", "int2"] False "bool" True,
>         CatCreateFunction "int42ge" ["int2", "int4"] False "bool" True,
>         CatCreateFunction "int42gt" ["int2", "int4"] False "bool" True,
>         CatCreateFunction "int42le" ["int4", "int2"] False "bool" True,
>         CatCreateFunction "int42lt" ["int2", "int4"] False "bool" True,
>         CatCreateFunction "int42mi" ["int4", "int2"] False "int4" True,
>         CatCreateFunction "int42mul" ["int4", "int2"] False "int4" True,
>         CatCreateFunction "int42ne" ["int4", "int2"] False "bool" True,
>         CatCreateFunction "int42pl" ["int4", "int2"] False "int4" True,
>         CatCreateFunction "int48div" ["int4", "int8"] False "int8" True,
>         CatCreateFunction "int48eq" ["int4", "int8"] False "bool" True,
>         CatCreateFunction "int48ge" ["int4", "int8"] False "bool" True,
>         CatCreateFunction "int48gt" ["int4", "int8"] False "bool" True,
>         CatCreateFunction "int48le" ["int4", "int8"] False "bool" True,
>         CatCreateFunction "int48lt" ["int4", "int8"] False "bool" True,
>         CatCreateFunction "int48mi" ["int4", "int8"] False "int8" True,
>         CatCreateFunction "int48mul" ["int4", "int8"] False "int8" True,
>         CatCreateFunction "int48ne" ["int4", "int8"] False "bool" True,
>         CatCreateFunction "int48pl" ["int4", "int8"] False "int8" True,
>         CatCreateFunction "int4_accum" ["_numeric", "int4"] False
>           "_numeric"
>           True,
>         CatCreateFunction "int4_avg_accum" ["_int8", "int4"] False "_int8" True,
>         CatCreateFunction "int4_mul_cash" ["int4", "money"] False "money" True,
>         CatCreateFunction "int4_sum" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int4abs" ["int4"] False "int4" True,
>         CatCreateFunction "int4and" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4div" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4eq" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "int4ge" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "int4gt" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "int4in" ["cstring"] False "int4" True,
>         CatCreateFunction "int4inc" ["int4"] False "int4" True,
>         CatCreateFunction "int4larger" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4le" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "int4lt" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "int4mi" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4mod" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4mul" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4ne" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "int4not" ["int4"] False "int4" True,
>         CatCreateFunction "int4or" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4out" ["int4"] False "cstring" True,
>         CatCreateFunction "int4pl" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4recv" ["internal"] False "int4" True,
>         CatCreateFunction "int4send" ["int4"] False "bytea" True,
>         CatCreateFunction "int4shl" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4shr" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4smaller" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int4um" ["int4"] False "int4" True,
>         CatCreateFunction "int4up" ["int4"] False "int4" True,
>         CatCreateFunction "int4xor" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "int8" ["int4"] False "int8" True,
>         CatCreateFunction "int8" ["float8"] False "int8" True,
>         CatCreateFunction "int8" ["float4"] False "int8" True,
>         CatCreateFunction "int8" ["int2"] False "int8" True,
>         CatCreateFunction "int8" ["oid"] False "int8" True,
>         CatCreateFunction "int8" ["numeric"] False "int8" True,
>         CatCreateFunction "int8" ["bit"] False "int8" True,
>         CatCreateFunction "int82div" ["int2", "int8"] False "int8" True,
>         CatCreateFunction "int82eq" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int82ge" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int82gt" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int82le" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int82lt" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int82mi" ["int8", "int2"] False "int8" True,
>         CatCreateFunction "int82mul" ["int8", "int2"] False "int8" True,
>         CatCreateFunction "int82ne" ["int8", "int2"] False "bool" True,
>         CatCreateFunction "int82pl" ["int8", "int2"] False "int8" True,
>         CatCreateFunction "int84div" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int84eq" ["int8", "int4"] False "bool" True,
>         CatCreateFunction "int84ge" ["int8", "int4"] False "bool" True,
>         CatCreateFunction "int84gt" ["int8", "int4"] False "bool" True,
>         CatCreateFunction "int84le" ["int8", "int4"] False "bool" True,
>         CatCreateFunction "int84lt" ["int8", "int4"] False "bool" True,
>         CatCreateFunction "int84mi" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int84mul" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int84ne" ["int8", "int4"] False "bool" True,
>         CatCreateFunction "int84pl" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int8_accum" ["_numeric", "int8"] False "_numeric" True,
>         CatCreateFunction "int8_avg" ["_int8"] False "numeric" True,
>         CatCreateFunction "int8_avg_accum" ["_numeric", "int8"] False "_numeric" True,
>         CatCreateFunction "int8_sum" ["numeric", "int8"] False "numeric" True,
>         CatCreateFunction "int8abs" ["int8"] False "int8" True,
>         CatCreateFunction "int8and" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8div" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8eq" ["int8", "int8"] False "bool" True,
>         CatCreateFunction "int8ge" ["int8", "int8"] False "bool" True,
>         CatCreateFunction "int8gt" ["int8", "int8"] False "bool" True,
>         CatCreateFunction "int8in" ["cstring"] False "int8" True,
>         CatCreateFunction "int8inc" ["int8"] False "int8" True,
>         CatCreateFunction "int8inc_any" ["int8", "any"] False "int8" True,
>         CatCreateFunction "int8inc_float8_float8"
>           ["int8", "float8", "float8"]
>           False
>           "int8" True,
>         CatCreateFunction "int8larger" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8le" ["int8", "int8"] False "bool" True,
>         CatCreateFunction "int8lt" ["int8", "int8"] False "bool" True,
>         CatCreateFunction "int8mi" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8mod" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8mul" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8ne" ["int8", "int8"] False "bool" True,
>         CatCreateFunction "int8not" ["int8"] False "int8" True,
>         CatCreateFunction "int8or" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8out" ["int8"] False "cstring" True,
>         CatCreateFunction "int8pl" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8pl_inet" ["int8", "inet"] False "inet" True,
>         CatCreateFunction "int8recv" ["internal"] False "int8" True,
>         CatCreateFunction "int8send" ["int8"] False "bytea" True,
>         CatCreateFunction "int8shl" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int8shr" ["int8", "int4"] False "int8" True,
>         CatCreateFunction "int8smaller" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "int8um" ["int8"] False "int8" True,
>         CatCreateFunction "int8up" ["int8"] False "int8" True,
>         CatCreateFunction "int8xor" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "integer_pl_date" ["int4", "date"] False "date" True,
>         CatCreateFunction "inter_lb" ["line", "box"] False "bool" True,
>         CatCreateFunction "inter_sb" ["lseg", "box"] False "bool" True,
>         CatCreateFunction "inter_sl" ["lseg", "line"] False "bool" True,
>         CatCreateFunction "internal_in" ["cstring"] False "internal" True,
>         CatCreateFunction "internal_out" ["internal"] False "cstring" True,
>         CatCreateFunction "interval" ["reltime"] False "interval" True,
>         CatCreateFunction "interval" ["interval", "int4"] False "interval" True,
>         CatCreateFunction "interval" ["time"] False "interval" True,
>         CatCreateFunction "interval_accum" ["_interval", "interval"] False "_interval" True,
>         CatCreateFunction "interval_avg" ["_interval"] False "interval" True,
>         CatCreateFunction "interval_cmp" ["interval", "interval"] False "int4" True,
>         CatCreateFunction "interval_div" ["float8", "interval"] False "interval" True,
>         CatCreateFunction "interval_eq" ["interval", "interval"] False "bool" True,
>         CatCreateFunction "interval_ge" ["interval", "interval"] False "bool" True,
>         CatCreateFunction "interval_gt" ["interval", "interval"] False "bool" True,
>         CatCreateFunction "interval_hash" ["interval"] False "int4" True,
>         CatCreateFunction "interval_in" ["cstring", "oid", "int4"] False "interval" True,
>         CatCreateFunction "interval_larger" ["interval", "interval"] False "interval" True,
>         CatCreateFunction "interval_le" ["interval", "interval"] False "bool" True,
>         CatCreateFunction "interval_lt" ["interval", "interval"] False "bool" True,
>         CatCreateFunction "interval_mi" ["interval", "interval"] False "interval" True,
>         CatCreateFunction "interval_mul" ["float8", "interval"] False "interval" True,
>         CatCreateFunction "interval_ne" ["interval", "interval"] False "bool" True,
>         CatCreateFunction "interval_out" ["interval"] False "cstring" True,
>         CatCreateFunction "interval_pl" ["interval", "interval"] False "interval" True,
>         CatCreateFunction "interval_pl_date" ["interval", "date"] False "timestamp" True,
>         CatCreateFunction "interval_pl_time" ["interval", "time"] False "time" True,
>         CatCreateFunction "interval_pl_timestamp" ["interval", "timestamp"]
>           False
>           "timestamp"
>           True,
>         CatCreateFunction "interval_pl_timestamptz"
>           ["interval", "timestamptz"]
>           False
>           "timestamptz"
>           True,
>         CatCreateFunction "interval_pl_timetz" ["timetz", "interval"] False
>           "timetz"
>           True,
>         CatCreateFunction "interval_recv" ["int4", "internal", "oid"] False
>           "interval"
>           True,
>         CatCreateFunction "interval_send" ["interval"] False "bytea" True,
>         CatCreateFunction "interval_smaller" ["interval", "interval"] False
>           "interval"
>           True,
>         CatCreateFunction "interval_um" ["interval"] False "interval" True,
>         CatCreateFunction "intervaltypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "intervaltypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "intinterval" ["abstime", "tinterval"] False
>           "bool"
>           True,
>         CatCreateFunction "isclosed" ["path"] False "bool" True,
>         CatCreateFunction "isfinite" ["abstime"] False "bool" True,
>         CatCreateFunction "isfinite" ["date"] False "bool" True,
>         CatCreateFunction "isfinite" ["timestamptz"] False "bool" True,
>         CatCreateFunction "isfinite" ["interval"] False "bool" True,
>         CatCreateFunction "isfinite" ["timestamp"] False "bool" True,
>         CatCreateFunction "ishorizontal" ["point", "point"] False "bool" True,
>         CatCreateFunction "ishorizontal" ["lseg"] False "bool" True,
>         CatCreateFunction "ishorizontal" ["line"] False "bool" True,
>         CatCreateFunction "iso8859_1_to_utf8"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "iso8859_to_utf8"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "iso_to_koi8r"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "iso_to_mic"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "iso_to_win1251"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "iso_to_win866"
>           ["int4", "cstring", "internal", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "isopen" ["path"] False "bool" True,
>         CatCreateFunction "isparallel" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "isparallel" ["line", "line"] False "bool" True,
>         CatCreateFunction "isperp" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "isperp" ["line", "line"] False "bool" True,
>         CatCreateFunction "isvertical" ["point", "point"] False "bool" True,
>         CatCreateFunction "isvertical" ["lseg"] False "bool" True,
>         CatCreateFunction "isvertical" ["line"] False "bool" True,
>         CatCreateFunction "johab_to_utf8"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "justify_days" ["interval"] False "interval" True,
>         CatCreateFunction "justify_hours" ["interval"] False "interval" True,
>         CatCreateFunction "justify_interval" ["interval"] False "interval" True,
>         CatCreateFunction "koi8r_to_iso"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "koi8r_to_mic"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "koi8r_to_utf8"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "koi8r_to_win1251"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "koi8r_to_win866"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "koi8u_to_utf8"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "language_handler_in" ["cstring"] False "language_handler" True,
>         CatCreateFunction "language_handler_out" ["language_handler"] False "cstring" True,
>         CatCreateFunction "latin1_to_mic"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "latin2_to_mic"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "latin2_to_win1250"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void"
>           True,
>         CatCreateFunction "latin3_to_mic"
>           ["int4", "internal", "cstring", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "latin4_to_mic"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void"
>           True,
>         CatCreateFunction "left" ["text", "int4"] False "text" True,
>         CatCreateFunction "length" ["text"] False "int4" True,
>         CatCreateFunction "length" ["bpchar"] False "int4" True,
>         CatCreateFunction "length" ["lseg"] False "float8" True,
>         CatCreateFunction "length" ["path"] False "float8" True,
>         CatCreateFunction "length" ["bit"] False "int4" True,
>         CatCreateFunction "length" ["bytea", "name"] False "int4" True,
>         CatCreateFunction "length" ["bytea"] False "int4" True,
>         CatCreateFunction "length" ["tsvector"] False "int4" True,
>         CatCreateFunction "like" ["text", "text"] False "bool" True,
>         CatCreateFunction "like" ["name", "text"] False "bool" True,
>         CatCreateFunction "like" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "like_escape" ["text", "text"] False "text" True,
>         CatCreateFunction "like_escape" ["bytea", "bytea"] False "bytea" True,
>         CatCreateFunction "likejoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "likesel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8" True,
>         CatCreateFunction "line" ["point", "point"] False "line" True,
>         CatCreateFunction "line_distance" ["line", "line"] False "float8" True,
>         CatCreateFunction "line_eq" ["line", "line"] False "bool" True,
>         CatCreateFunction "line_horizontal" ["line"] False "bool" True,
>         CatCreateFunction "line_in" ["cstring"] False "line" True,
>         CatCreateFunction "line_interpt" ["line", "line"] False "point" True,
>         CatCreateFunction "line_intersect" ["line", "line"] False "bool" True,
>         CatCreateFunction "line_out" ["line"] False "cstring" True,
>         CatCreateFunction "line_parallel" ["line", "line"] False "bool" True,
>         CatCreateFunction "line_perp" ["line", "line"] False "bool" True,
>         CatCreateFunction "line_recv" ["internal"] False "line" True,
>         CatCreateFunction "line_send" ["line"] False "bytea" True,
>         CatCreateFunction "line_vertical" ["line"] False "bool" True,
>         CatCreateFunction "ln" ["float8"] False "float8" True,
>         CatCreateFunction "ln" ["numeric"] False "numeric" True,
>         CatCreateFunction "lo_close" ["int4"] False "int4" True,
>         CatCreateFunction "lo_creat" ["int4"] False "oid" True,
>         CatCreateFunction "lo_create" ["oid"] False "oid" True,
>         CatCreateFunction "lo_export" ["oid", "text"] False "int4" True,
>         CatCreateFunction "lo_import" ["text"] False "oid" True,
>         CatCreateFunction "lo_import" ["text", "oid"] False "oid" True,
>         CatCreateFunction "lo_lseek" ["int4", "int4", "int4"] False "int4" True,
>         CatCreateFunction "lo_open" ["oid", "int4"] False "int4" True,
>         CatCreateFunction "lo_tell" ["int4"] False "int4" True,
>         CatCreateFunction "lo_truncate" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "lo_unlink" ["oid"] False "int4" True,
>         CatCreateFunction "log" ["float8"] False "float8" True,
>         CatCreateFunction "log" ["numeric", "numeric"] False "numeric" True,
>         CatCreateFunction "log" ["numeric"] False "numeric" True,
>         CatCreateFunction "loread" ["int4", "int4"] False "bytea" True,
>         CatCreateFunction "lower" ["text"] False "text" True,
>         CatCreateFunction "lowrite" ["int4", "bytea"] False "int4" True,
>         CatCreateFunction "lpad" ["text", "int4", "text"] False "text" True,
>         CatCreateFunction "lpad" ["text", "int4"] False "text" True,
>         CatCreateFunction "lseg" ["point", "point"] False "lseg" True,
>         CatCreateFunction "lseg" ["box"] False "lseg" True,
>         CatCreateFunction "lseg_center" ["lseg"] False "point" True,
>         CatCreateFunction "lseg_distance" ["lseg", "lseg"] False "float8" True,
>         CatCreateFunction "lseg_eq" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_ge" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_gt" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_horizontal" ["lseg"] False "bool" True,
>         CatCreateFunction "lseg_in" ["cstring"] False "lseg" True,
>         CatCreateFunction "lseg_interpt" ["lseg", "lseg"] False "point" True,
>         CatCreateFunction "lseg_intersect" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_le" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_length" ["lseg"] False "float8" True,
>         CatCreateFunction "lseg_lt" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_ne" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_out" ["lseg"] False "cstring" True,
>         CatCreateFunction "lseg_parallel" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_perp" ["lseg", "lseg"] False "bool" True,
>         CatCreateFunction "lseg_recv" ["internal"] False "lseg" True,
>         CatCreateFunction "lseg_send" ["lseg"] False "bytea" True,
>         CatCreateFunction "lseg_vertical" ["lseg"] False "bool" True,
>         CatCreateFunction "ltrim" ["text", "text"] False "text" True,
>         CatCreateFunction "ltrim" ["text"] False "text" True,
>         CatCreateFunction "macaddr_cmp" ["macaddr", "macaddr"] False "int4" True,
>         CatCreateFunction "macaddr_eq" ["macaddr", "macaddr"] False "bool" True,
>         CatCreateFunction "macaddr_ge" ["macaddr", "macaddr"] False "bool" True,
>         CatCreateFunction "macaddr_gt" ["macaddr", "macaddr"] False "bool" True,
>         CatCreateFunction "macaddr_in" ["cstring"] False "macaddr" True,
>         CatCreateFunction "macaddr_le" ["macaddr", "macaddr"] False "bool" True,
>         CatCreateFunction "macaddr_lt" ["macaddr", "macaddr"] False "bool" True,
>         CatCreateFunction "macaddr_ne" ["macaddr", "macaddr"] False "bool" True,
>         CatCreateFunction "macaddr_out" ["macaddr"] False "cstring" True,
>         CatCreateFunction "macaddr_recv" ["internal"] False "macaddr" True,
>         CatCreateFunction "macaddr_send" ["macaddr"] False "bytea" True,
>         CatCreateFunction "makeaclitem" ["oid", "oid", "text", "bool"]
>           False
>           "aclitem" True,
>         CatCreateFunction "masklen" ["inet"] False "int4" True,
>         CatCreateFunction "md5" ["text"] False "text" True,
>         CatCreateFunction "md5" ["bytea"] False "text" True,
>         CatCreateFunction "mic_to_ascii"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_big5"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_euc_cn"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_euc_jp"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_euc_kr"
>           ["int4", "internal", "cstring", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_euc_tw"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_iso"
>           ["int4", "internal", "int4", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_koi8r"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_latin1"
>           ["int4", "internal", "int4", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_latin2"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_latin3"
>           ["cstring", "internal", "int4", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_latin4"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_sjis"
>           ["internal", "int4", "cstring", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_win1250"
>           ["int4", "int4", "int4", "internal", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_win1251"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "mic_to_win866"
>           ["int4", "int4", "cstring", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "mktinterval" ["abstime", "abstime"] False
>           "tinterval" True,
>         CatCreateFunction "mod" ["int2", "int2"] False "int2" True,
>         CatCreateFunction "mod" ["int4", "int4"] False "int4" True,
>         CatCreateFunction "mod" ["int8", "int8"] False "int8" True,
>         CatCreateFunction "mod" ["numeric", "numeric"] False "numeric" True,
>         CatCreateFunction "money" ["int4"] False "money" True,
>         CatCreateFunction "money" ["int8"] False "money" True,
>         CatCreateFunction "money" ["numeric"] False "money" True,
>         CatCreateFunction "mul_d_interval" ["float8", "interval"] False
>           "interval" True,
>         CatCreateFunction "name" ["text"] False "name" True,
>         CatCreateFunction "name" ["bpchar"] False "name" True,
>         CatCreateFunction "name" ["varchar"] False "name" True,
>         CatCreateFunction "nameeq" ["name", "name"] False "bool" True,
>         CatCreateFunction "namege" ["name", "name"] False "bool" True,
>         CatCreateFunction "namegt" ["name", "name"] False "bool" True,
>         CatCreateFunction "nameiclike" ["name", "text"] False "bool" True,
>         CatCreateFunction "nameicnlike" ["name", "text"] False "bool" True,
>         CatCreateFunction "nameicregexeq" ["name", "text"] False "bool" True,
>         CatCreateFunction "nameicregexne" ["name", "text"] False "bool" True,
>         CatCreateFunction "namein" ["cstring"] False "name" True,
>         CatCreateFunction "namele" ["name", "name"] False "bool" True,
>         CatCreateFunction "namelike" ["name", "text"] False "bool" True,
>         CatCreateFunction "namelt" ["name", "name"] False "bool" True,
>         CatCreateFunction "namene" ["name", "name"] False "bool" True,
>         CatCreateFunction "namenlike" ["name", "text"] False "bool" True,
>         CatCreateFunction "nameout" ["name"] False "cstring" True,
>         CatCreateFunction "namerecv" ["internal"] False "name" True,
>         CatCreateFunction "nameregexeq" ["text", "name"] False "bool" True,
>         CatCreateFunction "nameregexne" ["name", "text"] False "bool" True,
>         CatCreateFunction "namesend" ["name"] False "bytea" True,
>         CatCreateFunction "neqjoinsel"
>           ["internal", "int2", "internal", "internal", "oid"]
>           False
>           "float8" True,
>         CatCreateFunction "neqsel" ["internal", "oid", "internal", "int4"]
>           False
>           "float8" True,
>         CatCreateFunction "netmask" ["inet"] False "inet" True,
>         CatCreateFunction "network" ["inet"] False "cidr" True,
>         CatCreateFunction "network_cmp" ["inet", "inet"] False "int4" True,
>         CatCreateFunction "network_eq" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_ge" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_gt" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_le" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_lt" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_ne" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_sub" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_subeq" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_sup" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "network_supeq" ["inet", "inet"] False "bool" True,
>         CatCreateFunction "nextval" ["regclass"] False "int8" True,
>         CatCreateFunction "nlikejoinsel"
>           ["internal", "int2", "internal", "internal", "oid"]
>           False
>           "float8" True,
>         CatCreateFunction "nlikesel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8" True,
>         CatCreateFunction "notlike" ["text", "text"] False "bool" True,
>         CatCreateFunction "notlike" ["name", "text"] False "bool" True,
>         CatCreateFunction "notlike" ["bytea", "bytea"] False "bool" True,
>         CatCreateFunction "npoints" ["path"] False "int4" True,
>         CatCreateFunction "npoints" ["polygon"] False "int4" True,
>         CatCreateFunction "numeric" ["numeric", "int4"] False "numeric" True,
>         CatCreateFunction "numeric" ["int4"] False "numeric" True,
>         CatCreateFunction "numeric" ["float4"] False "numeric" True,
>         CatCreateFunction "numeric" ["float8"] False "numeric" True,
>         CatCreateFunction "numeric" ["int8"] False "numeric" True,
>         CatCreateFunction "numeric" ["int2"] False "numeric" True,
>         CatCreateFunction "numeric" ["money"] False "numeric" True,
>         CatCreateFunction "numeric_abs" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_accum" ["_numeric", "numeric"] False
>           "_numeric" True,
>         CatCreateFunction "numeric_add" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_avg" ["_numeric"] False "numeric" True,
>         CatCreateFunction "numeric_avg_accum" ["numeric", "_numeric"] False
>           "_numeric" True,
>         CatCreateFunction "numeric_cmp" ["numeric", "numeric"] False
>           "int4" True,
>         CatCreateFunction "numeric_div" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_div_trunc" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_eq" ["numeric", "numeric"] False "bool" True,
>         CatCreateFunction "numeric_exp" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_fac" ["int8"] False "numeric" True,
>         CatCreateFunction "numeric_ge" ["numeric", "numeric"] False "bool" True,
>         CatCreateFunction "numeric_gt" ["numeric", "numeric"] False "bool" True,
>         CatCreateFunction "numeric_in" ["int4", "cstring", "oid"] False
>           "numeric" True,
>         CatCreateFunction "numeric_inc" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_larger" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_le" ["numeric", "numeric"] False "bool" True,
>         CatCreateFunction "numeric_ln" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_log" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_lt" ["numeric", "numeric"] False "bool" True,
>         CatCreateFunction "numeric_mod" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_mul" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_ne" ["numeric", "numeric"] False "bool" True,
>         CatCreateFunction "numeric_out" ["numeric"] False "cstring" True,
>         CatCreateFunction "numeric_power" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_recv" ["internal", "oid", "int4"] False
>           "numeric" True,
>         CatCreateFunction "numeric_send" ["numeric"] False "bytea" True,
>         CatCreateFunction "numeric_smaller" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_sqrt" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_stddev_pop" ["_numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_stddev_samp" ["_numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_sub" ["numeric", "numeric"] False
>           "numeric" True,
>         CatCreateFunction "numeric_uminus" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_uplus" ["numeric"] False "numeric" True,
>         CatCreateFunction "numeric_var_pop" ["_numeric"] False "numeric" True,
>         CatCreateFunction "numeric_var_samp" ["_numeric"] False "numeric" True,
>         CatCreateFunction "numerictypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "numerictypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "numnode" ["tsquery"] False "int4" True,
>         CatCreateFunction "obj_description" ["oid", "name"] False "text" True,
>         CatCreateFunction "obj_description" ["oid"] False "text" True,
>         CatCreateFunction "octet_length" ["bytea"] False "int4" True,
>         CatCreateFunction "octet_length" ["text"] False "int4" True,
>         CatCreateFunction "octet_length" ["bpchar"] False "int4" True,
>         CatCreateFunction "octet_length" ["bit"] False "int4" True,
>         CatCreateFunction "oid" ["int8"] False "oid" True,
>         CatCreateFunction "oideq" ["oid", "oid"] False "bool" True,
>         CatCreateFunction "oidge" ["oid", "oid"] False "bool" True,
>         CatCreateFunction "oidgt" ["oid", "oid"] False "bool" True,
>         CatCreateFunction "oidin" ["cstring"] False "oid" True,
>         CatCreateFunction "oidlarger" ["oid", "oid"] False "oid" True,
>         CatCreateFunction "oidle" ["oid", "oid"] False "bool" True,
>         CatCreateFunction "oidlt" ["oid", "oid"] False "bool" True,
>         CatCreateFunction "oidne" ["oid", "oid"] False "bool" True,
>         CatCreateFunction "oidout" ["oid"] False "cstring" True,
>         CatCreateFunction "oidrecv" ["internal"] False "oid" True,
>         CatCreateFunction "oidsend" ["oid"] False "bytea" True,
>         CatCreateFunction "oidsmaller" ["oid", "oid"] False "oid" True,
>         CatCreateFunction "oidvectoreq" ["oidvector", "oidvector"] False
>           "bool" True,
>         CatCreateFunction "oidvectorge" ["oidvector", "oidvector"] False
>           "bool" True,
>         CatCreateFunction "oidvectorgt" ["oidvector", "oidvector"] False
>           "bool" True,
>         CatCreateFunction "oidvectorin" ["cstring"] False "oidvector" True,
>         CatCreateFunction "oidvectorle" ["oidvector", "oidvector"] False
>           "bool" True,
>         CatCreateFunction "oidvectorlt" ["oidvector", "oidvector"] False
>           "bool" True,
>         CatCreateFunction "oidvectorne" ["oidvector", "oidvector"] False
>           "bool" True,
>         CatCreateFunction "oidvectorout" ["oidvector"] False "cstring" True,
>         CatCreateFunction "oidvectorrecv" ["internal"] False "oidvector" True,
>         CatCreateFunction "oidvectorsend" ["oidvector"] False "bytea" True,
>         CatCreateFunction "oidvectortypes" ["oidvector"] False "text" True,
>         CatCreateFunction "on_pb" ["point", "box"] False "bool" True,
>         CatCreateFunction "on_pl" ["line", "point"] False "bool" True,
>         CatCreateFunction "on_ppath" ["point", "path"] False "bool" True,
>         CatCreateFunction "on_ps" ["point", "lseg"] False "bool" True,
>         CatCreateFunction "on_sb" ["box", "lseg"] False "bool" True,
>         CatCreateFunction "on_sl" ["line", "lseg"] False "bool" True,
>         CatCreateFunction "opaque_in" ["cstring"] False "opaque" True,
>         CatCreateFunction "opaque_out" ["opaque"] False "cstring" True,
>         CatCreateFunction "overlaps"
>           ["timetz", "timetz", "timetz", "timetz"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamptz", "timestamptz", "timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamptz", "interval", "timestamptz", "interval"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamptz", "timestamptz", "timestamptz", "interval"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamptz", "interval", "timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps" ["time", "time", "time", "time"] False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["time", "time", "interval", "interval"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps" ["time", "time", "time", "interval"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps" ["time", "time", "time", "interval"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamp", "timestamp", "timestamp", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamp", "interval", "interval", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamp", "timestamp", "interval", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "overlaps"
>           ["timestamp", "timestamp", "interval", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "overlay" ["bytea", "int4", "bytea", "int4"]
>           False
>           "bytea" True,
>         CatCreateFunction "overlay" ["bytea", "bytea", "int4"] False
>           "bytea" True,
>         CatCreateFunction "overlay" ["text", "text", "int4", "int4"] False
>           "text" True,
>         CatCreateFunction "overlay" ["text", "int4", "text"] False "text" True,
>         CatCreateFunction "overlay" ["bit", "bit", "int4", "int4"] False
>           "bit" True,
>         CatCreateFunction "overlay" ["bit", "bit", "int4"] False "bit" True,
>         CatCreateFunction "path" ["polygon"] False "path" True,
>         CatCreateFunction "path_add" ["path", "path"] False "path" True,
>         CatCreateFunction "path_add_pt" ["path", "point"] False "path" True,
>         CatCreateFunction "path_center" ["path"] False "point" True,
>         CatCreateFunction "path_contain_pt" ["path", "point"] False "bool" True,
>         CatCreateFunction "path_distance" ["path", "path"] False "float8" True,
>         CatCreateFunction "path_div_pt" ["point", "path"] False "path" True,
>         CatCreateFunction "path_in" ["cstring"] False "path" True,
>         CatCreateFunction "path_inter" ["path", "path"] False "bool" True,
>         CatCreateFunction "path_length" ["path"] False "float8" True,
>         CatCreateFunction "path_mul_pt" ["path", "point"] False "path" True,
>         CatCreateFunction "path_n_eq" ["path", "path"] False "bool" True,
>         CatCreateFunction "path_n_ge" ["path", "path"] False "bool" True,
>         CatCreateFunction "path_n_gt" ["path", "path"] False "bool" True,
>         CatCreateFunction "path_n_le" ["path", "path"] False "bool" True,
>         CatCreateFunction "path_n_lt" ["path", "path"] False "bool" True,
>         CatCreateFunction "path_npoints" ["path"] False "int4" True,
>         CatCreateFunction "path_out" ["path"] False "cstring" True,
>         CatCreateFunction "path_recv" ["internal"] False "path" True,
>         CatCreateFunction "path_send" ["path"] False "bytea" True,
>         CatCreateFunction "path_sub_pt" ["path", "point"] False "path" True,
>         CatCreateFunction "pclose" ["path"] False "path" True,
>         CatCreateFunction "pg_advisory_lock" ["int8"] False "void" True,
>         CatCreateFunction "pg_advisory_lock" ["int4", "int4"] False "void" True,
>         CatCreateFunction "pg_advisory_lock_shared" ["int8"] False "void" True,
>         CatCreateFunction "pg_advisory_lock_shared" ["int4", "int4"] False
>           "void" True,
>         CatCreateFunction "pg_advisory_unlock" ["int8"] False "bool" True,
>         CatCreateFunction "pg_advisory_unlock" ["int4", "int4"] False
>           "bool" True,
>         CatCreateFunction "pg_advisory_unlock_shared" ["int8"] False
>           "bool" True,
>         CatCreateFunction "pg_advisory_unlock_shared" ["int4", "int4"]
>           False
>           "bool" True,
>         CatCreateFunction "pg_advisory_xact_lock" ["int8"] False "void" True,
>         CatCreateFunction "pg_advisory_xact_lock" ["int4", "int4"] False
>           "void" True,
>         CatCreateFunction "pg_advisory_xact_lock_shared" ["int8"] False
>           "void" True,
>         CatCreateFunction "pg_advisory_xact_lock_shared" ["int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "pg_cancel_backend" ["int4"] False "bool" True,
>         CatCreateFunction "pg_char_to_encoding" ["name"] False "int4" True,
>         CatCreateFunction "pg_collation_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_column_size" ["any"] False "int4" True,
>         CatCreateFunction "pg_conversion_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_create_restore_point" ["text"] False "text" True,
>         CatCreateFunction "pg_database_size" ["name"] False "int8" True,
>         CatCreateFunction "pg_database_size" ["oid"] False "int8" True,
>         CatCreateFunction "pg_describe_object" ["oid", "oid", "int4"] False
>           "text" True,
>         CatCreateFunction "pg_encoding_max_length" ["int4"] False "int4" True,
>         CatCreateFunction "pg_encoding_to_char" ["int4"] False "name" True,
>         CatCreateFunction "pg_extension_config_dump" ["regclass", "text"]
>           False
>           "void" True,
>         CatCreateFunction "pg_extension_update_paths" ["name"] True
>           "record" True,
>         CatCreateFunction "pg_function_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_get_constraintdef" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_constraintdef" ["oid", "bool"] False
>           "text" True,
>         CatCreateFunction "pg_get_expr" ["pg_node_tree", "oid"] False
>           "text" True,
>         CatCreateFunction "pg_get_expr" ["pg_node_tree", "oid", "bool"]
>           False
>           "text" True,
>         CatCreateFunction "pg_get_function_arguments" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_function_identity_arguments" ["oid"]
>           False
>           "text" True,
>         CatCreateFunction "pg_get_function_result" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_functiondef" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_indexdef" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_indexdef" ["oid", "bool", "int4"] False
>           "text" True,
>         CatCreateFunction "pg_get_ruledef" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_ruledef" ["oid", "bool"] False "text" True,
>         CatCreateFunction "pg_get_serial_sequence" ["text", "text"] False
>           "text" True,
>         CatCreateFunction "pg_get_triggerdef" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_triggerdef" ["bool", "oid"] False "text" True,
>         CatCreateFunction "pg_get_userbyid" ["oid"] False "name" True,
>         CatCreateFunction "pg_get_viewdef" ["text"] False "text" True,
>         CatCreateFunction "pg_get_viewdef" ["oid"] False "text" True,
>         CatCreateFunction "pg_get_viewdef" ["text", "bool"] False "text" True,
>         CatCreateFunction "pg_get_viewdef" ["bool", "oid"] False "text" True,
>         CatCreateFunction "pg_has_role" ["name", "name", "text"] False
>           "bool" True,
>         CatCreateFunction "pg_has_role" ["name", "oid", "text"] False
>           "bool" True,
>         CatCreateFunction "pg_has_role" ["oid", "name", "text"] False
>           "bool" True,
>         CatCreateFunction "pg_has_role" ["oid", "oid", "text"] False
>           "bool" True,
>         CatCreateFunction "pg_has_role" ["name", "text"] False "bool" True,
>         CatCreateFunction "pg_has_role" ["oid", "text"] False "bool" True,
>         CatCreateFunction "pg_indexes_size" ["regclass"] False "int8" True,
>         CatCreateFunction "pg_is_other_temp_schema" ["oid"] False "bool" True,
>         CatCreateFunction "pg_ls_dir" ["text"] True "text" True,
>         CatCreateFunction "pg_node_tree_in" ["cstring"] False
>           "pg_node_tree" True,
>         CatCreateFunction "pg_node_tree_out" ["pg_node_tree"] False
>           "cstring" True,
>         CatCreateFunction "pg_node_tree_recv" ["internal"] False
>           "pg_node_tree" True,
>         CatCreateFunction "pg_node_tree_send" ["pg_node_tree"] False
>           "bytea" True,
>         CatCreateFunction "pg_notify" ["text", "text"] False "void" True,
>         CatCreateFunction "pg_opclass_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_operator_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_options_to_table" ["_text"] True "record" True,
>         CatCreateFunction "pg_read_binary_file" ["int8", "text", "int8"]
>           False
>           "bytea" True,
>         CatCreateFunction "pg_read_binary_file" ["text"] False "bytea" True,
>         CatCreateFunction "pg_read_file" ["text", "int8", "int8"] False
>           "text" True,
>         CatCreateFunction "pg_read_file" ["text"] False "text" True,
>         CatCreateFunction "pg_relation_filenode" ["regclass"] False "oid" True,
>         CatCreateFunction "pg_relation_filepath" ["regclass"] False "text" True,
>         CatCreateFunction "pg_relation_size" ["regclass"] False "int8" True,
>         CatCreateFunction "pg_relation_size" ["regclass", "text"] False
>           "int8" True,
>         CatCreateFunction "pg_sequence_parameters" ["oid"] False "record" True,
>         CatCreateFunction "pg_size_pretty" ["int8"] False "text" True,
>         CatCreateFunction "pg_sleep" ["float8"] False "void" True,
>         CatCreateFunction "pg_start_backup" ["bool", "text"] False "text" True,
>         CatCreateFunction "pg_stat_file" ["text"] False "record" True,
>         CatCreateFunction "pg_stat_get_activity" ["int4"] True "record" True,
>         CatCreateFunction "pg_stat_get_analyze_count" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_autoanalyze_count" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_autovacuum_count" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_backend_activity" ["int4"] False
>           "text" True,
>         CatCreateFunction "pg_stat_get_backend_activity_start" ["int4"]
>           False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_backend_client_addr" ["int4"] False
>           "inet" True,
>         CatCreateFunction "pg_stat_get_backend_client_port" ["int4"] False
>           "int4" True,
>         CatCreateFunction "pg_stat_get_backend_dbid" ["int4"] False "oid" True,
>         CatCreateFunction "pg_stat_get_backend_pid" ["int4"] False "int4" True,
>         CatCreateFunction "pg_stat_get_backend_start" ["int4"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_backend_userid" ["int4"] False
>           "oid" True,
>         CatCreateFunction "pg_stat_get_backend_waiting" ["int4"] False
>           "bool" True,
>         CatCreateFunction "pg_stat_get_backend_xact_start" ["int4"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_blocks_fetched" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_blocks_hit" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_db_blocks_fetched" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_blocks_hit" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_db_conflict_all" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_conflict_bufferpin" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_conflict_lock" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_conflict_snapshot" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_conflict_startup_deadlock"
>           ["oid"]
>           False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_conflict_tablespace" ["oid"]
>           False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_numbackends" ["oid"] False
>           "int4" True,
>         CatCreateFunction "pg_stat_get_db_stat_reset_time" ["oid"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_db_tuples_deleted" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_tuples_fetched" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_tuples_inserted" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_tuples_returned" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_tuples_updated" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_xact_commit" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_db_xact_rollback" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_dead_tuples" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_function_calls" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_function_self_time" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_function_time" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_last_analyze_time" ["oid"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_last_autoanalyze_time" ["oid"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_last_autovacuum_time" ["oid"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_last_vacuum_time" ["oid"] False
>           "timestamptz" True,
>         CatCreateFunction "pg_stat_get_live_tuples" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_numscans" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_tuples_deleted" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_tuples_fetched" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_tuples_hot_updated" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_tuples_inserted" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_tuples_returned" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_tuples_updated" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_vacuum_count" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_xact_blocks_fetched" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_blocks_hit" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_function_calls" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_function_self_time" ["oid"]
>           False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_function_time" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_numscans" ["oid"] False "int8" True,
>         CatCreateFunction "pg_stat_get_xact_tuples_deleted" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_tuples_fetched" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_tuples_hot_updated" ["oid"]
>           False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_tuples_inserted" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_tuples_returned" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_get_xact_tuples_updated" ["oid"] False
>           "int8" True,
>         CatCreateFunction "pg_stat_reset_shared" ["text"] False "void" True,
>         CatCreateFunction "pg_stat_reset_single_function_counters" ["oid"]
>           False
>           "void" True,
>         CatCreateFunction "pg_stat_reset_single_table_counters" ["oid"]
>           False
>           "void" True,
>         CatCreateFunction "pg_table_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_table_size" ["regclass"] False "int8" True,
>         CatCreateFunction "pg_tablespace_databases" ["oid"] True "oid" True,
>         CatCreateFunction "pg_tablespace_size" ["oid"] False "int8" True,
>         CatCreateFunction "pg_tablespace_size" ["name"] False "int8" True,
>         CatCreateFunction "pg_terminate_backend" ["int4"] False "bool" True,
>         CatCreateFunction "pg_total_relation_size" ["regclass"] False
>           "int8" True,
>         CatCreateFunction "pg_try_advisory_lock" ["int8"] False "bool" True,
>         CatCreateFunction "pg_try_advisory_lock" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "pg_try_advisory_lock_shared" ["int8"] False "bool" True,
>         CatCreateFunction "pg_try_advisory_lock_shared" ["int4", "int4"] False "bool" True,
>         CatCreateFunction "pg_try_advisory_xact_lock" ["int8"] False
>           "bool" True,
>         CatCreateFunction "pg_try_advisory_xact_lock" ["int4", "int4"]
>           False
>           "bool" True,
>         CatCreateFunction "pg_try_advisory_xact_lock_shared" ["int8"] False
>           "bool" True,
>         CatCreateFunction "pg_try_advisory_xact_lock_shared"
>           ["int4", "int4"]
>           False
>           "bool" True,
>         CatCreateFunction "pg_ts_config_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_ts_dict_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_ts_parser_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_ts_template_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_type_is_visible" ["oid"] False "bool" True,
>         CatCreateFunction "pg_typeof" ["any"] False "regtype" True,
>         CatCreateFunction "pg_xlogfile_name" ["text"] False "text" True,
>         CatCreateFunction "pg_xlogfile_name_offset" ["text"] False
>           "record" True,
>         CatCreateFunction "plainto_tsquery" ["regconfig", "text"] False
>           "tsquery" True,
>         CatCreateFunction "plainto_tsquery" ["text"] False "tsquery" True,
>         CatCreateFunction "plpgsql_inline_handler" ["internal"] False
>           "void" True,
>         CatCreateFunction "plpgsql_validator" ["oid"] False "void" True,
>         CatCreateFunction "point" ["circle"] False "point" True,
>         CatCreateFunction "point" ["float8", "float8"] False "point" True,
>         CatCreateFunction "point" ["lseg"] False "point" True,
>         CatCreateFunction "point" ["path"] False "point" True,
>         CatCreateFunction "point" ["box"] False "point" True,
>         CatCreateFunction "point" ["polygon"] False "point" True,
>         CatCreateFunction "point_above" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_add" ["point", "point"] False "point" True,
>         CatCreateFunction "point_below" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_distance" ["point", "point"] False
>           "float8" True,
>         CatCreateFunction "point_div" ["point", "point"] False "point" True,
>         CatCreateFunction "point_eq" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_horiz" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_in" ["cstring"] False "point" True,
>         CatCreateFunction "point_left" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_mul" ["point", "point"] False "point" True,
>         CatCreateFunction "point_ne" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_out" ["point"] False "cstring" True,
>         CatCreateFunction "point_recv" ["internal"] False "point" True,
>         CatCreateFunction "point_right" ["point", "point"] False "bool" True,
>         CatCreateFunction "point_send" ["point"] False "bytea" True,
>         CatCreateFunction "point_sub" ["point", "point"] False "point" True,
>         CatCreateFunction "point_vert" ["point", "point"] False "bool" True,
>         CatCreateFunction "poly_above" ["polygon", "polygon"] False "bool" True,
>         CatCreateFunction "poly_below" ["polygon", "polygon"] False "bool" True,
>         CatCreateFunction "poly_center" ["polygon"] False "point" True,
>         CatCreateFunction "poly_contain" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_contain_pt" ["polygon", "point"] False
>           "bool" True,
>         CatCreateFunction "poly_contained" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_distance" ["polygon", "polygon"] False
>           "float8" True,
>         CatCreateFunction "poly_in" ["cstring"] False "polygon" True,
>         CatCreateFunction "poly_left" ["polygon", "polygon"] False "bool" True,
>         CatCreateFunction "poly_npoints" ["polygon"] False "int4" True,
>         CatCreateFunction "poly_out" ["polygon"] False "cstring" True,
>         CatCreateFunction "poly_overabove" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_overbelow" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_overlap" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_overleft" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_overright" ["polygon", "polygon"] False
>           "bool" True,
>         CatCreateFunction "poly_recv" ["internal"] False "polygon" True,
>         CatCreateFunction "poly_right" ["polygon", "polygon"] False "bool" True,
>         CatCreateFunction "poly_same" ["polygon", "polygon"] False "bool" True,
>         CatCreateFunction "poly_send" ["polygon"] False "bytea" True,
>         CatCreateFunction "polygon" ["box"] False "polygon" True,
>         CatCreateFunction "polygon" ["path"] False "polygon" True,
>         CatCreateFunction "polygon" ["circle", "int4"] False "polygon" True,
>         CatCreateFunction "polygon" ["circle"] False "polygon" True,
>         CatCreateFunction "popen" ["path"] False "path" True,
>         CatCreateFunction "position" ["text", "text"] False "int4" True,
>         CatCreateFunction "position" ["bit", "bit"] False "int4" True,
>         CatCreateFunction "position" ["bytea", "bytea"] False "int4" True,
>         CatCreateFunction "positionjoinsel"
>           ["internal", "oid", "internal", "int2", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "positionsel"
>           ["internal", "oid", "internal", "int4"]
>           False
>           "float8" True,
>         CatCreateFunction "postgresql_fdw_validator" ["_text", "oid"] False
>           "bool" True,
>         CatCreateFunction "pow" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "pow" ["numeric", "numeric"] False "numeric" True,
>         CatCreateFunction "power" ["float8", "float8"] False "float8" True,
>         CatCreateFunction "power" ["numeric", "numeric"] False "numeric" True,
>         CatCreateFunction "prsd_end" ["internal"] False "void" True,
>         CatCreateFunction "prsd_headline"
>           ["internal", "internal", "tsquery"]
>           False
>           "internal" True,
>         CatCreateFunction "prsd_lextype" ["internal"] False "internal" True,
>         CatCreateFunction "prsd_nexttoken"
>           ["internal", "internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "prsd_start" ["internal", "int4"] False
>           "internal" True,
>         CatCreateFunction "pt_contained_circle" ["circle", "point"] False
>           "bool" True,
>         CatCreateFunction "pt_contained_poly" ["point", "polygon"] False
>           "bool" True,
>         CatCreateFunction "query_to_xml" ["text", "text", "bool", "bool"]
>           False
>           "xml" True,
>         CatCreateFunction "query_to_xml_and_xmlschema"
>           ["text", "bool", "bool", "text"]
>           False
>           "xml" True,
>         CatCreateFunction "query_to_xmlschema"
>           ["text", "text", "bool", "bool"]
>           False
>           "xml" True,
>         CatCreateFunction "querytree" ["tsquery"] False "text" True,
>         CatCreateFunction "quote_ident" ["text"] False "text" True,
>         CatCreateFunction "quote_literal" ["text"] False "text" True,
>         CatCreateFunction "quote_literal" ["anyelement"] False "text" True,
>         CatCreateFunction "quote_nullable" ["text"] False "text" True,
>         CatCreateFunction "quote_nullable" ["anyelement"] False "text" True,
>         CatCreateFunction "radians" ["float8"] False "float8" True,
>         CatCreateFunction "radius" ["circle"] False "float8" True,
>         CatCreateFunction "record_eq" ["record", "record"] False "bool" True,
>         CatCreateFunction "record_ge" ["record", "record"] False "bool" True,
>         CatCreateFunction "record_gt" ["record", "record"] False "bool" True,
>         CatCreateFunction "record_in" ["int4", "cstring", "oid"] False
>           "record" True,
>         CatCreateFunction "record_le" ["record", "record"] False "bool" True,
>         CatCreateFunction "record_lt" ["record", "record"] False "bool" True,
>         CatCreateFunction "record_ne" ["record", "record"] False "bool" True,
>         CatCreateFunction "record_out" ["record"] False "cstring" True,
>         CatCreateFunction "record_recv" ["oid", "int4", "internal"] False
>           "record" True,
>         CatCreateFunction "record_send" ["record"] False "bytea" True,
>         CatCreateFunction "regclass" ["text"] False "regclass" True,
>         CatCreateFunction "regclassin" ["cstring"] False "regclass" True,
>         CatCreateFunction "regclassout" ["regclass"] False "cstring" True,
>         CatCreateFunction "regclassrecv" ["internal"] False "regclass" True,
>         CatCreateFunction "regclasssend" ["regclass"] False "bytea" True,
>         CatCreateFunction "regconfigin" ["cstring"] False "regconfig" True,
>         CatCreateFunction "regconfigout" ["regconfig"] False "cstring" True,
>         CatCreateFunction "regconfigrecv" ["internal"] False "regconfig" True,
>         CatCreateFunction "regconfigsend" ["regconfig"] False "bytea" True,
>         CatCreateFunction "regdictionaryin" ["cstring"] False
>           "regdictionary" True,
>         CatCreateFunction "regdictionaryout" ["regdictionary"] False
>           "cstring" True,
>         CatCreateFunction "regdictionaryrecv" ["internal"] False
>           "regdictionary" True,
>         CatCreateFunction "regdictionarysend" ["regdictionary"] False
>           "bytea" True,
>         CatCreateFunction "regexeqjoinsel"
>           ["internal", "oid", "int2", "internal", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "regexeqsel"
>           ["internal", "int4", "oid", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "regexnejoinsel"
>           ["oid", "int2", "internal", "internal", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "regexnesel"
>           ["internal", "internal", "int4", "oid"]
>           False
>           "float8" True,
>         CatCreateFunction "regexp_matches" ["text", "text"] True "_text" True,
>         CatCreateFunction "regexp_matches" ["text", "text", "text"] True
>           "_text" True,
>         CatCreateFunction "regexp_replace" ["text", "text", "text"] False
>           "text" True,
>         CatCreateFunction "regexp_replace" ["text", "text", "text", "text"]
>           False
>           "text" True,
>         CatCreateFunction "regexp_split_to_array" ["text", "text"] False
>           "_text" True,
>         CatCreateFunction "regexp_split_to_array" ["text", "text", "text"]
>           False
>           "_text" True,
>         CatCreateFunction "regexp_split_to_table" ["text", "text"] True
>           "text" True,
>         CatCreateFunction "regexp_split_to_table" ["text", "text", "text"]
>           True
>           "text" True,
>         CatCreateFunction "regoperatorin" ["cstring"] False "regoperator" True,
>         CatCreateFunction "regoperatorout" ["regoperator"] False "cstring" True,
>         CatCreateFunction "regoperatorrecv" ["internal"] False
>           "regoperator" True,
>         CatCreateFunction "regoperatorsend" ["regoperator"] False "bytea" True,
>         CatCreateFunction "regoperin" ["cstring"] False "regoper" True,
>         CatCreateFunction "regoperout" ["regoper"] False "cstring" True,
>         CatCreateFunction "regoperrecv" ["internal"] False "regoper" True,
>         CatCreateFunction "regopersend" ["regoper"] False "bytea" True,
>         CatCreateFunction "regprocedurein" ["cstring"] False
>           "regprocedure" True,
>         CatCreateFunction "regprocedureout" ["regprocedure"] False
>           "cstring" True,
>         CatCreateFunction "regprocedurerecv" ["internal"] False
>           "regprocedure" True,
>         CatCreateFunction "regproceduresend" ["regprocedure"] False
>           "bytea" True,
>         CatCreateFunction "regprocin" ["cstring"] False "regproc" True,
>         CatCreateFunction "regprocout" ["regproc"] False "cstring" True,
>         CatCreateFunction "regprocrecv" ["internal"] False "regproc" True,
>         CatCreateFunction "regprocsend" ["regproc"] False "bytea" True,
>         CatCreateFunction "regtypein" ["cstring"] False "regtype" True,
>         CatCreateFunction "regtypeout" ["regtype"] False "cstring" True,
>         CatCreateFunction "regtyperecv" ["internal"] False "regtype" True,
>         CatCreateFunction "regtypesend" ["regtype"] False "bytea" True,
>         CatCreateFunction "reltime" ["interval"] False "reltime" True,
>         CatCreateFunction "reltimeeq" ["reltime", "reltime"] False "bool" True,
>         CatCreateFunction "reltimege" ["reltime", "reltime"] False "bool" True,
>         CatCreateFunction "reltimegt" ["reltime", "reltime"] False "bool" True,
>         CatCreateFunction "reltimein" ["cstring"] False "reltime" True,
>         CatCreateFunction "reltimele" ["reltime", "reltime"] False "bool" True,
>         CatCreateFunction "reltimelt" ["reltime", "reltime"] False "bool" True,
>         CatCreateFunction "reltimene" ["reltime", "reltime"] False "bool" True,
>         CatCreateFunction "reltimeout" ["reltime"] False "cstring" True,
>         CatCreateFunction "reltimerecv" ["internal"] False "reltime" True,
>         CatCreateFunction "reltimesend" ["reltime"] False "bytea" True,
>         CatCreateFunction "repeat" ["text", "int4"] False "text" True,
>         CatCreateFunction "replace" ["text", "text", "text"] False "text" True,
>         CatCreateFunction "reverse" ["text"] False "text" True,
>         CatCreateFunction "right" ["text", "int4"] False "text" True,
>         CatCreateFunction "round" ["float8"] False "float8" True,
>         CatCreateFunction "round" ["numeric", "int4"] False "numeric" True,
>         CatCreateFunction "round" ["numeric"] False "numeric" True,
>         CatCreateFunction "rpad" ["int4", "text", "text"] False "text" True,
>         CatCreateFunction "rpad" ["text", "int4"] False "text" True,
>         CatCreateFunction "rtrim" ["text", "text"] False "text" True,
>         CatCreateFunction "rtrim" ["text"] False "text" True,
>         CatCreateFunction "scalargtjoinsel"
>           ["oid", "internal", "int2", "internal", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "scalargtsel"
>           ["internal", "internal", "int4", "oid"]
>           False
>           "float8" True,
>         CatCreateFunction "scalarltjoinsel"
>           ["internal", "oid", "int2", "internal", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "scalarltsel"
>           ["internal", "oid", "int4", "internal"]
>           False
>           "float8" True,
>         CatCreateFunction "schema_to_xml" ["text", "bool", "name", "bool"]
>           False
>           "xml" True,
>         CatCreateFunction "schema_to_xml_and_xmlschema"
>           ["bool", "name", "bool", "text"]
>           False
>           "xml" True,
>         CatCreateFunction "schema_to_xmlschema"
>           ["text", "name", "bool", "bool"]
>           False
>           "xml" True,
>         CatCreateFunction "set_bit" ["bytea", "int4", "int4"] False
>           "bytea" True,
>         CatCreateFunction "set_bit" ["bit", "int4", "int4"] False "bit" True,
>         CatCreateFunction "set_byte" ["bytea", "int4", "int4"] False
>           "bytea" True,
>         CatCreateFunction "set_config" ["text", "bool", "text"] False
>           "text" True,
>         CatCreateFunction "set_masklen" ["int4", "inet"] False "inet" True,
>         CatCreateFunction "set_masklen" ["int4", "cidr"] False "cidr" True,
>         CatCreateFunction "setseed" ["float8"] False "void" True,
>         CatCreateFunction "setval" ["regclass", "int8"] False "int8" True,
>         CatCreateFunction "setval" ["bool", "regclass", "int8"] False
>           "int8" True,
>         CatCreateFunction "setweight" ["tsvector", "char"] False
>           "tsvector" True,
>         CatCreateFunction "shell_in" ["cstring"] False "opaque" True,
>         CatCreateFunction "shell_out" ["opaque"] False "cstring" True,
>         CatCreateFunction "shift_jis_2004_to_euc_jis_2004"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "shift_jis_2004_to_utf8"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "shobj_description" ["name", "oid"] False "text" True,
>         CatCreateFunction "sign" ["numeric"] False "numeric" True,
>         CatCreateFunction "sign" ["float8"] False "float8" True,
>         CatCreateFunction "similar_escape" ["text", "text"] False "text" True,
>         CatCreateFunction "sin" ["float8"] False "float8" True,
>         CatCreateFunction "sjis_to_euc_jp"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "sjis_to_mic"
>           ["cstring", "internal", "int4", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "sjis_to_utf8"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "slope" ["point", "point"] False "float8" True,
>         CatCreateFunction "smgreq" ["smgr", "smgr"] False "bool" True,
>         CatCreateFunction "smgrin" ["cstring"] False "smgr" True,
>         CatCreateFunction "smgrne" ["smgr", "smgr"] False "bool" True,
>         CatCreateFunction "smgrout" ["smgr"] False "cstring" True,
>         CatCreateFunction "split_part" ["text", "text", "int4"] False
>           "text" True,
>         CatCreateFunction "sqrt" ["float8"] False "float8" True,
>         CatCreateFunction "sqrt" ["numeric"] False "numeric" True,
>         CatCreateFunction "string_agg_finalfn" ["internal"] False "text" True,
>         CatCreateFunction "string_agg_transfn" ["internal", "text", "text"]
>           False
>           "internal" True,
>         CatCreateFunction "string_to_array" ["text", "text", "text"] False
>           "_text" True,
>         CatCreateFunction "string_to_array" ["text", "text"] False "_text" True,
>         CatCreateFunction "strip" ["tsvector"] False "tsvector" True,
>         CatCreateFunction "strpos" ["text", "text"] False "int4" True,
>         CatCreateFunction "substr" ["text", "int4", "int4"] False "text" True,
>         CatCreateFunction "substr" ["text", "int4"] False "text" True,
>         CatCreateFunction "substr" ["bytea", "int4", "int4"] False "bytea" True,
>         CatCreateFunction "substr" ["bytea", "int4"] False "bytea" True,
>         CatCreateFunction "substring" ["int4", "text", "int4"] False
>           "text" True,
>         CatCreateFunction "substring" ["text", "int4"] False "text" True,
>         CatCreateFunction "substring" ["int4", "bit", "int4"] False "bit" True,
>         CatCreateFunction "substring" ["bit", "int4"] False "bit" True,
>         CatCreateFunction "substring" ["bytea", "int4", "int4"] False
>           "bytea" True,
>         CatCreateFunction "substring" ["int4", "bytea"] False "bytea" True,
>         CatCreateFunction "substring" ["text", "text"] False "text" True,
>         CatCreateFunction "substring" ["text", "text", "text"] False
>           "text" True,
>         CatCreateFunction "table_to_xml"
>           ["bool", "bool", "text", "regclass"]
>           False
>           "xml" True,
>         CatCreateFunction "table_to_xml_and_xmlschema"
>           ["bool", "bool", "text", "regclass"]
>           False
>           "xml" True,
>         CatCreateFunction "table_to_xmlschema"
>           ["regclass", "bool", "text", "bool"]
>           False
>           "xml" True,
>         CatCreateFunction "tan" ["float8"] False "float8" True,
>         CatCreateFunction "text" ["bpchar"] False "text" True,
>         CatCreateFunction "text" ["name"] False "text" True,
>         CatCreateFunction "text" ["inet"] False "text" True,
>         CatCreateFunction "text" ["char"] False "text" True,
>         CatCreateFunction "text" ["xml"] False "text" True,
>         CatCreateFunction "text" ["bool"] False "text" True,
>         CatCreateFunction "text_ge" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_gt" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_larger" ["text", "text"] False "text" True,
>         CatCreateFunction "text_le" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_lt" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_pattern_ge" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_pattern_gt" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_pattern_le" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_pattern_lt" ["text", "text"] False "bool" True,
>         CatCreateFunction "text_smaller" ["text", "text"] False "text" True,
>         CatCreateFunction "textanycat" ["text", "anynonarray"] False
>           "text" True,
>         CatCreateFunction "textcat" ["text", "text"] False "text" True,
>         CatCreateFunction "texteq" ["text", "text"] False "bool" True,
>         CatCreateFunction "texticlike" ["text", "text"] False "bool" True,
>         CatCreateFunction "texticnlike" ["text", "text"] False "bool" True,
>         CatCreateFunction "texticregexeq" ["text", "text"] False "bool" True,
>         CatCreateFunction "texticregexne" ["text", "text"] False "bool" True,
>         CatCreateFunction "textin" ["cstring"] False "text" True,
>         CatCreateFunction "textlen" ["text"] False "int4" True,
>         CatCreateFunction "textlike" ["text", "text"] False "bool" True,
>         CatCreateFunction "textne" ["text", "text"] False "bool" True,
>         CatCreateFunction "textnlike" ["text", "text"] False "bool" True,
>         CatCreateFunction "textout" ["text"] False "cstring" True,
>         CatCreateFunction "textrecv" ["internal"] False "text" True,
>         CatCreateFunction "textregexeq" ["text", "text"] False "bool" True,
>         CatCreateFunction "textregexne" ["text", "text"] False "bool" True,
>         CatCreateFunction "textsend" ["text"] False "bytea" True,
>         CatCreateFunction "thesaurus_init" ["internal"] False "internal" True,
>         CatCreateFunction "thesaurus_lexize"
>           ["internal", "internal", "internal", "internal"]
>           False
>           "internal" True,
>         CatCreateFunction "tideq" ["tid", "tid"] False "bool" True,
>         CatCreateFunction "tidge" ["tid", "tid"] False "bool" True,
>         CatCreateFunction "tidgt" ["tid", "tid"] False "bool" True,
>         CatCreateFunction "tidin" ["cstring"] False "tid" True,
>         CatCreateFunction "tidlarger" ["tid", "tid"] False "tid" True,
>         CatCreateFunction "tidle" ["tid", "tid"] False "bool" True,
>         CatCreateFunction "tidlt" ["tid", "tid"] False "bool" True,
>         CatCreateFunction "tidne" ["tid", "tid"] False "bool" True,
>         CatCreateFunction "tidout" ["tid"] False "cstring" True,
>         CatCreateFunction "tidrecv" ["internal"] False "tid" True,
>         CatCreateFunction "tidsend" ["tid"] False "bytea" True,
>         CatCreateFunction "tidsmaller" ["tid", "tid"] False "tid" True,
>         CatCreateFunction "time" ["timestamp"] False "time" True,
>         CatCreateFunction "time" ["abstime"] False "time" True,
>         CatCreateFunction "time" ["interval"] False "time" True,
>         CatCreateFunction "time" ["time", "int4"] False "time" True,
>         CatCreateFunction "time" ["timestamptz"] False "time" True,
>         CatCreateFunction "time" ["timetz"] False "time" True,
>         CatCreateFunction "time_cmp" ["time", "time"] False "int4" True,
>         CatCreateFunction "time_eq" ["time", "time"] False "bool" True,
>         CatCreateFunction "time_ge" ["time", "time"] False "bool" True,
>         CatCreateFunction "time_gt" ["time", "time"] False "bool" True,
>         CatCreateFunction "time_hash" ["time"] False "int4" True,
>         CatCreateFunction "time_in" ["cstring", "oid", "int4"] False
>           "time" True,
>         CatCreateFunction "time_larger" ["time", "time"] False "time" True,
>         CatCreateFunction "time_le" ["time", "time"] False "bool" True,
>         CatCreateFunction "time_lt" ["time", "time"] False "bool" True,
>         CatCreateFunction "time_mi_interval" ["time", "interval"] False
>           "time" True,
>         CatCreateFunction "time_mi_time" ["time", "time"] False "interval" True,
>         CatCreateFunction "time_ne" ["time", "time"] False "bool" True,
>         CatCreateFunction "time_out" ["time"] False "cstring" True,
>         CatCreateFunction "time_pl_interval" ["time", "interval"] False
>           "time" True,
>         CatCreateFunction "time_recv" ["int4", "internal", "oid"] False
>           "time" True,
>         CatCreateFunction "time_send" ["time"] False "bytea" True,
>         CatCreateFunction "time_smaller" ["time", "time"] False "time" True,
>         CatCreateFunction "timedate_pl" ["date", "time"] False "timestamp" True,
>         CatCreateFunction "timemi" ["abstime", "reltime"] False "abstime" True,
>         CatCreateFunction "timepl" ["abstime", "reltime"] False "abstime" True,
>         CatCreateFunction "timestamp" ["timestamp", "int4"] False
>           "timestamp" True,
>         CatCreateFunction "timestamp" ["abstime"] False "timestamp" True,
>         CatCreateFunction "timestamp" ["date"] False "timestamp" True,
>         CatCreateFunction "timestamp" ["time", "date"] False "timestamp" True,
>         CatCreateFunction "timestamp" ["timestamptz"] False "timestamp" True,
>         CatCreateFunction "timestamp_cmp" ["timestamp", "timestamp"] False
>           "int4" True,
>         CatCreateFunction "timestamp_cmp_date" ["date", "timestamp"] False
>           "int4" True,
>         CatCreateFunction "timestamp_cmp_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "int4" True,
>         CatCreateFunction "timestamp_eq" ["timestamp", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_eq_date" ["date", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_eq_timestamptz"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamp_ge" ["timestamp", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_ge_date" ["timestamp", "date"] False
>           "bool" True,
>         CatCreateFunction "timestamp_ge_timestamptz"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamp_gt" ["timestamp", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_gt_date" ["date", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_gt_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamp_hash" ["timestamp"] False "int4" True,
>         CatCreateFunction "timestamp_in" ["oid", "cstring", "int4"] False
>           "timestamp" True,
>         CatCreateFunction "timestamp_larger" ["timestamp", "timestamp"]
>           False
>           "timestamp" True,
>         CatCreateFunction "timestamp_le" ["timestamp", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_le_date" ["date", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_le_timestamptz"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamp_lt" ["timestamp", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_lt_date" ["timestamp", "date"] False
>           "bool" True,
>         CatCreateFunction "timestamp_lt_timestamptz"
>           ["timestamptz", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamp_mi" ["timestamp", "timestamp"] False
>           "interval" True,
>         CatCreateFunction "timestamp_mi_interval" ["interval", "timestamp"]
>           False
>           "timestamp" True,
>         CatCreateFunction "timestamp_ne" ["timestamp", "timestamp"] False
>           "bool" True,
>         CatCreateFunction "timestamp_ne_date" ["timestamp", "date"] False
>           "bool" True,
>         CatCreateFunction "timestamp_ne_timestamptz"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamp_out" ["timestamp"] False "cstring" True,
>         CatCreateFunction "timestamp_pl_interval" ["timestamp", "interval"]
>           False
>           "timestamp" True,
>         CatCreateFunction "timestamp_recv" ["internal", "int4", "oid"]
>           False
>           "timestamp" True,
>         CatCreateFunction "timestamp_send" ["timestamp"] False "bytea" True,
>         CatCreateFunction "timestamp_smaller" ["timestamp", "timestamp"]
>           False
>           "timestamp" True,
>         CatCreateFunction "timestamptypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "timestamptypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "timestamptz" ["abstime"] False "timestamptz" True,
>         CatCreateFunction "timestamptz" ["date"] False "timestamptz" True,
>         CatCreateFunction "timestamptz" ["date", "time"] False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz" ["timetz", "date"] False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz" ["timestamptz", "int4"] False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz" ["timestamp"] False "timestamptz" True,
>         CatCreateFunction "timestamptz_cmp" ["timestamptz", "timestamptz"]
>           False
>           "int4" True,
>         CatCreateFunction "timestamptz_cmp_date" ["timestamptz", "date"]
>           False
>           "int4" True,
>         CatCreateFunction "timestamptz_cmp_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "int4" True,
>         CatCreateFunction "timestamptz_eq" ["timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_eq_date" ["date", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_eq_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_ge" ["timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_ge_date" ["date", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_ge_timestamp"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_gt" ["timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_gt_date" ["date", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_gt_timestamp"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_in" ["oid", "cstring", "int4"] False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz_larger"
>           ["timestamptz", "timestamptz"]
>           False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz_le" ["timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_le_date" ["timestamptz", "date"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_le_timestamp"
>           ["timestamptz", "timestamp"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_lt" ["timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_lt_date" ["timestamptz", "date"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_lt_timestamp"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_mi" ["timestamptz", "timestamptz"]
>           False
>           "interval" True,
>         CatCreateFunction "timestamptz_mi_interval"
>           ["timestamptz", "interval"]
>           False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz_ne" ["timestamptz", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_ne_date" ["timestamptz", "date"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_ne_timestamp"
>           ["timestamp", "timestamptz"]
>           False
>           "bool" True,
>         CatCreateFunction "timestamptz_out" ["timestamptz"] False
>           "cstring" True,
>         CatCreateFunction "timestamptz_pl_interval"
>           ["timestamptz", "interval"]
>           False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz_recv" ["internal", "int4", "oid"]
>           False
>           "timestamptz" True,
>         CatCreateFunction "timestamptz_send" ["timestamptz"] False "bytea" True,
>         CatCreateFunction "timestamptz_smaller"
>           ["timestamptz", "timestamptz"]
>           False
>           "timestamptz" True,
>         CatCreateFunction "timestamptztypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "timestamptztypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "timetypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "timetypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "timetz" ["timestamptz"] False "timetz" True,
>         CatCreateFunction "timetz" ["timetz", "int4"] False "timetz" True,
>         CatCreateFunction "timetz" ["time"] False "timetz" True,
>         CatCreateFunction "timetz_cmp" ["timetz", "timetz"] False "int4" True,
>         CatCreateFunction "timetz_eq" ["timetz", "timetz"] False "bool" True,
>         CatCreateFunction "timetz_ge" ["timetz", "timetz"] False "bool" True,
>         CatCreateFunction "timetz_gt" ["timetz", "timetz"] False "bool" True,
>         CatCreateFunction "timetz_hash" ["timetz"] False "int4" True,
>         CatCreateFunction "timetz_in" ["oid", "cstring", "int4"] False
>           "timetz" True,
>         CatCreateFunction "timetz_larger" ["timetz", "timetz"] False
>           "timetz" True,
>         CatCreateFunction "timetz_le" ["timetz", "timetz"] False "bool" True,
>         CatCreateFunction "timetz_lt" ["timetz", "timetz"] False "bool" True,
>         CatCreateFunction "timetz_mi_interval" ["timetz", "interval"] False
>           "timetz" True,
>         CatCreateFunction "timetz_ne" ["timetz", "timetz"] False "bool" True,
>         CatCreateFunction "timetz_out" ["timetz"] False "cstring" True,
>         CatCreateFunction "timetz_pl_interval" ["timetz", "interval"] False
>           "timetz" True,
>         CatCreateFunction "timetz_recv" ["int4", "internal", "oid"] False
>           "timetz" True,
>         CatCreateFunction "timetz_send" ["timetz"] False "bytea" True,
>         CatCreateFunction "timetz_smaller" ["timetz", "timetz"] False
>           "timetz" True,
>         CatCreateFunction "timetzdate_pl" ["date", "timetz"] False
>           "timestamptz" True,
>         CatCreateFunction "timetztypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "timetztypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "timezone" ["timestamptz", "interval"] False
>           "timestamp" True,
>         CatCreateFunction "timezone" ["text", "timestamptz"] False
>           "timestamp" True,
>         CatCreateFunction "timezone" ["timetz", "text"] False "timetz" True,
>         CatCreateFunction "timezone" ["interval", "timetz"] False "timetz" True,
>         CatCreateFunction "timezone" ["timestamp", "text"] False
>           "timestamptz" True,
>         CatCreateFunction "timezone" ["timestamp", "interval"] False
>           "timestamptz" True,
>         CatCreateFunction "tinterval" ["abstime", "abstime"] False
>           "tinterval" True,
>         CatCreateFunction "tintervalct" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalend" ["tinterval"] False "abstime" True,
>         CatCreateFunction "tintervaleq" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalge" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalgt" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalin" ["cstring"] False "tinterval" True,
>         CatCreateFunction "tintervalle" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalleneq" ["reltime", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervallenge" ["tinterval", "reltime"] False
>           "bool" True,
>         CatCreateFunction "tintervallengt" ["reltime", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervallenle" ["tinterval", "reltime"] False
>           "bool" True,
>         CatCreateFunction "tintervallenlt" ["tinterval", "reltime"] False
>           "bool" True,
>         CatCreateFunction "tintervallenne" ["reltime", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervallt" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalne" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalout" ["tinterval"] False "cstring" True,
>         CatCreateFunction "tintervalov" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalrecv" ["internal"] False "tinterval" True,
>         CatCreateFunction "tintervalrel" ["tinterval"] False "reltime" True,
>         CatCreateFunction "tintervalsame" ["tinterval", "tinterval"] False
>           "bool" True,
>         CatCreateFunction "tintervalsend" ["tinterval"] False "bytea" True,
>         CatCreateFunction "tintervalstart" ["tinterval"] False "abstime" True,
>         CatCreateFunction "to_ascii" ["text"] False "text" True,
>         CatCreateFunction "to_ascii" ["int4", "text"] False "text" True,
>         CatCreateFunction "to_ascii" ["name", "text"] False "text" True,
>         CatCreateFunction "to_char" ["text", "interval"] False "text" True,
>         CatCreateFunction "to_char" ["text", "timestamptz"] False "text" True,
>         CatCreateFunction "to_char" ["numeric", "text"] False "text" True,
>         CatCreateFunction "to_char" ["text", "int4"] False "text" True,
>         CatCreateFunction "to_char" ["int8", "text"] False "text" True,
>         CatCreateFunction "to_char" ["float4", "text"] False "text" True,
>         CatCreateFunction "to_char" ["float8", "text"] False "text" True,
>         CatCreateFunction "to_char" ["timestamp", "text"] False "text" True,
>         CatCreateFunction "to_date" ["text", "text"] False "date" True,
>         CatCreateFunction "to_hex" ["int4"] False "text" True,
>         CatCreateFunction "to_hex" ["int8"] False "text" True,
>         CatCreateFunction "to_number" ["text", "text"] False "numeric" True,
>         CatCreateFunction "to_timestamp" ["float8"] False "timestamptz" True,
>         CatCreateFunction "to_timestamp" ["text", "text"] False
>           "timestamptz" True,
>         CatCreateFunction "to_tsquery" ["regconfig", "text"] False
>           "tsquery" True,
>         CatCreateFunction "to_tsquery" ["text"] False "tsquery" True,
>         CatCreateFunction "to_tsvector" ["text", "regconfig"] False
>           "tsvector" True,
>         CatCreateFunction "to_tsvector" ["text"] False "tsvector" True,
>         CatCreateFunction "translate" ["text", "text", "text"] False
>           "text" True,
>         CatCreateFunction "trigger_in" ["cstring"] False "trigger" True,
>         CatCreateFunction "trigger_out" ["trigger"] False "cstring" True,
>         CatCreateFunction "trunc" ["macaddr"] False "macaddr" True,
>         CatCreateFunction "trunc" ["float8"] False "float8" True,
>         CatCreateFunction "trunc" ["numeric", "int4"] False "numeric" True,
>         CatCreateFunction "trunc" ["numeric"] False "numeric" True,
>         CatCreateFunction "truncate" ["float8"] False "float8" True,
>         CatCreateFunction "truncate" ["numeric", "int4"] False "numeric" True,
>         CatCreateFunction "truncate" ["numeric"] False "numeric" True,
>         CatCreateFunction "ts_debug" ["regconfig", "text"] True "record" True,
>         CatCreateFunction "ts_debug" ["text"] True "record" True,
>         CatCreateFunction "ts_headline"
>           ["regconfig", "text", "text", "tsquery"]
>           False
>           "text" True,
>         CatCreateFunction "ts_headline" ["text", "regconfig", "tsquery"]
>           False
>           "text" True,
>         CatCreateFunction "ts_headline" ["text", "text", "tsquery"] False
>           "text" True,
>         CatCreateFunction "ts_headline" ["text", "tsquery"] False "text" True,
>         CatCreateFunction "ts_lexize" ["text", "regdictionary"] False
>           "_text" True,
>         CatCreateFunction "ts_match_qv" ["tsquery", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "ts_match_tq" ["tsquery", "text"] False "bool" True,
>         CatCreateFunction "ts_match_tt" ["text", "text"] False "bool" True,
>         CatCreateFunction "ts_match_vq" ["tsquery", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "ts_parse" ["text", "oid"] True "record" True,
>         CatCreateFunction "ts_parse" ["text", "text"] True "record" True,
>         CatCreateFunction "ts_rank"
>           ["tsquery", "int4", "_float4", "tsvector"]
>           False
>           "float4" True,
>         CatCreateFunction "ts_rank" ["tsquery", "tsvector", "_float4"]
>           False
>           "float4" True,
>         CatCreateFunction "ts_rank" ["tsquery", "tsvector", "int4"] False
>           "float4" True,
>         CatCreateFunction "ts_rank" ["tsquery", "tsvector"] False "float4" True,
>         CatCreateFunction "ts_rank_cd"
>           ["tsquery", "_float4", "tsvector", "int4"]
>           False
>           "float4" True,
>         CatCreateFunction "ts_rank_cd" ["_float4", "tsquery", "tsvector"]
>           False
>           "float4" True,
>         CatCreateFunction "ts_rank_cd" ["tsquery", "tsvector", "int4"]
>           False
>           "float4" True,
>         CatCreateFunction "ts_rank_cd" ["tsvector", "tsquery"] False
>           "float4" True,
>         CatCreateFunction "ts_rewrite" ["tsquery", "tsquery", "tsquery"]
>           False
>           "tsquery" True,
>         CatCreateFunction "ts_rewrite" ["tsquery", "text"] False "tsquery" True,
>         CatCreateFunction "ts_stat" ["text"] True "record" True,
>         CatCreateFunction "ts_stat" ["text", "text"] True "record" True,
>         CatCreateFunction "ts_token_type" ["oid"] True "record" True,
>         CatCreateFunction "ts_token_type" ["text"] True "record" True,
>         CatCreateFunction "ts_typanalyze" ["internal"] False "bool" True,
>         CatCreateFunction "tsmatchjoinsel"
>           ["internal", "internal", "internal", "int2", "oid"]
>           False
>           "float8" True,
>         CatCreateFunction "tsmatchsel"
>           ["oid", "internal", "internal", "int4"]
>           False
>           "float8" True,
>         CatCreateFunction "tsq_mcontained" ["tsquery", "tsquery"] False
>           "bool" True,
>         CatCreateFunction "tsq_mcontains" ["tsquery", "tsquery"] False
>           "bool" True,
>         CatCreateFunction "tsquery_and" ["tsquery", "tsquery"] False
>           "tsquery" True,
>         CatCreateFunction "tsquery_cmp" ["tsquery", "tsquery"] False
>           "int4" True,
>         CatCreateFunction "tsquery_eq" ["tsquery", "tsquery"] False "bool" True,
>         CatCreateFunction "tsquery_ge" ["tsquery", "tsquery"] False "bool" True,
>         CatCreateFunction "tsquery_gt" ["tsquery", "tsquery"] False "bool" True,
>         CatCreateFunction "tsquery_le" ["tsquery", "tsquery"] False "bool" True,
>         CatCreateFunction "tsquery_lt" ["tsquery", "tsquery"] False "bool" True,
>         CatCreateFunction "tsquery_ne" ["tsquery", "tsquery"] False "bool" True,
>         CatCreateFunction "tsquery_not" ["tsquery"] False "tsquery" True,
>         CatCreateFunction "tsquery_or" ["tsquery", "tsquery"] False
>           "tsquery" True,
>         CatCreateFunction "tsqueryin" ["cstring"] False "tsquery" True,
>         CatCreateFunction "tsqueryout" ["tsquery"] False "cstring" True,
>         CatCreateFunction "tsqueryrecv" ["internal"] False "tsquery" True,
>         CatCreateFunction "tsquerysend" ["tsquery"] False "bytea" True,
>         CatCreateFunction "tsvector_cmp" ["tsvector", "tsvector"] False
>           "int4" True,
>         CatCreateFunction "tsvector_concat" ["tsvector", "tsvector"] False
>           "tsvector" True,
>         CatCreateFunction "tsvector_eq" ["tsvector", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "tsvector_ge" ["tsvector", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "tsvector_gt" ["tsvector", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "tsvector_le" ["tsvector", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "tsvector_lt" ["tsvector", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "tsvector_ne" ["tsvector", "tsvector"] False
>           "bool" True,
>         CatCreateFunction "tsvectorin" ["cstring"] False "tsvector" True,
>         CatCreateFunction "tsvectorout" ["tsvector"] False "cstring" True,
>         CatCreateFunction "tsvectorrecv" ["internal"] False "tsvector" True,
>         CatCreateFunction "tsvectorsend" ["tsvector"] False "bytea" True,
>         CatCreateFunction "txid_snapshot_in" ["cstring"] False
>           "txid_snapshot" True,
>         CatCreateFunction "txid_snapshot_out" ["txid_snapshot"] False
>           "cstring" True,
>         CatCreateFunction "txid_snapshot_recv" ["internal"] False
>           "txid_snapshot" True,
>         CatCreateFunction "txid_snapshot_send" ["txid_snapshot"] False
>           "bytea" True,
>         CatCreateFunction "txid_snapshot_xip" ["txid_snapshot"] True
>           "int8" True,
>         CatCreateFunction "txid_snapshot_xmax" ["txid_snapshot"] False
>           "int8" True,
>         CatCreateFunction "txid_snapshot_xmin" ["txid_snapshot"] False
>           "int8" True,
>         CatCreateFunction "txid_visible_in_snapshot"
>           ["txid_snapshot", "int8"]
>           False
>           "bool" True,
>         CatCreateFunction "uhc_to_utf8"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "unknownin" ["cstring"] False "unknown" True,
>         CatCreateFunction "unknownout" ["unknown"] False "cstring" True,
>         CatCreateFunction "unknownrecv" ["internal"] False "unknown" True,
>         CatCreateFunction "unknownsend" ["unknown"] False "bytea" True,
>         CatCreateFunction "unnest" ["anyarray"] True "anyelement" True,
>         CatCreateFunction "upper" ["text"] False "text" True,
>         CatCreateFunction "utf8_to_ascii"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_big5"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_euc_cn"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_euc_jis_2004"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_euc_jp"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_euc_kr"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_euc_tw"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_gb18030"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_gbk"
>           ["cstring", "int4", "int4", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_iso8859"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_iso8859_1"
>           ["cstring", "int4", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_johab"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_koi8r"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_koi8u"
>           ["cstring", "int4", "internal", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_shift_jis_2004"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_sjis"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_uhc"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "utf8_to_win"
>           ["int4", "cstring", "int4", "internal", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "uuid_cmp" ["uuid", "uuid"] False "int4" True,
>         CatCreateFunction "uuid_eq" ["uuid", "uuid"] False "bool" True,
>         CatCreateFunction "uuid_ge" ["uuid", "uuid"] False "bool" True,
>         CatCreateFunction "uuid_gt" ["uuid", "uuid"] False "bool" True,
>         CatCreateFunction "uuid_hash" ["uuid"] False "int4" True,
>         CatCreateFunction "uuid_in" ["cstring"] False "uuid" True,
>         CatCreateFunction "uuid_le" ["uuid", "uuid"] False "bool" True,
>         CatCreateFunction "uuid_lt" ["uuid", "uuid"] False "bool" True,
>         CatCreateFunction "uuid_ne" ["uuid", "uuid"] False "bool" True,
>         CatCreateFunction "uuid_out" ["uuid"] False "cstring" True,
>         CatCreateFunction "uuid_recv" ["internal"] False "uuid" True,
>         CatCreateFunction "uuid_send" ["uuid"] False "bytea" True,
>         CatCreateFunction "varbit" ["bool", "varbit", "int4"] False
>           "varbit" True,
>         CatCreateFunction "varbit_in" ["oid", "cstring", "int4"] False
>           "varbit" True,
>         CatCreateFunction "varbit_out" ["varbit"] False "cstring" True,
>         CatCreateFunction "varbit_recv" ["internal", "oid", "int4"] False
>           "varbit" True,
>         CatCreateFunction "varbit_send" ["varbit"] False "bytea" True,
>         CatCreateFunction "varbitcmp" ["varbit", "varbit"] False "int4" True,
>         CatCreateFunction "varbiteq" ["varbit", "varbit"] False "bool" True,
>         CatCreateFunction "varbitge" ["varbit", "varbit"] False "bool" True,
>         CatCreateFunction "varbitgt" ["varbit", "varbit"] False "bool" True,
>         CatCreateFunction "varbitle" ["varbit", "varbit"] False "bool" True,
>         CatCreateFunction "varbitlt" ["varbit", "varbit"] False "bool" True,
>         CatCreateFunction "varbitne" ["varbit", "varbit"] False "bool" True,
>         CatCreateFunction "varbittypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "varbittypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "varchar" ["int4", "bool", "varchar"] False
>           "varchar" True,
>         CatCreateFunction "varchar" ["name"] False "varchar" True,
>         CatCreateFunction "varcharin" ["cstring", "int4", "oid"] False
>           "varchar" True,
>         CatCreateFunction "varcharout" ["varchar"] False "cstring" True,
>         CatCreateFunction "varcharrecv" ["int4", "oid", "internal"] False
>           "varchar" True,
>         CatCreateFunction "varcharsend" ["varchar"] False "bytea" True,
>         CatCreateFunction "varchartypmodin" ["_cstring"] False "int4" True,
>         CatCreateFunction "varchartypmodout" ["int4"] False "cstring" True,
>         CatCreateFunction "void_in" ["cstring"] False "void" True,
>         CatCreateFunction "void_out" ["void"] False "cstring" True,
>         CatCreateFunction "void_recv" ["internal"] False "void" True,
>         CatCreateFunction "void_send" ["void"] False "bytea" True,
>         CatCreateFunction "width" ["box"] False "float8" True,
>         CatCreateFunction "width_bucket"
>           ["float8", "float8", "float8", "int4"]
>           False
>           "int4" True,
>         CatCreateFunction "width_bucket"
>           ["numeric", "int4", "numeric", "numeric"]
>           False
>           "int4" True,
>         CatCreateFunction "win1250_to_latin2"
>           ["int4", "cstring", "int4", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "win1250_to_mic"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "win1251_to_iso"
>           ["internal", "int4", "int4", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "win1251_to_koi8r"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "win1251_to_mic"
>           ["int4", "int4", "cstring", "int4", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "win1251_to_win866"
>           ["internal", "cstring", "int4", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "win866_to_iso"
>           ["cstring", "internal", "int4", "int4", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "win866_to_koi8r"
>           ["int4", "int4", "internal", "int4", "cstring"]
>           False
>           "void" True,
>         CatCreateFunction "win866_to_mic"
>           ["internal", "int4", "int4", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "win866_to_win1251"
>           ["int4", "int4", "internal", "cstring", "int4"]
>           False
>           "void" True,
>         CatCreateFunction "win_to_utf8"
>           ["int4", "int4", "int4", "cstring", "internal"]
>           False
>           "void" True,
>         CatCreateFunction "xideq" ["xid", "xid"] False "bool" True,
>         CatCreateFunction "xideqint4" ["xid", "int4"] False "bool" True,
>         CatCreateFunction "xidin" ["cstring"] False "xid" True,
>         CatCreateFunction "xidout" ["xid"] False "cstring" True,
>         CatCreateFunction "xidrecv" ["internal"] False "xid" True,
>         CatCreateFunction "xidsend" ["xid"] False "bytea" True,
>         CatCreateFunction "xml" ["text"] False "xml" True,
>         CatCreateFunction "xml_in" ["cstring"] False "xml" True,
>         CatCreateFunction "xml_is_well_formed" ["text"] False "bool" True,
>         CatCreateFunction "xml_is_well_formed_content" ["text"] False
>           "bool" True,
>         CatCreateFunction "xml_is_well_formed_document" ["text"] False
>           "bool" True,
>         CatCreateFunction "xml_out" ["xml"] False "cstring" True,
>         CatCreateFunction "xml_recv" ["internal"] False "xml" True,
>         CatCreateFunction "xml_send" ["xml"] False "bytea" True,
>         CatCreateFunction "xmlcomment" ["text"] False "xml" True,
>         CatCreateFunction "xmlconcat2" ["xml", "xml"] False "xml" True,
>         CatCreateFunction "xmlexists" ["xml", "text"] False "bool" True,
>         CatCreateFunction "xmlvalidate" ["xml", "text"] False "bool" True,
>         CatCreateFunction "xpath" ["text", "_text", "xml"] False "_xml" True,
>         CatCreateFunction "xpath" ["xml", "text"] False "_xml" True,
>         CatCreateFunction "xpath_exists" ["xml", "_text", "text"] False
>           "bool" True,
>         CatCreateFunction "xpath_exists" ["text", "xml"] False "bool" True,
>         CatCreateAggregate "array_agg" ["anyelement"] "anyarray",
>         CatCreateAggregate "avg" ["int8"] "numeric",
>         CatCreateAggregate "avg" ["int4"] "numeric",
>         CatCreateAggregate "avg" ["int2"] "numeric",
>         CatCreateAggregate "avg" ["numeric"] "numeric",
>         CatCreateAggregate "avg" ["float4"] "float8",
>         CatCreateAggregate "avg" ["float8"] "float8",
>         CatCreateAggregate "avg" ["interval"] "interval",
>         CatCreateAggregate "bit_and" ["int2"] "int2",
>         CatCreateAggregate "bit_and" ["int4"] "int4",
>         CatCreateAggregate "bit_and" ["int8"] "int8",
>         CatCreateAggregate "bit_and" ["bit"] "bit",
>         CatCreateAggregate "bit_or" ["int2"] "int2",
>         CatCreateAggregate "bit_or" ["int4"] "int4",
>         CatCreateAggregate "bit_or" ["int8"] "int8",
>         CatCreateAggregate "bit_or" ["bit"] "bit",
>         CatCreateAggregate "bool_and" ["bool"] "bool",
>         CatCreateAggregate "bool_or" ["bool"] "bool",
>         CatCreateAggregate "corr" ["float8", "float8"] "float8",
>         CatCreateAggregate "count" ["any"] "int8",
>         CatCreateAggregate "covar_pop" ["float8", "float8"] "float8",
>         CatCreateAggregate "covar_samp" ["float8", "float8"] "float8",
>         CatCreateAggregate "every" ["bool"] "bool",
>         CatCreateAggregate "max" ["anyarray"] "anyarray",
>         CatCreateAggregate "max" ["int8"] "int8",
>         CatCreateAggregate "max" ["int4"] "int4",
>         CatCreateAggregate "max" ["int2"] "int2",
>         CatCreateAggregate "max" ["oid"] "oid",
>         CatCreateAggregate "max" ["float4"] "float4",
>         CatCreateAggregate "max" ["float8"] "float8",
>         CatCreateAggregate "max" ["abstime"] "abstime",
>         CatCreateAggregate "max" ["date"] "date",
>         CatCreateAggregate "max" ["time"] "time",
>         CatCreateAggregate "max" ["timetz"] "timetz",
>         CatCreateAggregate "max" ["money"] "money",
>         CatCreateAggregate "max" ["timestamp"] "timestamp",
>         CatCreateAggregate "max" ["timestamptz"] "timestamptz",
>         CatCreateAggregate "max" ["interval"] "interval",
>         CatCreateAggregate "max" ["text"] "text",
>         CatCreateAggregate "max" ["numeric"] "numeric",
>         CatCreateAggregate "max" ["bpchar"] "bpchar",
>         CatCreateAggregate "max" ["tid"] "tid",
>         CatCreateAggregate "max" ["anyenum"] "anyenum",
>         CatCreateAggregate "min" ["anyarray"] "anyarray",
>         CatCreateAggregate "min" ["int8"] "int8",
>         CatCreateAggregate "min" ["int4"] "int4",
>         CatCreateAggregate "min" ["int2"] "int2",
>         CatCreateAggregate "min" ["oid"] "oid",
>         CatCreateAggregate "min" ["float4"] "float4",
>         CatCreateAggregate "min" ["float8"] "float8",
>         CatCreateAggregate "min" ["abstime"] "abstime",
>         CatCreateAggregate "min" ["date"] "date",
>         CatCreateAggregate "min" ["time"] "time",
>         CatCreateAggregate "min" ["timetz"] "timetz",
>         CatCreateAggregate "min" ["money"] "money",
>         CatCreateAggregate "min" ["timestamp"] "timestamp",
>         CatCreateAggregate "min" ["timestamptz"] "timestamptz",
>         CatCreateAggregate "min" ["interval"] "interval",
>         CatCreateAggregate "min" ["text"] "text",
>         CatCreateAggregate "min" ["numeric"] "numeric",
>         CatCreateAggregate "min" ["bpchar"] "bpchar",
>         CatCreateAggregate "min" ["tid"] "tid",
>         CatCreateAggregate "min" ["anyenum"] "anyenum",
>         CatCreateAggregate "regr_avgx" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_avgy" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_count" ["float8", "float8"] "int8",
>         CatCreateAggregate "regr_intercept" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_r2" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_slope" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_sxx" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_sxy" ["float8", "float8"] "float8",
>         CatCreateAggregate "regr_syy" ["float8", "float8"] "float8",
>         CatCreateAggregate "stddev" ["int8"] "numeric",
>         CatCreateAggregate "stddev" ["int4"] "numeric",
>         CatCreateAggregate "stddev" ["int2"] "numeric",
>         CatCreateAggregate "stddev" ["float4"] "float8",
>         CatCreateAggregate "stddev" ["float8"] "float8",
>         CatCreateAggregate "stddev" ["numeric"] "numeric",
>         CatCreateAggregate "stddev_pop" ["int8"] "numeric",
>         CatCreateAggregate "stddev_pop" ["int4"] "numeric",
>         CatCreateAggregate "stddev_pop" ["int2"] "numeric",
>         CatCreateAggregate "stddev_pop" ["float4"] "float8",
>         CatCreateAggregate "stddev_pop" ["float8"] "float8",
>         CatCreateAggregate "stddev_pop" ["numeric"] "numeric",
>         CatCreateAggregate "stddev_samp" ["int8"] "numeric",
>         CatCreateAggregate "stddev_samp" ["int4"] "numeric",
>         CatCreateAggregate "stddev_samp" ["int2"] "numeric",
>         CatCreateAggregate "stddev_samp" ["float4"] "float8",
>         CatCreateAggregate "stddev_samp" ["float8"] "float8",
>         CatCreateAggregate "stddev_samp" ["numeric"] "numeric",
>         CatCreateAggregate "string_agg" ["text", "text"] "text",
>         CatCreateAggregate "sum" ["int8"] "numeric",
>         CatCreateAggregate "sum" ["int4"] "int8",
>         CatCreateAggregate "sum" ["int2"] "int8",
>         CatCreateAggregate "sum" ["float4"] "float4",
>         CatCreateAggregate "sum" ["float8"] "float8",
>         CatCreateAggregate "sum" ["money"] "money",
>         CatCreateAggregate "sum" ["interval"] "interval",
>         CatCreateAggregate "sum" ["numeric"] "numeric",
>         CatCreateAggregate "var_pop" ["int8"] "numeric",
>         CatCreateAggregate "var_pop" ["int4"] "numeric",
>         CatCreateAggregate "var_pop" ["int2"] "numeric",
>         CatCreateAggregate "var_pop" ["float4"] "float8",
>         CatCreateAggregate "var_pop" ["float8"] "float8",
>         CatCreateAggregate "var_pop" ["numeric"] "numeric",
>         CatCreateAggregate "var_samp" ["int8"] "numeric",
>         CatCreateAggregate "var_samp" ["int4"] "numeric",
>         CatCreateAggregate "var_samp" ["int2"] "numeric",
>         CatCreateAggregate "var_samp" ["float4"] "float8",
>         CatCreateAggregate "var_samp" ["float8"] "float8",
>         CatCreateAggregate "var_samp" ["numeric"] "numeric",
>         CatCreateAggregate "variance" ["int8"] "numeric",
>         CatCreateAggregate "variance" ["int4"] "numeric",
>         CatCreateAggregate "variance" ["int2"] "numeric",
>         CatCreateAggregate "variance" ["float4"] "float8",
>         CatCreateAggregate "variance" ["float8"] "float8",
>         CatCreateAggregate "variance" ["numeric"] "numeric",
>         CatCreateAggregate "xmlagg" ["xml"] "xml",
>         CatCreateCast "abstime" "date" AssignmentCastContext,
>         CatCreateCast "abstime" "int4" ExplicitCastContext,
>         CatCreateCast "abstime" "time" AssignmentCastContext,
>         CatCreateCast "abstime" "timestamp" ImplicitCastContext,
>         CatCreateCast "abstime" "timestamptz" ImplicitCastContext,
>         CatCreateCast "bit" "bit" ImplicitCastContext,
>         CatCreateCast "bit" "int4" ExplicitCastContext,
>         CatCreateCast "bit" "int8" ExplicitCastContext,
>         CatCreateCast "bit" "varbit" ImplicitCastContext,
>         CatCreateCast "bool" "bpchar" AssignmentCastContext,
>         CatCreateCast "bool" "int4" ExplicitCastContext,
>         CatCreateCast "bool" "text" AssignmentCastContext,
>         CatCreateCast "bool" "varchar" AssignmentCastContext,
>         CatCreateCast "box" "circle" ExplicitCastContext,
>         CatCreateCast "box" "lseg" ExplicitCastContext,
>         CatCreateCast "box" "point" ExplicitCastContext,
>         CatCreateCast "box" "polygon" AssignmentCastContext,
>         CatCreateCast "bpchar" "bpchar" ImplicitCastContext,
>         CatCreateCast "bpchar" "char" AssignmentCastContext,
>         CatCreateCast "bpchar" "name" ImplicitCastContext,
>         CatCreateCast "bpchar" "text" ImplicitCastContext,
>         CatCreateCast "bpchar" "varchar" ImplicitCastContext,
>         CatCreateCast "bpchar" "xml" ExplicitCastContext,
>         CatCreateCast "char" "bpchar" AssignmentCastContext,
>         CatCreateCast "char" "int4" ExplicitCastContext,
>         CatCreateCast "char" "text" ImplicitCastContext,
>         CatCreateCast "char" "varchar" AssignmentCastContext,
>         CatCreateCast "cidr" "bpchar" AssignmentCastContext,
>         CatCreateCast "cidr" "inet" ImplicitCastContext,
>         CatCreateCast "cidr" "text" AssignmentCastContext,
>         CatCreateCast "cidr" "varchar" AssignmentCastContext,
>         CatCreateCast "circle" "box" ExplicitCastContext,
>         CatCreateCast "circle" "point" ExplicitCastContext,
>         CatCreateCast "circle" "polygon" ExplicitCastContext,
>         CatCreateCast "date" "timestamp" ImplicitCastContext,
>         CatCreateCast "date" "timestamptz" ImplicitCastContext,
>         CatCreateCast "float4" "float8" ImplicitCastContext,
>         CatCreateCast "float4" "int2" AssignmentCastContext,
>         CatCreateCast "float4" "int4" AssignmentCastContext,
>         CatCreateCast "float4" "int8" AssignmentCastContext,
>         CatCreateCast "float4" "numeric" AssignmentCastContext,
>         CatCreateCast "float8" "float4" AssignmentCastContext,
>         CatCreateCast "float8" "int2" AssignmentCastContext,
>         CatCreateCast "float8" "int4" AssignmentCastContext,
>         CatCreateCast "float8" "int8" AssignmentCastContext,
>         CatCreateCast "float8" "numeric" AssignmentCastContext,
>         CatCreateCast "inet" "bpchar" AssignmentCastContext,
>         CatCreateCast "inet" "cidr" AssignmentCastContext,
>         CatCreateCast "inet" "text" AssignmentCastContext,
>         CatCreateCast "inet" "varchar" AssignmentCastContext,
>         CatCreateCast "int2" "float4" ImplicitCastContext,
>         CatCreateCast "int2" "float8" ImplicitCastContext,
>         CatCreateCast "int2" "int4" ImplicitCastContext,
>         CatCreateCast "int2" "int8" ImplicitCastContext,
>         CatCreateCast "int2" "numeric" ImplicitCastContext,
>         CatCreateCast "int2" "oid" ImplicitCastContext,
>         CatCreateCast "int2" "regclass" ImplicitCastContext,
>         CatCreateCast "int2" "regconfig" ImplicitCastContext,
>         CatCreateCast "int2" "regdictionary" ImplicitCastContext,
>         CatCreateCast "int2" "regoper" ImplicitCastContext,
>         CatCreateCast "int2" "regoperator" ImplicitCastContext,
>         CatCreateCast "int2" "regproc" ImplicitCastContext,
>         CatCreateCast "int2" "regprocedure" ImplicitCastContext,
>         CatCreateCast "int2" "regtype" ImplicitCastContext,
>         CatCreateCast "int4" "abstime" ExplicitCastContext,
>         CatCreateCast "int4" "bit" ExplicitCastContext,
>         CatCreateCast "int4" "bool" ExplicitCastContext,
>         CatCreateCast "int4" "char" ExplicitCastContext,
>         CatCreateCast "int4" "float4" ImplicitCastContext,
>         CatCreateCast "int4" "float8" ImplicitCastContext,
>         CatCreateCast "int4" "int2" AssignmentCastContext,
>         CatCreateCast "int4" "int8" ImplicitCastContext,
>         CatCreateCast "int4" "money" AssignmentCastContext,
>         CatCreateCast "int4" "numeric" ImplicitCastContext,
>         CatCreateCast "int4" "oid" ImplicitCastContext,
>         CatCreateCast "int4" "regclass" ImplicitCastContext,
>         CatCreateCast "int4" "regconfig" ImplicitCastContext,
>         CatCreateCast "int4" "regdictionary" ImplicitCastContext,
>         CatCreateCast "int4" "regoper" ImplicitCastContext,
>         CatCreateCast "int4" "regoperator" ImplicitCastContext,
>         CatCreateCast "int4" "regproc" ImplicitCastContext,
>         CatCreateCast "int4" "regprocedure" ImplicitCastContext,
>         CatCreateCast "int4" "regtype" ImplicitCastContext,
>         CatCreateCast "int4" "reltime" ExplicitCastContext,
>         CatCreateCast "int8" "bit" ExplicitCastContext,
>         CatCreateCast "int8" "float4" ImplicitCastContext,
>         CatCreateCast "int8" "float8" ImplicitCastContext,
>         CatCreateCast "int8" "int2" AssignmentCastContext,
>         CatCreateCast "int8" "int4" AssignmentCastContext,
>         CatCreateCast "int8" "money" AssignmentCastContext,
>         CatCreateCast "int8" "numeric" ImplicitCastContext,
>         CatCreateCast "int8" "oid" ImplicitCastContext,
>         CatCreateCast "int8" "regclass" ImplicitCastContext,
>         CatCreateCast "int8" "regconfig" ImplicitCastContext,
>         CatCreateCast "int8" "regdictionary" ImplicitCastContext,
>         CatCreateCast "int8" "regoper" ImplicitCastContext,
>         CatCreateCast "int8" "regoperator" ImplicitCastContext,
>         CatCreateCast "int8" "regproc" ImplicitCastContext,
>         CatCreateCast "int8" "regprocedure" ImplicitCastContext,
>         CatCreateCast "int8" "regtype" ImplicitCastContext,
>         CatCreateCast "interval" "interval" ImplicitCastContext,
>         CatCreateCast "interval" "reltime" AssignmentCastContext,
>         CatCreateCast "interval" "time" AssignmentCastContext,
>         CatCreateCast "lseg" "point" ExplicitCastContext,
>         CatCreateCast "money" "numeric" AssignmentCastContext,
>         CatCreateCast "name" "bpchar" AssignmentCastContext,
>         CatCreateCast "name" "text" ImplicitCastContext,
>         CatCreateCast "name" "varchar" AssignmentCastContext,
>         CatCreateCast "numeric" "float4" ImplicitCastContext,
>         CatCreateCast "numeric" "float8" ImplicitCastContext,
>         CatCreateCast "numeric" "int2" AssignmentCastContext,
>         CatCreateCast "numeric" "int4" AssignmentCastContext,
>         CatCreateCast "numeric" "int8" AssignmentCastContext,
>         CatCreateCast "numeric" "money" AssignmentCastContext,
>         CatCreateCast "numeric" "numeric" ImplicitCastContext,
>         CatCreateCast "oid" "int4" AssignmentCastContext,
>         CatCreateCast "oid" "int8" AssignmentCastContext,
>         CatCreateCast "oid" "regclass" ImplicitCastContext,
>         CatCreateCast "oid" "regconfig" ImplicitCastContext,
>         CatCreateCast "oid" "regdictionary" ImplicitCastContext,
>         CatCreateCast "oid" "regoper" ImplicitCastContext,
>         CatCreateCast "oid" "regoperator" ImplicitCastContext,
>         CatCreateCast "oid" "regproc" ImplicitCastContext,
>         CatCreateCast "oid" "regprocedure" ImplicitCastContext,
>         CatCreateCast "oid" "regtype" ImplicitCastContext,
>         CatCreateCast "path" "point" ExplicitCastContext,
>         CatCreateCast "path" "polygon" AssignmentCastContext,
>         CatCreateCast "pg_node_tree" "text" ImplicitCastContext,
>         CatCreateCast "polygon" "box" ExplicitCastContext,
>         CatCreateCast "polygon" "circle" ExplicitCastContext,
>         CatCreateCast "polygon" "path" AssignmentCastContext,
>         CatCreateCast "polygon" "point" ExplicitCastContext,
>         CatCreateCast "regclass" "int4" AssignmentCastContext,
>         CatCreateCast "regclass" "int8" AssignmentCastContext,
>         CatCreateCast "regclass" "oid" ImplicitCastContext,
>         CatCreateCast "regconfig" "int4" AssignmentCastContext,
>         CatCreateCast "regconfig" "int8" AssignmentCastContext,
>         CatCreateCast "regconfig" "oid" ImplicitCastContext,
>         CatCreateCast "regdictionary" "int4" AssignmentCastContext,
>         CatCreateCast "regdictionary" "int8" AssignmentCastContext,
>         CatCreateCast "regdictionary" "oid" ImplicitCastContext,
>         CatCreateCast "regoper" "int4" AssignmentCastContext,
>         CatCreateCast "regoper" "int8" AssignmentCastContext,
>         CatCreateCast "regoper" "oid" ImplicitCastContext,
>         CatCreateCast "regoper" "regoperator" ImplicitCastContext,
>         CatCreateCast "regoperator" "int4" AssignmentCastContext,
>         CatCreateCast "regoperator" "int8" AssignmentCastContext,
>         CatCreateCast "regoperator" "oid" ImplicitCastContext,
>         CatCreateCast "regoperator" "regoper" ImplicitCastContext,
>         CatCreateCast "regproc" "int4" AssignmentCastContext,
>         CatCreateCast "regproc" "int8" AssignmentCastContext,
>         CatCreateCast "regproc" "oid" ImplicitCastContext,
>         CatCreateCast "regproc" "regprocedure" ImplicitCastContext,
>         CatCreateCast "regprocedure" "int4" AssignmentCastContext,
>         CatCreateCast "regprocedure" "int8" AssignmentCastContext,
>         CatCreateCast "regprocedure" "oid" ImplicitCastContext,
>         CatCreateCast "regprocedure" "regproc" ImplicitCastContext,
>         CatCreateCast "regtype" "int4" AssignmentCastContext,
>         CatCreateCast "regtype" "int8" AssignmentCastContext,
>         CatCreateCast "regtype" "oid" ImplicitCastContext,
>         CatCreateCast "reltime" "int4" ExplicitCastContext,
>         CatCreateCast "reltime" "interval" ImplicitCastContext,
>         CatCreateCast "text" "bpchar" ImplicitCastContext,
>         CatCreateCast "text" "char" AssignmentCastContext,
>         CatCreateCast "text" "name" ImplicitCastContext,
>         CatCreateCast "text" "regclass" ImplicitCastContext,
>         CatCreateCast "text" "varchar" ImplicitCastContext,
>         CatCreateCast "text" "xml" ExplicitCastContext,
>         CatCreateCast "time" "interval" ImplicitCastContext,
>         CatCreateCast "time" "time" ImplicitCastContext,
>         CatCreateCast "time" "timetz" ImplicitCastContext,
>         CatCreateCast "timestamp" "abstime" AssignmentCastContext,
>         CatCreateCast "timestamp" "date" AssignmentCastContext,
>         CatCreateCast "timestamp" "time" AssignmentCastContext,
>         CatCreateCast "timestamp" "timestamp" ImplicitCastContext,
>         CatCreateCast "timestamp" "timestamptz" ImplicitCastContext,
>         CatCreateCast "timestamptz" "abstime" AssignmentCastContext,
>         CatCreateCast "timestamptz" "date" AssignmentCastContext,
>         CatCreateCast "timestamptz" "time" AssignmentCastContext,
>         CatCreateCast "timestamptz" "timestamp" AssignmentCastContext,
>         CatCreateCast "timestamptz" "timestamptz" ImplicitCastContext,
>         CatCreateCast "timestamptz" "timetz" AssignmentCastContext,
>         CatCreateCast "timetz" "time" AssignmentCastContext,
>         CatCreateCast "timetz" "timetz" ImplicitCastContext,
>         CatCreateCast "varbit" "bit" ImplicitCastContext,
>         CatCreateCast "varbit" "varbit" ImplicitCastContext,
>         CatCreateCast "varchar" "bpchar" ImplicitCastContext,
>         CatCreateCast "varchar" "char" AssignmentCastContext,
>         CatCreateCast "varchar" "name" ImplicitCastContext,
>         CatCreateCast "varchar" "regclass" ImplicitCastContext,
>         CatCreateCast "varchar" "text" ImplicitCastContext,
>         CatCreateCast "varchar" "varchar" ImplicitCastContext,
>         CatCreateCast "varchar" "xml" ExplicitCastContext,
>         CatCreateCast "xml" "bpchar" AssignmentCastContext,
>         CatCreateCast "xml" "text" AssignmentCastContext,
>         CatCreateCast "xml" "varchar" AssignmentCastContext,
>         CatCreateTypeCategoryEntry "abstime" ("D", False),
>         CatCreateTypeCategoryEntry "aclitem" ("U", False),
>         CatCreateTypeCategoryEntry "bit" ("V", False),
>         CatCreateTypeCategoryEntry "bool" ("B", True),
>         CatCreateTypeCategoryEntry "box" ("G", False),
>         CatCreateTypeCategoryEntry "bpchar" ("S", False),
>         CatCreateTypeCategoryEntry "bytea" ("U", False),
>         CatCreateTypeCategoryEntry "char" ("S", False),
>         CatCreateTypeCategoryEntry "cid" ("U", False),
>         CatCreateTypeCategoryEntry "cidr" ("I", False),
>         CatCreateTypeCategoryEntry "circle" ("G", False),
>         CatCreateTypeCategoryEntry "date" ("D", False),
>         CatCreateTypeCategoryEntry "float4" ("N", False),
>         CatCreateTypeCategoryEntry "float8" ("N", True),
>         CatCreateTypeCategoryEntry "gtsvector" ("U", False),
>         CatCreateTypeCategoryEntry "inet" ("I", True),
>         CatCreateTypeCategoryEntry "int2" ("N", False),
>         CatCreateTypeCategoryEntry "int2vector" ("A", False),
>         CatCreateTypeCategoryEntry "int4" ("N", False),
>         CatCreateTypeCategoryEntry "int8" ("N", False),
>         CatCreateTypeCategoryEntry "interval" ("T", True),
>         CatCreateTypeCategoryEntry "line" ("G", False),
>         CatCreateTypeCategoryEntry "lseg" ("G", False),
>         CatCreateTypeCategoryEntry "macaddr" ("U", False),
>         CatCreateTypeCategoryEntry "money" ("N", False),
>         CatCreateTypeCategoryEntry "name" ("S", False),
>         CatCreateTypeCategoryEntry "numeric" ("N", False),
>         CatCreateTypeCategoryEntry "oid" ("N", True),
>         CatCreateTypeCategoryEntry "oidvector" ("A", False),
>         CatCreateTypeCategoryEntry "path" ("G", False),
>         CatCreateTypeCategoryEntry "point" ("G", False),
>         CatCreateTypeCategoryEntry "polygon" ("G", False),
>         CatCreateTypeCategoryEntry "refcursor" ("U", False),
>         CatCreateTypeCategoryEntry "regclass" ("N", False),
>         CatCreateTypeCategoryEntry "regconfig" ("N", False),
>         CatCreateTypeCategoryEntry "regdictionary" ("N", False),
>         CatCreateTypeCategoryEntry "regoper" ("N", False),
>         CatCreateTypeCategoryEntry "regoperator" ("N", False),
>         CatCreateTypeCategoryEntry "regproc" ("N", False),
>         CatCreateTypeCategoryEntry "regprocedure" ("N", False),
>         CatCreateTypeCategoryEntry "regtype" ("N", False),
>         CatCreateTypeCategoryEntry "reltime" ("T", False),
>         CatCreateTypeCategoryEntry "text" ("S", True),
>         CatCreateTypeCategoryEntry "tid" ("U", False),
>         CatCreateTypeCategoryEntry "time" ("D", False),
>         CatCreateTypeCategoryEntry "timestamp" ("D", False),
>         CatCreateTypeCategoryEntry "timestamptz" ("D", True),
>         CatCreateTypeCategoryEntry "timetz" ("D", False),
>         CatCreateTypeCategoryEntry "tinterval" ("T", False),
>         CatCreateTypeCategoryEntry "tsquery" ("U", False),
>         CatCreateTypeCategoryEntry "tsvector" ("U", False),
>         CatCreateTypeCategoryEntry "txid_snapshot" ("U", False),
>         CatCreateTypeCategoryEntry "uuid" ("U", False),
>         CatCreateTypeCategoryEntry "varbit" ("V", True),
>         CatCreateTypeCategoryEntry "varchar" ("S", False),
>         CatCreateTypeCategoryEntry "xid" ("U", False),
>         CatCreateTypeCategoryEntry "xml" ("U", False)]
>
>        ++ odbcCatalog)

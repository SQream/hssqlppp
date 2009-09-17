Copyright 2009 Jake Wheat

This file contains a bunch of small low level utilities to help with
type checking.

> module Database.HsSqlPpp.AstUtils
>     (
>      OperatorType(..)
>     ,getOperatorType
>     ,checkTypes
>     ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool
>     ,canonicalizeTypeName
>     ,checkTypeExists
>     ,lookupTypeByName
>     ,keywordOperatorTypes
>     ,specialFunctionTypes
>     ,isArrayType
>     ,unwrapArray
>     ,unwrapSetOfComposite
>     ,unwrapSetOf
>     ,unwrapComposite
>     ,consComposite
>     ,unwrapRowCtor
>     ,Message(..)
>     ,MessageStuff(..)
>     ) where

> import Data.Maybe
> import Data.List
> import Control.Monad.Error

> import Database.HsSqlPpp.TypeType
> import Database.HsSqlPpp.Scope
> import Database.HsSqlPpp.DefaultScope

================================================================================

= getOperatorType

used by the pretty printer, not sure this is a very good design

for now, assume that all the overloaded operators that have the
same name are all either binary, prefix or postfix, otherwise the
getoperatortype would need the types of the arguments to determine
the operator type, and the parser would have to be a lot cleverer

this is why binary @ operator isn't currently supported

> data OperatorType = BinaryOp | PrefixOp | PostfixOp
>                   deriving (Eq,Show)

> getOperatorType :: String -> OperatorType
> getOperatorType s = case () of
>                       _ | any (\(x,_,_) -> x == s) (scopeBinaryOperators defaultScope) ->
>                             BinaryOp
>                         | any (\(x,_,_) -> x == s ||
>                                            (x=="-" && s=="u-"))
>                               (scopePrefixOperators defaultScope) ->
>                             PrefixOp
>                         | any (\(x,_,_) -> x == s) (scopePostfixOperators defaultScope) ->
>                             PostfixOp
>                         | s `elem` ["!and", "!or","!like"] -> BinaryOp
>                         | s `elem` ["!not"] -> PrefixOp
>                         | s `elem` ["!isNull", "!isNotNull"] -> PostfixOp
>                         | otherwise ->
>                             error $ "don't know flavour of operator " ++ s

================================================================================

= type checking utils

The way most of the error handling works is all type errors live as
types and get returned as types, instead of e.g. using exceptions or
Either. All the type checking routines for each node should be in the
following form, or equivalent:

1. Take the node types of the sub nodes (e.g. the args for a function
invocation).

2. Check each type for a type error, pass any errors on as the type for
this node - so type errors filter up to the top level statements in
this way.

3. Check each type for unknown - this represents some type checking
which hasn't been coded. All the type calculations give up completely
when they come across an unknown type and just propagate unknown.

4a. If the node has no nodeType rule, it uses the default one which
just propagates the types of its subnodes. This sometimes results in
them having an uncannily accurate type, and sometimes a really weird
type, rather than returning unknown.

4b. If the node has a nodeType rule, calculate the actual type we want
for this node using the subnode types which we've already checked for
unknowns and errors. Any new type errors become this node's type, or
otherwise it is set to the calculated type.

Using laziness, in code this usually looks something like

thisNodeType =  checkErrors [@subnodea.nodeType
                            ,@subnodeb.nodetype] calcThisNodeType
where
  calcThisNodeType = some code referring to @subnodea.nodeType
                       and @subnodeb.nodetype

The checkErrors function, and its auxiliary unkErr, does the error and
unknown propagation.

Once the annotations are done properly, type errors won't need to rise
up the tree like this.

== checkErrors

runs through the types in the first list looking for type errors or
unknowns, if it finds any, return them, else return the second
argument. See unkErr below for exactly how it finds errors and
unknowns. It will only return errors from the first type containing
errors, which might need looking at when the focus is on good error
messages.

> checkTypes :: [Type] -> Either TypeError Type -> Either TypeError Type
> checkTypes (TypeCheckFailed:_) _ = Right TypeCheckFailed
> checkTypes (_:ts) r = checkTypes ts r
> checkTypes [] r = r

================================================================================

= basic types

random notes on pg types:

== domains:
the point of domains is you can't put constraints on types, but you
can wrap a type up in a domain and put a constraint on it there.

== literals/selectors

source strings are parsed as unknown type: they can be implicitly cast
to almost any type in the right context.

rows ctors can also be implicitly cast to any composite type matching
the elements (now sure how exactly are they matched? - number of
elements, type compatibility of elements, just by context?).

string literals are not checked for valid syntax currently, but this
will probably change so we can type check string literals statically.
Postgres defers all checking to runtime, because it has to cope with
custom data types. This code will allow adding a grammar checker for
each type so you can optionally check the string lits statically.

== notes on type checking types

=== basic type checking
Currently can lookup type names against a default template1 list of
types, or against the current list in a database (which is read before
processing and sql code).

todo: collect type names from current source file to check against
A lot of the infrastructure to do this is already in place. We also
need to do this for all other definitions, etc.

=== Type aliases

Some types in postgresql have multiple names. I think this is
hardcoded in the pg parser.

For the canonical name in this code, we use the name given in the
postgresql pg_type catalog relvar.

TODO: Change the ast canonical names: where there is a choice, prefer
the sql standard name, where there are multiple sql standard names,
choose the most concise or common one, so the ast will use different
canonical names to postgresql.

planned ast canonical names:
numbers:
int2, int4/integer, int8 -> smallint, int, bigint
numeric, decimal -> numeric
float(1) to float(24), real -> float(24)
float, float(25) to float(53), double precision -> float
serial, serial4 -> int
bigserial, serial8 -> bigint
character varying(n), varchar(n)-> varchar(n)
character(n), char(n) -> char(n)

TODO:

what about PrecTypeName? - need to fix the ast and parser (found out
these are called type modifiers in pg)

also, what can setof be applied to - don't know if it can apply to an
array or setof type

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type. Not sure if this is
handled quite correctly in this code.

=== canonical type name support

Introduce some aliases to protect client code if/when the ast
canonical names are changed:

> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool :: Type
> typeSmallInt = ScalarType "int2"
> typeBigInt = ScalarType "int8"
> typeInt = ScalarType "int4"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeFloat8 = ScalarType "float8"
> typeVarChar = ScalarType "varchar"
> typeChar = ScalarType "char"
> typeBool = ScalarType "bool"

this converts the name of a type to its canonical name

> canonicalizeTypeName :: String -> String
> canonicalizeTypeName s =
>   case () of
>                   _ | s `elem` smallIntNames -> "int2"
>                     | s `elem` intNames -> "int4"
>                     | s `elem` bigIntNames -> "int8"
>                     | s `elem` numericNames -> "numeric"
>                     | s `elem` float4Names -> "float4"
>                     | s `elem` float8Names -> "float8"
>                     | s `elem` varcharNames -> "varchar"
>                     | s `elem` charNames -> "char"
>                     | s `elem` boolNames -> "bool"
>                     | otherwise -> s
>   where
>       smallIntNames = ["int2", "smallint"]
>       intNames = ["int4", "integer", "int"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]

> checkTypeExists :: Scope -> Type -> Either TypeError ()
> checkTypeExists scope t =
>     if t `elem` scopeTypes scope
>       then Right ()
>       else Left (UnknownTypeError t)

> lookupTypeByName :: Scope -> String -> Either TypeError Type
> lookupTypeByName scope name =
>     case lookup name (scopeTypeNames scope) of
>       Just t -> Right t
>       Nothing -> Left (UnknownTypeName name)


================================================================================

Internal errors

TODO: work out monad transformers and try to use these. Want to throw
an internal error when a programming error is detected (instead of
e.g. letting the haskell runtime throw a pattern match failure), then
catch it in the top level type check routines in ast.ag, convert it to
a regular either style error, all without dropping into IO.

This isn't used at the moment.

> data TInternalError = TInternalError String
>                      deriving (Eq, Ord, Show)

> instance Error TInternalError where
>    noMsg  = TInternalError "oh noes!"
>    strMsg = TInternalError

================================================================================

types for the keyword operators, for use in findCallMatch. Not sure
where these should live but probably not here.

> keywordOperatorTypes :: [(String,[Type],Type)]
> keywordOperatorTypes = [
>   ("!and", [typeBool, typeBool], typeBool)
>  ,("!or", [typeBool, typeBool], typeBool)
>  ,("!like", [ScalarType "text", ScalarType "text"], typeBool)
>  ,("!not", [typeBool], typeBool)
>  ,("!isNull", [Pseudo AnyElement], typeBool)
>  ,("!isNotNull", [Pseudo AnyElement], typeBool)
>  ,("!arrayCtor", [Pseudo AnyElement], Pseudo AnyArray) -- not quite right,
>                                         -- needs variadic support
>                                         -- currently works via a special case
>                                         -- in the type checking code
>  ,("!between", [Pseudo AnyElement
>               ,Pseudo AnyElement
>               ,Pseudo AnyElement], Pseudo AnyElement)
>  ,("!substring", [ScalarType "text",typeInt,typeInt], ScalarType "text")
>  ,("!arraySub", [Pseudo AnyArray,typeInt], Pseudo AnyElement)
>  ]

special functions, stuck in here at random also

> specialFunctionTypes :: [(String,[Type],Type)]
> specialFunctionTypes = [
>   ("coalesce", [Pseudo AnyElement],
>     Pseudo AnyElement) -- needs variadic support to be correct,
>                        -- uses special case in type checking
>                        -- currently
>  ,("nullif", [Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement)
>  ,("greatest", [Pseudo AnyElement], Pseudo AnyElement) --variadic, blah
>  ,("least", [Pseudo AnyElement], Pseudo AnyElement) --also
>  ]


================================================================================

utilities for working with Types

> isArrayType :: Type -> Bool
> isArrayType (ArrayType _) = True
> isArrayType _ = False

 > unwrapTypeList :: Type -> [Type]
 > unwrapTypeList (TypeList ts) = ts
 > unwrapTypeList x = error $ "internal error: can't get types from list " ++ show x

> unwrapArray :: Type -> Type
> unwrapArray (ArrayType t) = t
> unwrapArray x = error $ "internal error: can't get types from non array " ++ show x

> unwrapSetOfComposite :: Type -> Type
> unwrapSetOfComposite (SetOfType a@(UnnamedCompositeType _)) = a
> unwrapSetOfComposite x = error $ "internal error: tried to unwrapSetOfComposite on " ++ show x

> unwrapSetOf :: Type -> Type
> unwrapSetOf (SetOfType a) = a
> unwrapSetOf x = error $ "internal error: tried to unwrapSetOf on " ++ show x

> unwrapComposite :: Type -> [(String,Type)]
> unwrapComposite (UnnamedCompositeType a) = a
> unwrapComposite x = error $ "internal error: cannot unwrapComposite on " ++ show x

> consComposite :: (String,Type) -> Type -> Type
> consComposite l (UnnamedCompositeType a) =
>     UnnamedCompositeType (l:a)
> consComposite a b = error $ "internal error: called consComposite on " ++ show (a,b)

> unwrapRowCtor :: Type -> [Type]
> unwrapRowCtor (RowCtor a) = a
> unwrapRowCtor x = error $ "internal error: cannot unwrapRowCtor on " ++ show x

================================================================================

message stuff, used by the continue in loop checking, will be
repurposed once the type checking is complete and lint-style checking
is introduced.

> data Message = Error MessageStuff
>              | Warning MessageStuff
>              | Notice MessageStuff
>                deriving (Eq,Show)
>
> data MessageStuff = ContinueNotInLoop
>                   | CustomMessage String
>                     deriving (Eq,Show)
>

 > instance Show Message where
 >    show = showMessage
 >
 > showMessage :: Message -> String
 > showMessage m = case m of
 >                   Error s -> showit "Error" s
 >                   Warning s -> showit "Warning" s
 >                   Notice s -> showit "Notice" s
 >                 where
 >                   showit lev (fn,l,c) s = lev ++ "\n" ++ fn ++ ":"
 >                                           ++ show l ++ ":" ++ show c ++ ":\n"
 >                                           ++ show s ++ "\n"
 >
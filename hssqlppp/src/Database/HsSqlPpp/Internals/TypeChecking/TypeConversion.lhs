

This file contains the functions for resolving types and
function/operator resolution (which is seriously crazy). See the pg
manual chapter 10:

http://www.postgresql.org/docs/8.4/interactive/typeconv.html

sql server todo: want to match sql server implicit cast rules better
when doing tsql type checking. Follows a completely different approach, possible info here:
http://msdn.microsoft.com/en-us/library/ms187928.aspx
http://msdn.microsoft.com/en-us/library/ms190309.aspx
linked from here:
http://blogs.msdn.com/b/craigfr/archive/2010/01/20/more-on-implicit-conversions.aspx

> {-# LANGUAGE OverloadedStrings, TupleSections, MultiWayIf, LambdaCase, ViewPatterns #-}
> module Database.HsSqlPpp.Internals.TypeChecking.TypeConversion (
>   matchApp
>   ,matchAppExtra
>   ,TypeSetResolutionFlavor(..)
>   ,resolveResultSetType
>   ,resolveResultSetTypeExtra
>   ,joinPrecision
>   ,joinScale
>   ,joinNullability
> ) where
>
> import Control.Monad
> import Control.Applicative
> import Control.Arrow
> import Data.Function
> import Data.Maybe
> import Data.Either
> import Data.List
> import Data.Char
>
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Internals.TypeChecking.OldTediousTypeUtils

> import Database.HsSqlPpp.SqlDialect
> import qualified Database.HsSqlPpp.Internals.TypeChecking.SqlTypeConversion as TSQL
> import Data.Text ()
> import qualified Data.Text as T
> import Text.Printf

******************************************************************

matchApp: takes the function name and argument types, and returns the
matching operator/function

This needs a lot more tests

> matchApp :: SQLSyntaxDialect
>          -> Catalog
>          -> [NameComponent]
>          -> [Type]
>          -> Either [TypeError] ([Type],Type)
> matchApp d cat nmcs = ambiguityResolver $ matchApp' d cat nmcs
>   where
>     -- hack in support for sql server datediff function
>     -- need to think of a better way to handle this when
>     -- have a better idea of all the weird syntax used in
>     -- tsql
>     matchApp' SQLServerDialect _cat [Nmc dd] [_
>                                             ,ScalarType "date"
>                                             ,ScalarType "date"]
>       | map toLower dd == "datediff" =
>       -- check there are 3 args
>       -- first is identifier from list
>       -- other two are date types
>       Right ([typeInt,typeDate,typeDate], typeInt)
>     matchApp' SQLServerDialect _cat [Nmc dd] [_,ScalarType "date"]
>       | map toLower dd == "datepart" =
>       Right ([typeInt,typeDate], typeInt)
>     matchApp' SQLServerDialect _cat [Nmc dd] [_,_,ScalarType "date"]
>       | map toLower dd == "dateadd" =
>       Right ([typeInt,typeInt,typeDate], typeDate)

double hack: support oracle decode when in tsql mode:

>     matchApp' SQLServerDialect cat [Nmc dd] as
>       | map toLower dd == "decode" =

decode is just syntax for simple case statement:
demand at least 3 arguments
get the type of the first argument: this is the test target

>       case as of
>         (tt:as'@(_:_:_)) -> do

for each pair of arguments following: check the first
one can be compared to the test target

collect all the second types
if there is a single trailing argument this is the else

>             let checkBranches [] acc = return $ reverse acc
>                 checkBranches [els] acc = return $ reverse (els:acc)
>                 checkBranches (w:t:xs) acc = do
>                   _ <- matchApp' SQLServerDialect cat [Nmc "="] [tt,w]
>                   checkBranches xs (t:acc)
>             sndTypes <- checkBranches as' []

check the seconds types + the else for type compatilibility
return this type
todo: add the implicit casting where needed

>             (as,) <$> resolveResultSetType cat UnionResolitionFlavor sndTypes


>         _ -> Left [NoMatchingOperator (T.pack dd) as]



>     matchApp' d cat nmcs pts = {-trace ("matchapp: " ++ show (d,nmcs,pts)) $ -} do
>       (_,ps,r,_) <- {-case d of
>                       SQLServerDialect -> TSQL.findCallMatch cat nm pts
>                       _ ->-} findCallMatch cat nm pts
>       return (ps,r)
>       where
>         nm = case last nmcs of
>                Nmc n -> T.pack $ map toLower n
>                QNmc n -> T.pack n
>                AntiNameComponent _ -> -- todo: use left instead of error
>                  error "tried to find function matching an antinamecomponent"

hack to support literal arguments to overloaded functions
  currently, the problem arises only for date/datetime arguments
  the solution reflects this
for long argument lists with several literals, there can be a lot of variants generated, but
  this shouldn't slow down the execution because lazyness composes well with the functions used

> ambiguityResolver:: ([Type] -> Either [TypeError] ([Type],Type))
>                     -> [Type] -> Either [TypeError] ([Type],Type)
> ambiguityResolver f ts =  let rs = map f $ expandList variants ts
>                           -- this is needed in order to preserve the original error
>                           --  in case when all the attempts fail
>                           in msum rs `mplus` head rs
>   where
>     variants t = case t of
>         UnknownType -> [t, ScalarType "timestamp"]
>         _ -> [t]
>     -- similar to handling of superaggregates
>     -- ToDo: move to a general library
>     expandList:: (a -> [a]) -> [a] -> [[a]]
>     expandList f = foldr (liftM2 (:) . f) [[]]

------------- precision and nullability of function application --------------

uses matchApp for inferring basic types

> matchAppExtra:: SQLSyntaxDialect
>                 -> Catalog
>                 -> [NameComponent]
>                 -> [TypeExtra]
>                 -> Either [TypeError] ([TypeExtra],TypeExtra)
> matchAppExtra dialect cat nmcs tes = do
>     (ts',t') <- matchApp dialect cat nmcs $ map teType tes
>     tes' <- joinArgsExtra appName tes $ zipWith addArgExtra tes ts'
>     return (tes', addResultExtra appName tes' t')
>   where
>     addArgExtra te t = te {teType = t}
>     appName = case nmcs of
>       [Nmc dd] -> map toLower dd
>       _ -> ""

precision and nullability of the result

> addResultExtra:: String -> [TypeExtra] -> Type -> TypeExtra
> addResultExtra appName tes t =  checkPrecisionRelevance . checkResultPrecisionClass tesr
>                                 $ TypeExtra t jp js jn
>   where
>     jp = if
>       | appName == "||" -> Just $ sum $ mapMaybe tePrecision tesr
>         -- precision of the result is unknown
>       | appName `elem` ["replace"] -- is actually known for 2-argument "replace"
>         -> Nothing
>       | otherwise
>         -> joinPrecision $ map tePrecision tesr
>     js = joinScale $ map teScale tesr
>     jn = if
>       | appName `elem`
>           ( ["isnotnull","isdate","isnumeric"]
>               -- standard "is null" expression
>             ++ ["isnull" | length tes == 1]
>               -- currently, aggregate functions are handled as scalar functions
>             ++ ["count","count_big"])
>         -> False
>       | appName `elem` 
>           ( ["coalesce","greatest","least"]
>               -- 2-argument function "isnull" of SqlServer
>               -- ImplicitCastToDo: isnull has quite complex cast rules,
>               --    not really reflected here and in the cast of arguments
>             ++ ["isnull" | length tes == 2]
>               -- nullability of corresponding SqlServer function "charindex"
>               --  may or may not differ, depending on database compatibility level
>               -- I implement the level above 70, so it goes to the default case
>             ++ ["strpos","position"])
>         -> all teNullable tesr
>         -- can produce null independently on the nullability of the arguments
>         -- ImplicitCastToDo: check again: doesn't it depend on the presence of "else" part
>       | appName `elem`
>           ( ["case","decode","nullif","substr","substring","replicate"]
>             ++ ["min","max","sum"]
>             ++ ["avg","var_pop","var_samp","variance","stddev_pop","stddev_samp","stddev"]
>             ++ ["covar_pop","covar_samp","corr"])
>         -> True
>         -- the default
>       | otherwise -> joinNullability $ map teNullable tesr
>     -- arguments that participate in the inference of the result type
>     tesr = case appName of
>       "decode" -> caseResultTypes tes
>           -- only the first argument influences precision and nullability
>       _ | appName `elem` ["nullif","substr","substring","left","right","ltrim","rtrim","replicate","translate","like","notlike","rlike"]
>           -> take 1 tes
>           -- the first two arguments influence
>         | appName `elem` ["charindex"]
>           -> take 2 tes
>           -- the first argument doesn't influence
>         | appName `elem` ["datepart","datediff"]
>           -> drop 1 tes
>           -- the first two arguments don't influence
>         | appName `elem` ["dateadd"]
>           -> drop 2 tes
>       -- the default case
>         | otherwise -> tes
>     -- tail is safe here because matchApp did all the checks
>     caseResultTypes tes = caseResultTypes' (tail tes) []
>       where
>         caseResultTypes' [] acc = acc
>         caseResultTypes' [els] acc = els:acc
>         caseResultTypes' (_:t:xs) acc = caseResultTypes' xs (t:acc)

------------- cast of arguments --------------

Bring relevant arguments of a function to common precision and nullability.
The meaning of "relevant" is complicated:
  - the list of arguments is split into partitions;
  - precision and nullability is partition-wise joined;
  - the results are broadcast back to the arguments that constituted partitions.
The algorithm follows this outline.
Examples:
  - for binary operators, like "=", both arguments constitute a single partition;
  - for some functions, like "substr", each argument belongs to its own partition;
  - arguments of "case" and "decode" form two partitions which, except for the analyzed value
    and the 'else' argument, are intervened with one another.
Actually, partitions for precision and partitions for nullability can be different.
Example:
  "||": both arguments must be brought to common nullability, but remain with same precision.
Additionaly:
  Before splitting onto partitions, check each argument for:
    - precision class of the original (before matchApp) argument;
    - precision relevance.
  After splitting onto partitions, check each precision partition:
    - all arguments must have same precision class (return an error if they don't).

> joinArgsExtra:: String -> [TypeExtra] -> [TypeExtra] -> Either [TypeError] [TypeExtra]
> joinArgsExtra appName tes0 tes1
>     = liftM (uncurry $ zipWith3 combine tes) $ uncurry (liftM2 (,))
>       $ (joinDim joinPrec partitionPrec &&& joinDim joinNull partitionNull) tes
>   where
>     -- checks and adjustments before partitioning
>     tes = map checkPrecisionRelevance
>           $ zipWith adjust tes0
>           $ zipWith checkPrecisionClass tes0 tes1
>       where
>         adjust te0 te1 = te1{tePrecision = head $ adjustStringCastPrec (teType te1) [te0]}
>     -- checks after partitioning
>     checkPartition ptes = if length (nub ppcs) > 1
>         then Left [InternalError $ printf "implicit cast: arguments of '%s' that belong to same partition are of different precision classes: %s -> %s" appName (show ptes) (show ppcs)]
>         else return ptes
>       where
>         ppcs = map (precisionClass . teType) ptes
>     -- the algorithm for a single partitioning dimension
>     joinDim:: ([TypeExtra] -> [TypeExtra])
>               -> ([TypeExtra] -> Either [TypeError]
>                                         ([[TypeExtra]] -> [TypeExtra], [[TypeExtra]]))
>               -> [TypeExtra] -> Either [TypeError] [TypeExtra]
>     joinDim join partitionArgs
>         = liftM (uncurry ($) . second (map join)) . partitionArgs
>     -- combine results for precision and nullability
>     combine te tePrec teNull = te {
>       tePrecision = tePrecision tePrec,
>       teScale = teScale tePrec,
>       teNullable = teNullable teNull
>     }
>     -- joins of precision and nullability partitions
>     joinPrec tes = map promote tes
>       where
>         promote (TypeExtra t p s n) = TypeExtra t (p' `mplus` p) (s' `mplus` s) n
>         p' = joinPrecision $ map tePrecision tes
>         s' = joinScale $ map teScale tes
>     joinNull tes = map promote tes
>       where
>         promote (TypeExtra t p s n) = TypeExtra t p s (n' || n)
>         n' = joinNullability $ map teNullable tes
>     -- the partitioning functions return partitions paired with a function
>     --    that puts them back into their places
>     -- because, in many cases, partitions for precision and for nullability are the same,
>     --    the partitioning code for such cases is factored out
>     partitionArgs:: [a] -> ([[a]] -> [a], [[a]])
>     partitionArgs as = case () of
>             -- functions whose arguments are independent
>             --  instead of splitting into partitions, just return the original list
>         _ | appName `elem`
>               ( ["datepart","dateadd"]
>                 ++ ["substr","substring","left","right","ltrim","rtrim"]
>                 ++ ["replicate","like","notlike","rlike"]
>                 ++ ["strpos","position","replace"]
>                     -- Oracle joins the datatypes (needed for the comparison)
>                 ++ ["nullif"]
>               )
>             -> (const as, [])
>             -- first argument is special, the rest are processed together
>           | appName `elem` ["datediff"]
>             -> (concat, pairToList $ splitAt 1 as)
>           | appName `elem` ["decode"]
>             ->  let (ws,ts) = decomposeDecodeTail (tail as) ([],[])
>                 in (composeDecodePartitions, [head as :ws, ts])
>             -- the default is to return a single partition
>           | otherwise -> (concat, [as])
>     partitionPrec as = secondM (mapM checkPartition) $ case () of
>             -- independent arguments
>         _ | appName `elem` ["||","concat","translate","charindex"]
>             -> (const as, [])
>             -- single partition
>           | appName `elem`
>               ( ["coalesce","greatest","least"]
>                     -- ImplicitCastToDo: think how to handle this properly
>                 ++ ["isnull" | length as == 2]
>               )
>             -> (concat, [as])
>           | otherwise -> partitionArgs as
>     partitionNull as = return $ case () of
>         _ | appName `elem`
>               ( ["coalesce","greatest","least"]
>                 ++ ["isnull" | length as == 2]
>               )
>             -> (const as, [])
>           | appName `elem` ["charindex"]
>             -> (concat, pairToList $ splitAt 2 as)
>           | otherwise -> partitionArgs as
>     -- utility
>     pairToList (x,y) = [x,y]
>     decomposeDecodeTail [] acc = (reverse***reverse) acc
>     decomposeDecodeTail [els] acc = (reverse.(els:)***reverse) acc
>     decomposeDecodeTail (w:t:xs) acc = decomposeDecodeTail xs $ ((w:)***(t:)) acc
>     composeDecodePartitions [t:ts,ws] = t : concat (transpose [ts,ws])
>     -- redundant
>     composeDecodePartitions xs = concat xs

------------- precision class --------------

This is a small library for checking whether inference of precision does make sense.
It is used both in inference of precision of arguments and result of a function.

It is theoretically possible that types belong to different precision classes,
    but inference of precision still makes sense (consider, for instance,
    conversion between string and decimal).
  Such cases must be handled specially.

> data PrecisionClass = String | Number | FlexiblePrecisionClass
>       deriving (Eq,Show)
>
> precisionClass:: Type -> Maybe PrecisionClass
> precisionClass (ScalarType tn)
>   | tn `elem` ["text","varchar","char"] = Just String
>   | tn `elem` ["int1","int2","int4","int8","float4","float8","numeric"] = Just Number
>   | otherwise = Nothing
> precisionClass UnknownType = Just FlexiblePrecisionClass
> precisionClass _ = Nothing

Do original and new type have compatible precision classes?
Note: this function is not commutative.

> infix 4 .~>.
> (.~>.):: TypeExtra -> TypeExtra -> Bool
> t0 .~>. t = Just FlexiblePrecisionClass `elem` [pc0,pc] || pc0 == pc
>   where
>     [pc0,pc] = map (precisionClass . teType) [t0,t]

retreat to default when original and new type are incompatible

> checkPrecisionClass:: TypeExtra -> TypeExtra -> TypeExtra
> checkPrecisionClass t0 t = if t0 .~>. t then t else t{tePrecision = Nothing, teScale = Nothing}

check precision class of result against precision classes of arguments

> checkResultPrecisionClass:: [TypeExtra] -> TypeExtra -> TypeExtra
> checkResultPrecisionClass tes t
>   = if and $ map (.~>. t) tes then t else t{tePrecision = Nothing, teScale = Nothing}

check whether precision/scale is relevant for a type (consider "round").

> checkPrecisionRelevance:: TypeExtra -> TypeExtra
> checkPrecisionRelevance te = if
>   | Just String <- pc
>     -> te{teScale = Nothing}
>   | Just FlexiblePrecisionClass <- pc
>     -> te
>   | ScalarType tn <- t, tn == "numeric"
>     -> te
>   | otherwise
>     -> te{tePrecision = Nothing, teScale = Nothing}
>   where
>     t = teType te
>     pc = precisionClass t

******************************************************************

 findCallMatch :: Catalog -> String -> [Type] ->  Either [TypeError] OperatorPrototype
 findCallMatch cat fnName' argsType =

code interspersed with text cut and pasted from postgresql manual
10.3. Functions

Function Type Resolution

Select the functions to be considered from the pg_proc system
catalog. If a non-schema-qualified function name was used, the
functions considered are those with the matching name and argument
count that are visible in the current search path (see Section
5.7.3). If a qualified function name was given, only functions in the
specified schema are considered.

[HsSqlPpp doesn't support schema stuff yet, so just get a list of all
the functions with a matching name]

>   {-let matchingNames = catGetOpsMatchingName cat nmcs
>       exactMatches = filter (\(_,ts,_,_) -> ts == pts) matchingNames
>   case exactMatches of
>     [(_,tys,rt,_)] -> return (tys, rt)
>     [] -> error $ "no matching fn: " ++ show nmcs
>                   ++ "(" ++ intercalate "," (map show pts) ++ ")"
>     _xs -> error "ambiguous"-}



If the search path finds multiple functions of identical argument
types, only the one appearing earliest in the path is
considered. Functions of different argument types are considered on an
equal footing regardless of search path position.

If a function is declared with a VARIADIC array parameter, and the
call does not use the VARIADIC keyword, then the function is treated
as if the array parameter were replaced by one or more occurrences of
its element type, as needed to match the call. After such expansion
the function might have effective argument types identical to some
non-variadic function. In that case the function appearing earlier in
the search path is used, or if the two functions are in the same
schema, the non-variadic one is preferred.

Functions that have default values for parameters are considered to
match any call that omits zero or more of the defaultable parameter
positions. If more than one such function matches a call, the one
appearing earliest in the search path is used. If there are two or
more such functions in the same schema with identical parameter types
in the non-defaulted positions (which is possible if they have
different sets of defaultable parameters), the system will not be able
to determine which to prefer, and so an "ambiguous function call"
error will result if no better match to the call can be found.

Check for a function accepting exactly the input argument types. If
one exists (there can be only one exact match in the set of functions
considered), use it. (Cases involving unknown will never find a match
at this step.)

If no exact match is found, see if the function call appears to be a
special type conversion request. This happens if the function call has
just one argument and the function name is the same as the (internal)
name of some data type. Furthermore, the function argument must be
either an unknown-type literal, or a type that is binary-coercible to
the named data type, or a type that could be converted to the named
data type by applying that type's I/O functions (that is, the
conversion is either to or from one of the standard string
types). When these conditions are met, the function call is treated as
a form of CAST specification. [1]

Look for the best match.

Discard candidate functions for which the input types do not match and
cannot be converted (using an implicit conversion) to match. unknown
literals are assumed to be convertible to anything for this
purpose. If only one candidate remains, use it; else continue to the
next step.

Run through all candidates and keep those with the most exact matches
on input types. (Domains are considered the same as their base type
for this purpose.) Keep all candidates if none have exact matches. If
only one candidate remains, use it; else continue to the next step.

Run through all candidates and keep those that accept preferred types
(of the input data type's type category) at the most positions where
type conversion will be required. Keep all candidates if none accept
preferred types. If only one candidate remains, use it; else continue
to the next step.

If any input arguments are unknown, check the type categories accepted
at those argument positions by the remaining candidates. At each
position, select the string category if any candidate accepts that
category. (This bias towards string is appropriate since an
unknown-type literal looks like a string.) Otherwise, if all the
remaining candidates accept the same type category, select that
category; otherwise fail because the correct choice cannot be deduced
without more clues. Now discard candidates that do not accept the
selected type category. Furthermore, if any candidate accepts a
preferred type in that category, discard candidates that accept
non-preferred types for that argument.

If only one candidate remains, use it. If no candidate or more than
one candidate remains, then fail.




exact match
binop1unknownmatch
polymorphic matches
reachable
mostexactmatches
filteredforpreferred
unknownmatchesbycat



TODO: do a log monad, which can record the tests and then return this
process along with the resolved function


--------------------------------
todo:

> {-resolveResultSetType :: Catalog -> [Type] -> Either [TypeError] Type
> resolveResultSetType _cat [] = error "resolveResultSetType: empty type set"
> resolveResultSetType _cat (t:ts) =
>   if all (==t) ts
>   then Right t
>   else Left [IncompatibleTypeSet (t:ts)]-}

todo:

assignmentCheck

************* overloaded implicit cast / function call resolution **************

The approach is as follows:
  - generate initial list of candidate type sets;
  - expand (on the basis of type templates) and filter (on the basis of possibility of
    implicit cast) each type set;
  - resolve the obtained set of type sets.

Common pieces of behaviour are factored out into separate (groups of) functions, and details
  of their behaviour are described there.

A couple of things of note:
1. Expressions like:
          xvarchar6 + xint
      are resolved to:
          cast(xvarchar6 as float8) + cast(xint as float8).
    This is obtained automatically because there is a type category change on one
      of the arguments, and the preferred type of the target category is picked in such cases.
    This is different from MsSql behaviour, which resolves this to:
          cast(xvarchar6 as int) + xint.
    Then, if the varchar column contains a string like '1.5', SqlServer will fail, while
      we will return the correct value.
    If the column contains only integers, we will return the same result as SqlServer. Only
      the data type will differ.
    In my view, Postgre catalog is more sane in this respect.
2. UnknownType represents a significant problem. I tried to do my best, but had to step back
      in some cases, because, say, string literals and null literals that are hidden under
      UnknownType are undistinguishable. So, same expression with a literal in place of
      xvarchar6 is resolved exactly as at MsSql. Are you guys happy?
    In my view... well, just read comment 7 to #512.

Because I wrote components of this file in the reversed order (result extra -> args extra
    -> basic (not extra) result and args), the overall organization is not the best one, and
    same ideas are repeated throught the code.
So, this file requires further refactoring. Maybe, a wise thing to do is to postpone this
    refactoring, and later to combine it with a new catalog, which should include
    the information about partitioning of arguments of functions.

------------- API --------------

> findCallMatch:: Catalog -> T.Text -> [Type] ->  Either [TypeError] OperatorPrototype
> findCallMatch cat fn args = do
>     when (null initialCandidates) $ Left [NoOperator fn]
>     candidates <- expandTypeSetResolutionCandidates cat getCandArgs updateCand args
>                                                     initialCandidates
>     maybe (Left [NoMatchingOperator fn args]) return
>           $ resolveCastSets cat (T.unpack fn) getCandArgs args candidates
>   where
>     getCandArgs (_,candArgs,_,_) = candArgs
>     updateCand (args,f) (fn,_,r,v) = (fn, args, f r, v)
>     -- generate list of initial candidates, including hacks
>     initialCandidates = filter argListLenFilter $ if
>       | fn `elem` ["between" | length args == 3] ++ ["greatest","least"]
>         -> let  as = nub $ map (\(_,as,_,_) -> head as) $ catLookupFns cat "<="
>                 (r,v) = if fn == "between"
>                         then (const (ScalarType "bool"), False)
>                         else (id, True)
>            in map (\a -> (fn, replicate (length args) a, r a, v)) as
>       | fn == "=" -- because of our restrictions for equijoins
>         -> filter (\(_,candArgs,_,_) -> length (nub candArgs) == 1) $ catLookupFns cat fn
>       | otherwise
>         -> map expandVariadic $ catLookupFns cat fn
>     expandVariadic (fn,as,r,True)
>       | (not . null) as, ArrayType t <- last as
>       , let as' = init as ++ replicate (length args - length as + 1) t
>         = (fn,as',r,True)
>     expandVariadic x = x
>     argListLenFilter (_,candArgs,_,_) = length candArgs == length args

This is treated as a special case of findCallMatch.
There is a single template type set that is generated right here instead of being taken
  from the catalog.
The function also accept an additional parameter of resolution flavor, which is used to
  distinguish between the two possible resolution semantics: union and comparison.
  This parameter is converted to a name of a pseudo app and later used during the resolution.
Note that in order to abide to the rules of MsSql, you have to use ComparisonResolutionFlavor
  in all places.

> data TypeSetResolutionFlavor = UnionResolitionFlavor | ComparisonResolutionFlavor

> resolveResultSetType::  Catalog -> TypeSetResolutionFlavor
>                         -> [Type] -> Either [TypeError] Type
> resolveResultSetType cat flav ts = do
>     when (null ts) $ Left [TypelessEmptyArray]
>     let candTemplates = replicate (length ts) $ Pseudo AnyElement
>     candss <- expandTypeSetResolutionCandidates cat id (const . fst) ts [candTemplates]
>     maybe (Left [IncompatibleTypeSet ts]) (return . head)
>           $ resolveCastSets cat fn id ts candss
>   where
>     fn = case flav of
>         UnionResolitionFlavor -> "|union|"
>         ComparisonResolutionFlavor -> "|compare|"

------------- expansion/filtering of type templates --------------

Given input type set and a list of candidate type sets, produce expanded list of candidates.
The expansions / filters account for:
  - target templates (Pseudo types);
  - input templates (UnknownType);
  - regular target types.

Target templates are:
  1. For each input type:
    - expanded to the list of all types to which the input type is implicitly castable
      (except the case when the input type is a template, which is handled separately)
    - filtered according to the template type (AnyArray, AnyNonArray)
  2. For group of types whose target template is the same:
    - combined together in such a way that they all are same type per type set.
Regular target types are only checked for being implicitly castable to from the input type.
If an input type is UnknownType, nothing is expanded. The target type is Left as is, but this
  Left marker later influences combining groups of types.

Groups in a type set are handled similarly to partitions of arguments in joinArgsExtra.
But here, the criterion is equality of target types. This way, same Pseudo types go
  to their own group.
One drawback is that same regular target types also go to the same group, but they must be
  combined differently from Pseudo groups. Instead of bothering with more sophisticated
  splitting into groups, I pass the group type to the function that combines groups.

If we deal with an overloaded function, its return type can also be expressed with a type
  template. So, I accompany each expansion item with a function (Type -> Type), which is later
  used for synchronizing the result type with the expansion.


> -- type set and result type transformation function
> type ExpansionResult = ([Type], Type -> Type)

> expandTypeSetResolutionCandidates:: (Eq r,Show r) => Catalog
>                                     -> (r -> [Type])
>                                     -> (ExpansionResult -> r -> r)
>                                     -> [Type]
>                                     -> [r]
>                                     -> Either [TypeError] [r]
> expandTypeSetResolutionCandidates cat getCandArgs updateCand args
>   = liftM (nub . concat) . mapM processCandidate
>   where
>     processCandidate r =  liftM (map (`updateCand` r))
>                           $ expandTypeSetResolutionCandidate cat args $ getCandArgs r

expand a single candidate type set

> expandTypeSetResolutionCandidate::  Catalog -> [Type]
>                                     -> [Type] -> Either [TypeError] [ExpansionResult]
> expandTypeSetResolutionCandidate cat args cands
>   = liftM (map (first permuteBack) . combineCandidateGroups)
>     $ mapM (uncurry expandGroup . unzip) groups
>   where
>     -- split into groups
>     (as,cs,perm) =  unzip3 $ sortBy (compare `on` snd3) $ zip3 args cands [0..]
>     groups = groupBy ((==) `on` snd) $ zip as cs
>     permuteBack = snd . unzip . sortBy (compare `on` fst) . zip perm
>     -- combine the two core group functions below
>     expandGroup:: [Type] -> [Type] -> Either [TypeError] [ExpansionResult]
>     expandGroup gas gcs = liftM (combineCandidateGroup (head gcs))
>                           $ zipWithM (typeResolutionCandidates cat) gas gcs

expand/filter a single candidate type
the inner Either is a selector between input type template and the rest

> typeResolutionCandidates::  Catalog -> Type -> Type -> Either [TypeError] [Either Type Type]
> typeResolutionCandidates cat t t' = if
>     -- input template will be accounted for later
>   | t == UnknownType
>     -> return $ [Left t']
>     -- target template is expanded and preliminarily filtered
>   | Just template <- typeTemplate t'
>     -> liftM (map Right . filter (templateFilter template)) candidates
>     -- a regular target is checked for being castable to
>   | otherwise
>     -> return [Right t' | isCastableFromTo cat t t']
>   where
>     typeTemplate = \case
>         Pseudo pt -> find (pt==) [Any,AnyElement,AnyArray,AnyNonArray]
>         _ -> Nothing
>     templateFilter = \case
>         AnyArray -> isArrayType
>         AnyNonArray -> not . isArrayType
>         _ -> const True
>     candidates = liftM nub $ do
>         ts <- catCastsFrom cat ImplicitCastContext t
>         bt <- catBaseType cat t
>         return $ t:bt:ts

Combine the expansions in a group that were produced by the previous function.

Note that the output representation is transposed w.r.t. the input one:
    in input, we have a list of types for each original type;
    in output, the inner list is a candidate type resolution for all original types in a group.
  Thus, the output is ready for cartesian product with other groups.
This function completes expansion of target template types.
Input template type (UnknownType) will be used during the actual resolution.

> combineCandidateGroup:: Type -> [[Either Type Type]] -> [ExpansionResult]
> combineCandidateGroup groupType tss
>   = if groupType `elem` map Pseudo [AnyElement,AnyArray,AnyNonArray]
>     -- intersect for same-element template expansions
>     then  map ((replicate (length tss) &&& expandTemplate) . replaceInputTemplate)
>           $ foldl1 intersectTypes tss
>     -- cartesian product for the rest
>     else map ((,id) . map replaceInputTemplate) $ foldr (liftM2 (:)) [[]] tss
>   where
>     intersectTypes [Left _] ts = ts
>     intersectTypes ts [Left _] = ts
>     intersectTypes ts1 ts2 = intersect ts1 ts2
>     -- replace remaining input templates
>     replaceInputTemplate = \case
>       Left (Pseudo{}) -> UnknownType --ScalarType "text"
>       t -> either id id t
>     -- intented for same-element template types
>     expandTemplate:: Type -> Type -> Type
>     expandTemplate r t = if t == groupType then r else t

Groups are combined with cartesian product over concatenation of argument type lists
  and composition of result transformation functions.

> combineCandidateGroups:: [[ExpansionResult]] -> [ExpansionResult]
> combineCandidateGroups = foldr  (liftM2 $ curry $ uncurry ((++) `on` fst)
>                                                   &&& uncurry ((.) `on` snd))
>                                 [([],id)]

------------- resolution core --------------

The [Type] argument is the list of original types.
The [r] argument is the list of candidates. A list of candidate types can be extracted from
  each candidate.

The outline of the algorithm is:
  1. Define a set of functions [Type] -> [Type] -> Ordering. Each function compares two lists
      of candidate types according to some criteria. The result of comparison is GT if the
      first list is preferred than the second for being choosed as the resolution result.
  2. Define an order of application of these functions.
  3. Sort and then group the list of candidate according to the list of comparison functions.
  4. Take the group with the highest rank. If it consists of a single candidate, the resolution
      is unambiguous, and this candidate is returned.

The order of comparison functions is defined in typeSetsComparisons.
The meaning of individual comparison functions is either clear from their names, or desribed
  in comments to the functions.
A few general notes on the set of functions:
  1. The first two functions (allSameCategory and compareByAppPreferences) deal with complex
      cases like (xvarchar6 + xint), when the app can process both varchars and ints, and it is
      unclear to which side to cast.
  2. The last four functions are rough comparisons that remained from my first attempts.
      They are the most questionable part of this whole module and should be revisited.
      But they still can become valid when partitions of app arguments are introduced.
    I call them "rough" because, for each pair of lists of candidate types, they don't compare
      pairs of corresponding types. Instead, they analyse each type independently, and return
      a boolean value. Then, they count the number of True values, independently for each list.
      After obtaining these integers, they compare them, which constitutes the only interaction
      between the lists of candidate types.
    In the comments inside the code, I call these functions "length-based".
  3. Because these four functions are rough, two more functions were introduced before them,
      to make the grouping more distinctive.

ToDo:
  Consider usage of Control.Monad.Cont in this module.

> resolveCastSets:: (Show r) => Catalog -> String -> (r -> [Type]) -> [Type] -> [r] -> Maybe r
> resolveCastSets cat fn getCandArgs ts
>   = (\rs -> guard (length rs == 1) >> listToMaybe rs) -- check for non-ambiguity
>     <=< listToMaybe -- take the highest scorer
>         . groupSortedBy (curry compareTypeSets `on` getCandArgs)
>   where
>     -- EQs are 1st argument of compare in order to get Desc order
>     compareTypeSets = (replicate (length typeSetsComparisons) EQ `compare`)
>                       . (`map` typeSetsComparisons) . flip ($)
>     -- order of length-based checks of a single type
>     typeChecks =  [isSameCategory
>                   ,isOriginal
>                   ,\t t' -> liftM2 (&&) (isSameCategory t t') (isPreferred t t')
>                   ,isPreferred]
>     -- order of checks of a type set
>     typeSetChecks:: [[Type] -> Int]
>     typeSetChecks = let f = length . filter (== Right True)
>                     in map ((f.) . ($ ts) . zipWith) typeChecks
>     -- order of comparisons of two type sets
>     typeSetsComparisons:: [([Type],[Type]) -> Ordering]
>     typeSetsComparisons = [allSameCategory
>                           ,uncurry $ compareByAppPreferences cat fn
>                           ,uncurry $ compareByCategoryChange cat ts
>                           ,uncurry $ compareByCastMinimality ts]
>                           ++ basicCmps
>       where
>         basicCmps = map (\f -> uncurry compare . both f) typeSetChecks
>     -- single-type checks for length-based checks; all return Either ... Bool
>     isOriginal t t' = return $  t == t' --t .==>. t'
>     isSameCategory UnknownType _ = return True
>     isSameCategory t t' = (liftM2 (==) `on` catTypeCategory cat) t t'
>     isPreferred = const $ catPreferredType cat
>     -- one integral comparison is defined right here, the others are top level
>     allSameCategory:: ([Type],[Type]) -> Ordering
>     allSameCategory = uncurry compare . both (all (== Right True) . zipWith isSameCategory ts)

ToDo: Factor out repetitive patterns in the following functions. I don't have time already.

If an overloaded function, which has sets of arguments of different type categories, but all
  types in a particular set are of same type category, is given arguments of different
  categories, it can prefer one or another type category for the implicit cast.
This function makes the choice.
The precondition is supplied by the previous check (allSameCategory).
  Which is actually not very good.
  In general, apps should define their preferences independently over partitions of arguments
    (see joinArgsExtra). And many other things in this whole module should be done
    in the partition-wise manner.
  But let's see how it works in this form.

> compareByAppPreferences:: Catalog -> String -> [Type] -> [Type] -> Ordering
> compareByAppPreferences cat fn ts1 ts2 =  fromMaybe EQ $ vectorPartialOrder
>                                           $ zipWith compareTypes ts1 ts2
>   where
>     prefs = appPreferences ["S"] fn
>     compareTypes:: Type -> Type -> Maybe Ordering
>     compareTypes t1 t2
>       | UnknownType `elem` [t1,t2] = return EQ
>       | otherwise = do
>           let [c1,c2] = map (either (const "???") T.unpack . catTypeCategory cat) [t1,t2]
>           guard $ any ("???"/=) [c1,c2]
>           if c1==c2
>             then return EQ
>             else do
>               let idxs = map (`elemIndex` prefs) [c1,c2]
>               guard $ not . null $ catMaybes idxs
>               let [i1,i2] = map (fromMaybe $ length prefs) idxs
>               return $ i2 `compare` i1
> appPreferences:: [String] -> String -> [String]
> appPreferences dflt = fromMaybe dflt . (`lookup` cat)
>   where
>     cat = map (,["N","D","S"])
>               ["|compare|","+","=","<>","<","<=",">",">=","greatest","least","between"]
>           -- actually redundant because of the default rules
>           ++ map (,["S"])
>               ["|union|","nullif","coalesce"]

in cases when there is a type category change, change to a preferred type is preferred

> compareByCategoryChange:: Catalog -> [Type] -> [Type] -> [Type] -> Ordering
> compareByCategoryChange cat ts ts1 ts2 =  fromMaybe EQ $ vectorPartialOrder
>                                           $ zipWith3 compareCatChanges ts ts1 ts2
>   where
>     compareCatChanges:: Type -> Type -> Type -> Maybe Ordering
>     compareCatChanges _ t1 t2 | t1 == t2 = return EQ
>     compareCatChanges _ UnknownType _ = return GT
>     compareCatChanges _ _ UnknownType = return LT
>     compareCatChanges UnknownType _ _ = return EQ
>     compareCatChanges t t1 t2 = if
>       | c/=c1 && c1==c2 -> return $ p1 `compare` p2
>       | c==c1 && c==c2 -> return EQ
>       | otherwise -> Nothing
>       where
>         [c1,c2] = map (either (const "->?") T.unpack . catTypeCategory cat) [t1,t2]
>         c = either (const "?->") T.unpack $ catTypeCategory cat t
>         [p1,p2] = map (either (const False) id . catPreferredType cat) [t1,t2]

This makes sure that the smallest possible type from a line of same-nature types is picked up
  whenever possible.
Before this was added, small integer types were cast to the preferred type (float8)
  in some cases, because of the subsequent (length-based) comparisons.

> compareByCastMinimality:: [Type] -> [Type] -> [Type] -> Ordering
> compareByCastMinimality ts ts1 ts2 =  fromMaybe EQ $ vectorPartialOrder
>                                       $ zipWith3 compareCasts ts ts1 ts2
>   where
>     compareCasts:: Type -> Type -> Type -> Maybe Ordering
>     compareCasts _ UnknownType UnknownType = return EQ
>     compareCasts _ UnknownType _ = return GT
>     compareCasts _ _ UnknownType = return LT
>     -- we use defaults for literals
>     compareCasts UnknownType t1 t2
>       = msum [compareCasts (ScalarType tn) t1 t2 | tn <- ["int4","varchar"]] --Nothing--Just EQ
>     -- the general case
>     compareCasts t t1 t2 = if t==t1 && t1==t2 then return EQ else do
>         ts <- find (t `elem`) cat
>         let ts' = dropWhile (/=t) ts
>             idxs = map (`elemIndex` ts') [t1,t2]
>         guard $ not . null $ catMaybes idxs
>         let [i1,i2] = map (fromMaybe $ length ts') idxs
>         return $ i2 `compare` i1

Cast of integer types to float4 leads to loss of precision, and so is supressed.
This leads to a kind of unelegancy: float8 occurs in two sublists. But:
  - when float8 is the original type, it is irrelevant (it is always the last item);
  - otherwise, the algorithm picks a list according to another original type, and operates
    in it.
If problems appear, make this a lookup table in which every type has its own list of cast
  priorities. This is also cleaner.

>     cat = map (map ScalarType)  [["int1","int2","int4","int8","numeric","float8"]
>                                 ,["float4","float8"]
>                                 ,["date","timestamp"] -- not really needed (no choice)
>                                 ,["char","varchar","text"]]

------------- utils --------------

sorry guys, this operator is almost a duplicate of (.~>.)
This is further evidence that all this requires refactoring. But it is hardly related to this
  particular file: types and precision clases are not the same. It is UnknownType who causes
  problems.
almost unused after all

> infix 4 .==>.
> (.==>.):: Type -> Type -> Bool
> t .==>. t' = t `elem` [t',UnknownType]

> isCastableFromTo:: Catalog -> Type -> Type -> Bool
> isCastableFromTo cat t t' = t .==>. t' || catCast cat ImplicitCastContext t t' == Right True

************* precision and nullability **************

ToDo: combine this function with matchAppExtra, like it is done for
        resolveResultSetType / findCallMatch

> resolveResultSetTypeExtra:: Catalog -> TypeSetResolutionFlavor
>                             -> [TypeExtra] -> Either [TypeError] TypeExtra
> resolveResultSetTypeExtra cat flav args
>   = liftM addPrecAndNull $ resolveResultSetType cat flav $ map teType args
>   where
>     addPrecAndNull t = if null args
>       then mkTypeExtraNN t
>       else TypeExtra t (prec t) scale nullability
>     nullability = joinNullability $ map teNullable args
>     prec t = joinPrecision $ adjustStringCastPrec t args
>     scale = joinScale $ map teScale args

SqlServer precisions for casts to strings

> adjustStringCastPrec:: Type -> [TypeExtra] -> [Maybe Int]
> adjustStringCastPrec tTo = map $ uncurry adjust . (teType&&&tePrecision)
>   where
>     stringTypes = map ScalarType ["char","varchar","text"]
>     adjust tFrom precFrom = msum  [guard (tTo `elem` stringTypes) >> lookup tFrom typePrecs
>                                   -- if there will be problems with literals, add here
>                                   -- a treatment for UnknownType
>                                   ,precFrom]
>     typePrecs = map (first ScalarType)  [("bool",1)
>                                         ,("int1",4), ("int2",6), ("int4",12), ("int8",24)
>                                         ,("float4",23), ("float8",23)
>                                         ,("date",40), ("timestamp",40)]

join (in Order Theory terms) of precision, scale, and nullability

> joinNullability:: [Bool] -> Bool
> joinNullability = or
> -- questionable logic; to be revisited
> joinPrecision:: [Maybe Int] -> Maybe Int
> joinPrecision ps = if null ps' then Nothing else Just $ maximum ps'
>   where
>     ps' = catMaybes ps
> -- same thing for now
> joinScale:: [Maybe Int] -> Maybe Int
> joinScale = joinPrecision


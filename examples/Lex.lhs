
> import Database.HsSqlPpp.Lex
> import Database.HsSqlPpp.Dialect
> import Text.Show.Pretty
> import System.Environment
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- LT.readFile s
>   putStrLn $ ppShow $ lexTokens postgresDialect "" (Just (1,0)) f
>   --putStrLn $ ppShow $ parseProcSQL "" s

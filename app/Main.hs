import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List

data Flag = Help      -- --Help
            deriving (Eq, Ord, Enum, Show, Bounded)

flags =
    [Option []    ["help"] (NoArg Help)     "Print this help message"]

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        let files = if null fs then ["-"] else fs
        -- Check if either help flag passed, or no flags passed
        if or [Help `elem` args, args == []]
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else return (nub (concatMap set args), files)

    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: "
          set f  = [f]

main = do
    (args, files) <- getArgs >>= parse
    print args
    print files

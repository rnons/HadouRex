import System.Environment
import GHC.IO.Exception
import Douban.FM
import Douban.Search

main = do
    --(command:argList) <- getArgs
    --dispatch command argList
    command <- getArgs
    if command /= [] then do
        (command:argList) <- getArgs
        dispatch command argList
        else select

dispatch :: String -> [String] -> IO GHC.IO.Exception.ExitCode
dispatch "search" = search
dispatch "hot" = hot
dispatch "trending" = trending
--dispatch "login" = login
--dispatch "" = select
--dispatch ("listen":[]) = select
dispatch "listen" = listen
dispatch x = help

help arg = do
    msg <- readFile "commands"
    putStrLn msg
    --TODO: hide "ExitSuccess" msg on the last line
    return GHC.IO.Exception.ExitSuccess

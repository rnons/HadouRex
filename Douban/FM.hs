{-# LANGUAGE DeriveDataTypeable #-}
module Douban.FM  where

import Network.URI
import Network.HTTP
import Network.Browser
import Network.HTTP.Cookie
import Text.JSON
import Text.JSON.Generic
import System.Cmd
import System.IO
import GHC.IO.Exception
import Codec.Binary.UTF8.String (decodeString)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.List
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Console.ANSI
import Douban.State
import Douban.Search
import System.Exit
import Douban.Utils

-- there are two types of json response: song & ad.
-- the 'company', 'rating_avg', 'public_time', 'ssid' fields are missing in ad_json
-- the 'like' and 'length' fields are of String type in ad_json
data Song = Song {
    picture :: String,
    albumtitle :: String,
    --company :: String,
    --rating_avg :: Float,
    --public_time :: String,
    ssid :: String,
    album :: String,
    like :: Int,
    artist :: String,
    url :: String,
    title :: String,
    subtype :: String,
    --length :: Int,
    sid :: String,
    aid :: String
} deriving (Eq, Show, Data, Typeable)

data FavChannel = FavChannel {
     fav_id    ::  String
    ,fav_name   ::  String
} deriving (Eq, Show, Data, Typeable)
    
    

-- filter out ads :-)
parseSong json = do
    let decoded = decode json :: Result (JSObject JSValue)
    let value = decoded >>= valFromObj "song" :: Result [JSObject JSValue]
    let songs = (\(Ok x) -> x) value
    --let folding = foldr (\x acc -> if 
    let filterSong s = do
        let ssid = valFromObj "ssid" s :: Result JSValue
        case ssid of
             Ok _ -> True
             _  -> False
    let ss = filter filterSong songs
    decodeJSON $ decodeString $ encode ss :: [Song]

getPlaylist t = do
    ch_id <- getsST st_ch_id
    let rdata = [("type", t),
                 ("channel", ch_id),
                 ("from", "HadouRex")
                ]
    let url = "http://douban.fm/j/mine/playlist?" ++ urlEncodeVars rdata
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    let songs = parseSong json
    handle ((\_ -> onError)::SomeException -> IO ()) (play songs)

play :: [Song] -> IO ()
play [] = getPlaylist "n"
play (x:xs) = do
    setSGR [SetConsoleIntensity BoldIntensity,
            SetColor Foreground Vivid Green]
    putStrLn ""
    putStr $ artist x ++ " - " ++ title x
    setSGR [Reset]
    putStrLn ""
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hPutStrLn hin $ "LOAD " ++ url x
        hFlush hin
    silentlyModifyST $ \st -> st { st_s_picture     =   picture x,
                                   st_s_albumtitle  =   albumtitle x,
                                   st_s_album       =   album x,
                                   st_s_artist      =   artist x,
                                   st_s_title       =   title x,
                                   st_s_sid         =   sid x,
                                   st_s_ssid        =   ssid x,
                                   st_s_aid         =   aid x
                                 }
    liftIO $ withST $ \st -> do
        hin <- readMVar (writeh st)
        hout <- readMVar (readh st)
        mpg123wait hin hout
    play xs
    where 
        mpg123wait hin hout = do
            line <- hGetLine hout
            case line of
                 "@P 0" -> do
                     --reportEnd
                     return ExitSuccess
                 _ -> mpg123wait hin hout

onError = do
    putStrLn "!!! Wrong channel_id !!!"
    selectChannel

select = do
    --putStrLn "Inexhaustive channel lists:"
    --chs <- readFile "channels"
    --putStrLn chs
    pprMarks
    selectChannel

listen [] = select
listen (cid:xs) = do
    putStr "Current channel is: "
    cname <- channelName cid 
    putStrLn cname
    silentlyModifyST $ \st -> st { st_ch_id = cid, st_ch_name = cname }
    forkIO $ getPlaylist "n"
    return ()

selectChannel = do
    putStrLn "Please enter the channel_id you want to listen to:"
    inpStr <- getLine
    -- default channel: "1001294 PostRock Odyssey"
    let channel_id = if inpStr `elem` ["","-3"] then "1001294" else inpStr
    listen [channel_id]

channelName cid = do
    name <- appChannelName cid
    case name of
        Just aname -> return aname
        Nothing -> searchChannelName cid

appChannelName cid = do
    let app_url = "http://www.douban.com/j/app/radio/channels"
    rsp <- simpleHTTP $ getRequest app_url
    json <- getResponseBody rsp
    let decoded = decode json :: Result (JSObject JSValue)
        value = decoded >>= valFromObj "channels" :: Result [JSObject JSValue]
        channels = (\(Ok x) -> x) value
    --let okc = head $ filter (\c -> cidMatch cid c) channels
    let okc = filter (cidMatch cid) channels
    case okc of
         [] -> return Nothing
         _ -> do
             let okname = valFromObj "name" (head okc) :: Result JSValue
             let jsname = (\(Ok x) -> x) okname
                 aokname = readJSON jsname :: Result JSString
                 name = decodeString $ fromJSString $ (\(Ok x) -> x) aokname
             return $ Just name 
    
cidMatch cid channel = do
    let ok_cid = valFromObj "channel_id" channel :: Result JSValue
        channel_id = (\(Ok x ) -> x) ok_cid
        okc = readJSON channel_id :: Result Int
        c = (\(Ok x) -> x) okc
    show c == cid

searchChannelName cid = do
    let rdata = [
                 ("channel", cid)
                ]
    let url = "http://douban.fm/j/explore/search?query=" ++ urlEncodeVars rdata 
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    let chs = parseChannel json
    return $ name $ head chs

login _ = do  
    putStr "Username: "
    username <- getLine
    password <- getPassword
    let fdata = [("form_email", username), ("form_password", password)]
    rsp <- simpleHTTP (postRequestWithBody "http://www.douban.com/accounts/login" "application/x-www-form-urlencoded" $ urlEncodeVars fdata)
    let headers = rspHeaders $ (\(Right r) -> r) rsp
    print headers
    let (err, cookies) = processCookieHeaders "" headers
        dbcl2 = ckValue $ last cookies
        --bid = ckValue $ (cookies !! 1)
    headers <- newMVar (cookiesToHeader cookies)
    --silentlyModifyST $ \st -> st { headers = headers, dbcl2 = dbcl2, bid = bid }
    print dbcl2
    --print bid
    select
    --return ()

getPassword :: IO String
getPassword = do
    putStr "Password: "
    hFlush stdout
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

bookmarks = "bookmarks.json"

mark ("current":xs) = do
    ch_id <- getsST st_ch_id
    ch_name <- getsST st_ch_name
    markCore ch_id ch_name
mark (ch_id:xs) = do
    ch_name <- channelName ch_id
    markCore ch_id ch_name
    shutdown
mark _ = do
    showHelp []
    shutdown

markCore ch_id ch_name = do
    let ch = FavChannel { fav_id = ch_id, fav_name = ch_name }
    json <- readFile bookmarks
    case json of
         "" -> do
             let newjson = encodeJSON [ch]
             writeFile bookmarks newjson
             putStrLn "Marked"
         _ -> do
             let fav_ch = decodeJSON json :: [FavChannel]
             if ch `elem` fav_ch then putStrLn "Already marked!"
                    else do
                        let newjson = encodeJSON $ ch:fav_ch
                        writeFile bookmarks newjson
                        putStrLn "Marked"
    --silentlyModifyST $ \st -> st { st_ch_marked = True }

unmark ("current":xs) = do
    ch_id <- getsST st_ch_id
    ch_name <- getsST st_ch_name
    unmarkCore ch_id ch_name
unmark (ch_id:xs) = do
    ch_name <- channelName ch_id
    unmarkCore ch_id ch_name
    shutdown
unmark _ = do
    showHelp []
    shutdown

unmarkCore ch_id ch_name = do
    let ch = FavChannel { fav_id = ch_id, fav_name = ch_name }
    json <- readFile bookmarks
    case json of
         "" -> do
             let newjson = encodeJSON ch
             writeFile bookmarks newjson
             putStrLn "Not marked!"
         _ -> do
             let fav_ch = decodeJSON json :: [FavChannel]
             if ch `notElem` fav_ch then putStrLn "Not marked!"
                    else do
                        let newjson = encodeJSON $ delete ch fav_ch
                        writeFile bookmarks newjson
                        putStrLn "Unmarked"
    --silentlyModifyST $ \st -> st { st_ch_marked = False }

marks _ = do
    pprMarks
    shutdown

pprMarks = do
    json <- readFile bookmarks
    case json of
         "" -> do
             putStrLn "You have no channel in your bookmark."
             putStrLn "Give a try to `dourex hot` or `dourex trending`."
         _ -> do
             putStrLn "Bookmarked channels:"
             let fav_ch = decodeJSON json :: [FavChannel]
             forM_ fav_ch (\c -> do
                setSGR [SetConsoleIntensity BoldIntensity]
                putStr $ "* " ++ fav_name c 
                setSGR [SetColor Foreground Vivid Green]
                putStr $ " cid=" ++ show (fav_id c)
                setSGR [Reset]
                putStrLn ""     -- [Reset] won't work without this
                )
    --putStrLn json
    
isMarked ch = do
    json <- readFile bookmarks
    case json of
         [] -> return False
         _ -> do
             let fav_ch = decodeJSON json :: [FavChannel]
             return $ ch `elem` fav_ch


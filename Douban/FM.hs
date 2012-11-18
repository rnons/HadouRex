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
import Control.Concurrent
import Control.Exception
import System.Console.ANSI
import Douban.State
import System.Exit

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

data Playlist = Playlist {
    r :: Int,
    song :: [Song] 
} deriving (Eq, Show, Data, Typeable)

-- filter out ads :-)
parseSong json = do
    let decoded = decode json :: Result (JSObject JSValue)
    let value = decoded >>= (valFromObj "song") :: Result [JSObject JSValue]
    let songs = (\(Ok x) -> x) value
    --let folding = foldr (\x acc -> if 
    let filterSong s = do
        let ssid = valFromObj "ssid" s :: Result JSValue
        case ssid of
             Ok _ -> True
             _  -> False
    let ss = filter filterSong songs
    decodeJSON $ decodeString $  encode ss :: [Song]

getPlaylist t = do
    channel_id <- getsST ccid
    let rdata = [("type", t),
                 ("channel", channel_id),
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
    --hin <- readMVar mhin
    --hout <- readMVar mhout
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hPutStrLn hin $ "LOAD " ++ url x
        hFlush hin
    silentlyModifyST $ \st -> st { cpicture     =   picture x,
                                   calbumtitle  =   albumtitle x,
                                   calbum       =   album x,
                                   cartist      =   artist x,
                                   ctitle       =   title x,
                                   csid         =   sid x,
                                   cssid        =   ssid x,
                                   caid         =   aid x
                                 }
    liftIO $ do
        withST $ \st -> do
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
    putStrLn "Inexhaustive channel lists:"
    chs <- readFile "channels"
    putStrLn chs
    selectChannel

listen [] = select
listen (cid:xs) = do
    putStrLn $ "Current channel id is: " ++ cid
    silentlyModifyST $ \st -> st { ccid = cid }
    forkIO $ getPlaylist "n"
    return ()

selectChannel = do
    putStrLn "Please enter the channel_id you want to listen to:"
    inpStr <- getLine
    -- default channel: "1001294 PostRock Odyssey"
    let channel_id = if inpStr `elem` ["","-3"] then "1001294" else inpStr
    silentlyModifyST $ \st -> st { ccid = channel_id }
    forkIO $ getPlaylist "n"
    return ()

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


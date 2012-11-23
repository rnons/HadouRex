module Douban.State where

import System.IO.Unsafe         (unsafePerformIO)
import System.IO                (Handle)

import Control.Concurrent       (ThreadId)
import Control.Concurrent.MVar
import Network.HTTP.Headers

data HState = HState {
    writeh          ::  MVar Handle,
    readh           ::  MVar Handle,
    status          ::  Bool,
    headers         ::  MVar Header,
    dbcl2           ::  String,
    bid             ::  String,
    st_context      ::  String,
    st_ch_id        ::  String,
    st_ch_name      ::  String,
    st_ch_intro     ::  String,
    st_ch_picture   ::  String,
    st_s_picture    ::  String,
    st_s_albumtitle ::  String,
    st_s_album      ::  String,
    st_s_like       ::  Int,
    st_s_artist     ::  String,
    st_s_title      ::  String,
    st_s_sid        ::  String,
    st_s_ssid       ::  String,
    st_s_aid        ::  String
}

--
-- | The initial state
--
emptySt :: HState
emptySt = HState {
    writeh          =   unsafePerformIO newEmptyMVar,
    readh           =   unsafePerformIO newEmptyMVar,
    status          =   True,
    headers         =   unsafePerformIO newEmptyMVar,
    dbcl2           =   "",
    bid             =   "",
    st_context      =   "",
    st_ch_id        =   "0",
    st_ch_name      =   "",
    st_ch_intro     =   "",
    st_ch_picture   =   "",
    st_s_picture    =   "",
    st_s_albumtitle =   "",
    st_s_album      =   "",
    st_s_like       =   0,
    st_s_artist     =   "",
    st_s_title      =   "",
    st_s_sid        =   "",
    st_s_ssid       =   "",
    st_s_aid        =   ""    
}

--
-- | A global variable holding the state. Todo StateT
--
state :: MVar HState
state = unsafePerformIO $ newMVar emptySt

------------------------------------------------------------------------
-- state accessor functions

-- | Access a component of the state with a projection function
getsST :: (HState -> a) -> IO a
getsST f = withST (return . f)

-- | Perform a (read-only) IO action on the state
withST :: (HState -> IO a) -> IO a
withST f = readMVar state >>= f

-- | Modify the state with a pure function
silentlyModifyST :: (HState -> HState) -> IO ()
silentlyModifyST  f = modifyMVar_ state (return . f)

------------------------------------------------------------------------

-- modifyST :: (HState -> HState) -> IO ()
-- modifyST f = silentlyModifyST f >> touchST

-- | Modify the state with an IO action, triggering a refresh
-- modifySTM :: (HState -> IO HState) -> IO ()
-- modifySTM f = modifyMVar_ state f >> touchST

-- | Modify the state with an IO action, returning a value
-- modifySTM_ :: (HState -> IO (HState,a)) -> IO a
-- modifySTM_ f = modifyMVar state f >>= \a -> touchST >> return a

-- | Trigger a refresh. This is the only way to update the screen
-- touchST :: IO ()
-- touchST = withMVar state $ \st -> tryPutMVar (modified st) () >> return ()


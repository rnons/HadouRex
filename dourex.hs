import Doubanfm
import Data.Global.Config (setConfig)

main = do
    setConfig $ Config 1001294
    getPlaylist


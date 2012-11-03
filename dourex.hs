import Doubanfm

main = do
    putStrLn "Inexhaustive channel lists:"
    chs <- readFile "channels"
    putStrLn chs
    selectChannel


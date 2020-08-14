module Main where

import Data.List.Split

main :: IO ()
main = do
    csvData <- readFile "data/data_set.csv"
    let my_players = (lines csvData)
    let avail = generatePlayerList my_players
    let tot = playerTotals avail
    let avg = playerAverages avail
    let std = playerStdDev pPTS (avail)
    let net = generateNetPlayerList [avg] avail
    let net_avg = netPlayerAverages net
    putStrLn "Run succesfully."


data Player = Player {
    pName :: String,
    pFGM :: Float,
    pFGA :: Float,
    pFTM :: Float,
    pFTA :: Float,
    p3PM :: Float,
    pREB :: Float,
    pAST :: Float,
    pSTL :: Float,
    pBLK :: Float,
    pTOV :: Float,
    pPTS :: Float
} deriving (Show)


generatePlayer :: String -> Player
generatePlayer playerString = case (splitOn "," playerString) of
    [name,pos,gms,mp,fgm,fga,fgp,tpm,ftm,fta,ftp,reb,ast,stl,blk,tov,pts] -> Player 
        name 
        (read fgm :: Float) 
        (read fga :: Float) 
        (read ftm :: Float) 
        (read fta :: Float) 
        (read tpm :: Float) 
        (read reb :: Float) 
        (read ast :: Float) 
        (read stl :: Float) 
        (read blk :: Float) 
        (read tov :: Float) 
        (read pts :: Float)
    _ -> Player "FAIL" 0 0 0 0 0 0 0 0 0 0 0


generatePlayerList :: [String] -> [Player]
generatePlayerList (x:xs) = fmap (generatePlayer) xs
generatePlayerList [] = []


playerTotals :: [Player] -> Player
playerTotals (x:xs) = foldl (\ a b -> Player 
        ("Totals") 
        ((pFGM a) + (pFGM b)) 
        ((pFGA a) + (pFGA b)) 
        ((pFTM a) + (pFTM b)) 
        ((pFTA a) + (pFTA b)) 
        ((p3PM a) + (p3PM b)) 
        ((pREB a) + (pREB b)) 
        ((pAST a) + (pAST b)) 
        ((pSTL a) + (pSTL b)) 
        ((pBLK a) + (pBLK b)) 
        ((pTOV a) + (pTOV b)) 
        ((pPTS a) + (pPTS b)) 
    ) x xs
playerTotals [] = Player "Totals" 0 0 0 0 0 0 0 0 0 0 0


playerAverages :: [Player] -> Player
playerAverages xs = (\ a -> Player 
        ("Averages")
        ((pFGM a) / len) 
        ((pFGA a) / len) 
        ((pFTM a) / len) 
        ((pFTA a) / len) 
        ((p3PM a) / len) 
        ((pREB a) / len) 
        ((pAST a) / len) 
        ((pSTL a) / len) 
        ((pBLK a) / len) 
        ((pTOV a) / len) 
        ((pPTS a) / len)
    ) (playerTotals xs) where len = (fromIntegral (length xs) :: Float)


playerStdDev :: (Player -> Float) -> [Player] -> Float
playerStdDev f xs = sqrt (summation / size) where 
    mean = f (playerAverages xs)
    size = fromIntegral (length xs) :: Float
    summation = sum (fmap sum_func xs)
    sum_func = (\ player -> ((f player) - mean) ^ 2 )


playerToStr :: Player -> String
playerToStr p = 
    (pName p) ++ " | " ++
    (show (pFGM p)) ++ " | " ++
    (show (pFGA p)) ++ " | " ++
    (show (pFTM p)) ++ " | " ++
    (show (pFTA p)) ++ " | " ++
    (show (p3PM p)) ++ " | " ++
    (show (pREB p)) ++ " | " ++
    (show (pAST p)) ++ " | " ++
    (show (pSTL p)) ++ " | " ++
    (show (pBLK p)) ++ " | " ++
    (show (pTOV p)) ++ " | "


playerListToStr :: [Player] -> String
playerListToStr list = case fmap playerToStr list of
    (x:xs) -> header ++ (foldl (\ a b -> a ++ "\n" ++ b) x xs) ++ "\n"
    [] -> "\nEmpty Player List\n"
    where header = "\nPLAYER NAME | FGM | FGA | FTM | FTA | 3PM | REB | AST | STL | BLK | TOV | PTS\n"


printPlayers :: IO ()
printPlayers = do
    csvData <- readFile "data/data_set.csv"
    let my_players = (lines csvData)
    let avail = generatePlayerList my_players
    putStrLn (playerListToStr avail)
    return ()


printPlayerAverages :: IO ()
printPlayerAverages = do
    csvData <- readFile "data/data_set.csv"
    let my_players = (lines csvData)
    let avail = generatePlayerList my_players
    let avg = playerAverages avail
    putStrLn (playerToStr avg)


data NetPlayer = NetPlayer {
    nName :: String,
    nFGP :: Float,
    nFTP :: Float,
    n3PM :: Float,
    nREB :: Float,
    nAST :: Float,
    nSTL :: Float,
    nBLK :: Float,
    nTOV :: Float,
    nPTS :: Float
} deriving (Show)


generateNetPlayer :: Player -> [Player] -> NetPlayer
generateNetPlayer player roster = NetPlayer 
    (pName player)
    (diffPercentageStat pFGM pFGA) 
    (diffPercentageStat pFTM pFTA) 
    (diffCountingStat p3PM) 
    (diffCountingStat pREB) 
    (diffCountingStat pAST) 
    (diffCountingStat pSTL) 
    (diffCountingStat pBLK) 
    (diffCountingStat pTOV) 
    (diffCountingStat pPTS)
    where 
        diffPercentageStat = (\ fm fa -> (fm with / fa with) - (fm without / fa without) )
        diffCountingStat = (\ f -> ((f with) / ((len roster)+1)) - ((f without) / (len roster)) )
        len = (\ x -> fromIntegral (length x) :: Float)
        with = playerTotals (player:roster)
        without = playerTotals roster


generateNetPlayerList :: [Player] -> [Player] -> [NetPlayer]
generateNetPlayerList roster avail = [ generateNetPlayer p roster | p <- avail]


netPlayerTotals :: [NetPlayer] -> NetPlayer
netPlayerTotals (x:xs) = foldl (\ a b -> NetPlayer 
        ("Totals") 
        ((nFGP a) + (nFGP b)) 
        ((nFTP a) + (nFTP b))
        ((n3PM a) + (n3PM b)) 
        ((nREB a) + (nREB b)) 
        ((nAST a) + (nAST b)) 
        ((nSTL a) + (nSTL b)) 
        ((nBLK a) + (nBLK b)) 
        ((nTOV a) + (nTOV b)) 
        ((nPTS a) + (nPTS b)) 
    ) x xs
netPlayerTotals [] = NetPlayer "Totals" 0 0 0 0 0 0 0 0 0


netPlayerAverages :: [NetPlayer] -> NetPlayer
netPlayerAverages xs = (\ a -> NetPlayer 
        ("Averages")
        ((nFGP a) / len)
        ((nFTP a) / len)
        ((n3PM a) / len) 
        ((nREB a) / len) 
        ((nAST a) / len) 
        ((nSTL a) / len) 
        ((nBLK a) / len) 
        ((nTOV a) / len) 
        ((nPTS a) / len)
    ) (netPlayerTotals xs) where len = (fromIntegral (length xs) :: Float)


-- netPlayerStdDev :: (NetPlayer -> Float) -> [NetPlayer] -> Float
-- netPlayerStdDev f xs = sqrt (summation / size) where 
--     mean = f (netPlayerAverages xs)
--     size = fromIntegral (length xs) :: Float
--     summation = sum (fmap sum_func xs)
--     sum_func = (\ player -> ((f player) - mean) ^ 2 )


netPlayerToStr :: NetPlayer -> String
netPlayerToStr p = 
    (nName p) ++ " | " ++
    (show (nFGP p)) ++ " | " ++
    (show (nFTP p)) ++ " | " ++
    (show (n3PM p)) ++ " | " ++
    (show (nREB p)) ++ " | " ++
    (show (nAST p)) ++ " | " ++
    (show (nSTL p)) ++ " | " ++
    (show (nBLK p)) ++ " | " ++
    (show (nTOV p)) ++ " | "


netPlayerListToStr :: [NetPlayer] -> String
netPlayerListToStr list = case fmap netPlayerToStr list of
    (x:xs) -> header ++ (foldl (\ a b -> a ++ "\n" ++ b) x xs) ++ "\n"
    [] -> "\nEmpty Net Player List\n"
    where header = "\nNET PLAYER NAME | FGP | FTP | 3PM | REB | AST | STL | BLK | TOV | PTS\n"


printNetPlayers :: IO ()
printNetPlayers = do
    csvData <- readFile "data/data_set.csv"
    let my_players = (lines csvData)
    let avail = generatePlayerList my_players
    let avg = playerAverages avail
    let net = generateNetPlayerList [avg] avail
    putStrLn (netPlayerListToStr net)
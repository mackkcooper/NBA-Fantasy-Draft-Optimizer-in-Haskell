module Main where

import Data.List.Split

main :: IO ()
main = do
    csvData <- readFile "data/data_set.csv"
    let my_players = (lines csvData)

    let avail = generatePlayerList my_players
    -- print ("Players")
    -- print avail

    -- let tot = totals avail
    -- print ("Totals:")
    -- print tot

    let avg = averages avail
    -- print ("Averages:")
    -- print avg

    -- let std = playerStdDev pPTS (avail)
    -- print ("Std Devs:")
    -- print std

    let net = generateNetPlayerList [avg] avail
    -- print ("Net Stats:")
    -- print net

    putStrLn "Done."


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


averages :: [Player] -> Player
averages xs = (\ a -> Player 
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
    ) (totals xs) where len = (fromIntegral (length xs) :: Float)


totals :: [Player] -> Player
totals (x:xs) = foldl (\ a b -> Player 
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
totals [] = Player "Totals" 0 0 0 0 0 0 0 0 0 0 0


playerStdDev :: (Player -> Float) -> [Player] -> Float
playerStdDev f xs = sqrt (summation / size) where 
    mean = f (averages xs)
    size = fromIntegral (length xs) :: Float
    summation = sum (fmap sum_func xs)
    sum_func = (\ player -> ((f player) - mean) ^ 2 )


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
        with = totals (player:roster)
        without = totals roster


generateNetPlayerList :: [Player] -> [Player] -> [NetPlayer]
generateNetPlayerList roster avail = [ generateNetPlayer p roster | p <- avail]

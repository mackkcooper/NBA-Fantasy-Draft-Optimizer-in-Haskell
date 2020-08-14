module Main where

import Data.List.Split

main :: IO ()
main = do
    csvData <- readFile "data/data_set.csv"
    let my_players = (lines csvData)

    print ("Totals:")
    print (totals (generatePlayerList my_players))
    print ("Averages:")
    print (averages (generatePlayerList my_players))
    
    putStrLn "Done."

generatePlayerList :: [String] -> [Player]
generatePlayerList xs = fmap (generatePlayer) (tail xs)

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

-- generateNetPlayer :: Player -> [Player] -> NetPlayer
-- generateNetPlayer player roster :: NetPlayer ()

averages :: [Player] -> Player
averages xs = (\ a -> Player 
        ("Averages")
        ((pFGM a) / (fromIntegral (length xs) :: Float)) 
        ((pFGA a) / (fromIntegral (length xs) :: Float)) 
        ((pFTM a) / (fromIntegral (length xs) :: Float)) 
        ((pFTA a) / (fromIntegral (length xs) :: Float)) 
        ((p3PM a) / (fromIntegral (length xs) :: Float)) 
        ((pREB a) / (fromIntegral (length xs) :: Float)) 
        ((pAST a) / (fromIntegral (length xs) :: Float)) 
        ((pSTL a) / (fromIntegral (length xs) :: Float)) 
        ((pBLK a) / (fromIntegral (length xs) :: Float)) 
        ((pTOV a) / (fromIntegral (length xs) :: Float)) 
        ((pPTS a) / (fromIntegral (length xs) :: Float))
    ) (totals xs) 


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

stdDev :: (Player -> Float) -> [Player] -> Float
stdDev f (x:xs) = (f (averages (x:xs)))

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
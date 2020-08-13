module Main where

import Data.List.Split

main :: IO ()
main = do
    csvData <- readFile "data/data_set.csv"
    -- let avail = lines csvData
    -- fmap putStrLn avail
    let my_players = (lines csvData)
    print (generatePlayerList my_players)
    putStrLn "Done."

generatePlayerList :: [String] -> [Player]
generatePlayerList xs = fmap (generatePlayer) (tail xs)

generatePlayer :: String -> Player
generatePlayer playerString = case (splitOn "," playerString) of
    [name,pos,gms,mp,fgm,fga,fgp,tpm,ftm,fta,ftp,reb,ast,stl,blk,tov,pts] -> Player name (read fgm :: Float) (read fga :: Float) (read ftm :: Float) (read fta :: Float) (read tpm :: Float) (read reb :: Float) (read ast :: Float) (read stl :: Float) (read blk :: Float) (read tov :: Float) (read pts :: Float)

-- generateNetPlayer :: Player -> [Player] -> NetPlayer
-- generateNetPlayer player roster :: 

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
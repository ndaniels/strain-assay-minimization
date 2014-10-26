module Main
where
  
import Barcode
import System.Environment  
import qualified Data.Vector as V
import qualified Data.List as DL
  
main :: IO ()
main = do
  -- get input arg
  [inFile, maxSnps'] <- getArgs >>= return . take 2
  let maxSnps = read maxSnps' :: Int
  samples <- parseBarcode inFile
  let samples' = uniquify $ V.toList $ V.filter validBarcode samples
  putStrLn $ show $ DL.sort $ map (+1) $ take maxSnps $ chooseSnps samples' [0..23]


  
-- :load Barcode
-- samples <- parseBarcode "2013-test.csv"
-- let samples' = uniquify $ V.toList $ V.filter validBarcode samples
-- let ids = map (\x -> x - 1) [1, 9, 10, 11, 12, 14, 15, 18]
-- putStrLn $ unlines $ reportDuplicates samples' ids
-- let ids = map (\x -> x - 1) [19,15,6,4,3,20,5,12,8,13,14,22,18,17,7,16,10,11,21,2]
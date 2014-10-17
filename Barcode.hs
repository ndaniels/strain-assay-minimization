module Barcode
  (parseBarcode
  ,validBarcode
  ,Barcode
  ,chooseSnps
  ,uniquify
  ,reportDuplicates
  )
where
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Char -- ord
import Data.Ord
import Data.Function (on)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import qualified Data.Traversable as T
import qualified Data.Csv.Streaming as S
import qualified Data.HashMap as H

import Debug.Trace (trace)
  
type BarcodeList = V.Vector Barcode
  
data Barcode = Barcode
    { name    :: !String
    , snps    :: V.Vector Char
    }
    deriving Show

instance FromRecord Barcode where
     parseRecord v
         | V.length v == 25 = Barcode <$>
                             v .! 0  <*>
                             T.traverse parseField (V.tail v)
         | otherwise        = mzero
         
parseBarcode :: String -> IO BarcodeList
parseBarcode f = do
    csvData <- BL.readFile f
    case decode NoHeader csvData of
        Left err    -> putStrLn err >> return empty
        Right val   -> return val
        
valAt :: Barcode -> Int -> Char
valAt b i = (snps b) V.! i

validBarcode :: Barcode -> Bool
validBarcode = check . snps 
             where check b = ns <= 1 && xs + ns <= 5
                           where ns = count 'N' b
                                 xs = count 'X' b
                                 count x q = V.foldl' (count' x) 0 q
                                 count' x' acc y = if x' == y
                                                 then (1+acc)
                                                 else (acc)

subBarcode :: Barcode -> [Int] -> [Char]
subBarcode b (i:is) = valAt b i : subBarcode b is
subBarcode b []     = []

subBarcodeWith :: Barcode -> [Int] -> Int -> [Char]
subBarcodeWith b is k = subBarcode b (k:is)

ambigAt :: Barcode -> Int -> Bool
ambigAt b i = valAt b i == 'N' || valAt b i == 'X'

ambigAny :: [Barcode] -> Int -> Bool
ambigAny (b:bs) i = ambigAt b i || ambigAny bs i
ambigAny []     i = False

reportDuplicates :: [Barcode] -> [Int] -> [String]
reportDuplicates bss@(b:bs) is = display h bss is
                               where h = (clust (H.singleton (subBarcode b is) [b]) bs)
                                     clust :: H.Map [Char] [Barcode] -> 
                                             [Barcode]         -> 
                                             H.Map [Char] [Barcode]
                                     clust h (b':bs') = clust (H.insertWith
                                                        (\n o -> (n++o))
                                                        (subBarcode b' is)
                                                        [b']
                                                        h) bs'
                                     clust h []       = h
                                     display h bs is = dSamples is $ H.elems $ H.filter (\l -> length l > 1) h
                                     -- print every name and its subBarcode
                                     dSamples is (bs:bss) = disp is bs : dSamples is bss
                                     dSamples _      []   = []
                                     disp is (b:bs) = (name b ++ ", " ++ subBarcode b is ++ "\n") ++ (disp is bs)
                                     disp _ [] = "\n"
                                          

-- given a set of barcodes, and a SNP, how evenly does it split the
-- set of barcodes? YES, this is what we want.

-- cost is the average bucket size, + 1 if THIS snp is ambiguous in any barcode
-- so we can use `elems` from Data.Hashmap. This gives a list of elements.
-- Each element will be a Barcode, so we can just compute an average element
-- size, and add 1 if `ambigAny barcodes i` is true.

cost :: [Barcode] -> [Int] -> Int -> Double
cost bss@(b:bs) is i = ambigCost bss is i h +
                       avgSize h 
                     where h = (clust (H.singleton (subBarcodeWith b is i) 1) bs)
                           clust :: H.Map [Char] Double -> [Barcode] -> H.Map [Char] Double
                           clust h (b':bs') = clust (H.insertWith
                                              (\n o -> o + 1.0)
                                              (subBarcodeWith b' is i)
                                              1.0
                                              h) bs'
                           clust h []     = h
   

avgSize :: H.Map [Char] Double -> Double
avgSize h = Prelude.foldr (+) 0.0 es / (fromIntegral $ length es)
             where es = H.elems h

-- might need a function from H.Map [Char] Double -> Int
-- for any Barcode that's ambiguous at that index,
-- how big is its hash bucket?
-- its hash bucket is subBarcodeWith b is i
-- so we could look over all Barcodes
-- for each one, 

-- If this snp is only ambiguous for already-singleton strains,
-- then it's fine.
ambigCost :: [Barcode] -> [Int] -> Int -> H.Map [Char] Double -> Double
ambigCost bs is i h = if shouldPenalize bs is i h
                    then 1.0
                    else 0.0

-- still not right at all.
-- I impose that cost ONLY IF the hash bucket for some barcode is > 1.0 AND
-- this position is ambiguous in that barcode.
-- so look through the barcodes. For any one that's ambigAt i, 
-- if its hash bucket is > 1.0, then ambigCost is 1.0

shouldPenalize :: [Barcode] -> [Int] -> Int -> H.Map [Char] Double -> Bool
shouldPenalize (b:bs) is i h = (ambigAt b i && H.findWithDefault 1.0
                                   (subBarcodeWith b is i) h > 1.0)
                               || shouldPenalize bs is i h
shouldPenalize  []     _ _ _ = False                               
                               

choose :: [Barcode] -> [Int] -> [Int] -> Int
choose bs is cs = L.minimumBy (compare `on` (cost bs cs)) is



-- if cost of all the chosen is 1.0, then we just return them
-- else, choose the best one from CANDIDATES, removing it from candidates,
-- and recurse.
-- if candidates is empty, return chosen

-- take a list of Barcodes and a list of Snp indices.
-- return the set of Snp indices to use
chooseSnps :: [Barcode] -> [Int] -> [Int]
chooseSnps bs is = chooseSnps' bs is []

chooseSnps' :: [Barcode] -> [Int] -> [Int] -> [Int]
chooseSnps' bs is css@(c:cs) = if cost bs cs c == 1.0
                            then css
                            else (chooseSnps' bs (dropWhile (== choice) is) (choice:css))
                            where choice = choose bs is css 
chooseSnps' bs []         cs = cs
chooseSnps' bs is         cs = chooseSnps' bs (dropWhile (== choice) is) (choice:cs)
                            where choice = choose bs is cs 



uniquify :: [Barcode] -> [Barcode]
uniquify bs = L.nubBy checkBarcode bs
            where checkBarcode x y = (snps x == snps y)
                                      

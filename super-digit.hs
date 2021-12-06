
Bad code....

module Main
  where
  
import Control.Monad ( join )
import Data.Char (digitToInt)
import System.Environment
import Data.List
import System.IO

sumDigits :: Int -> Int 
sumDigits res 
    | (length $ show $ res) == 1 = res
    | otherwise  = sumDigits (foldr (\chac acc -> acc+digitToInt chac) 0 $ show res)

superDigit :: Integer -> Integer  -> Int  
superDigit n k =
    let stringDigit = show n                                                            // Bad happends here --- it creates a huge replica
        replicateAndSumDigit1sRes =  foldr (\chac acc -> acc+ digitToInt chac) 0 $ join $ replicate (fromIntegral k) stringDigit
      in sumDigits replicateAndSumDigit1sRes


toI  x =  read x :: Integer

main :: IO ()
main = do
  arg <- getLine
  let wds = words arg
  print $ superDigit ( toI (head wds)) (toI (last wds))
  
  
  
Good code .....


module Main
  where
  
import Control.Monad ( join )
import Data.Char (digitToInt)
import System.Environment
import Data.List
import System.IO

sumDigits :: String  -> String 
sumDigits res 
    | (length res) == 1 = res
    | otherwise  = sumDigits ( show $ foldr (\chac acc -> acc+digitToInt chac) 0  res)

superDigit :: Integer -> Integer  -> Int   
superDigit n k =
    let stringDigit = show n
        replicateAndSumDigit1sRes =  sumDigits $ join $ replicate (fromIntegral k) (sumDigits stringDigit)
      in  read replicateAndSumDigit1sRes 
      
      
toI  x =  read x :: Integer

main :: IO ()
main = do
  arg <- getLine
  let wds = words arg
  print $ superDigit ( toI (head wds)) (toI (last wds))
  
  
  
  


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
    let stringDigit = show n
        replicateAndSumDigit1sRes =  foldr (\chac acc -> acc+ digitToInt chac) 0 $ join $ replicate (fromIntegral k) stringDigit
      in sumDigits replicateAndSumDigit1sRes


toI  x =  read x :: Integer

main :: IO ()
main = do
  arg <- getLine
  let wds = words arg
  print $ superDigit ( toI (head wds)) (toI (last wds))
  
  

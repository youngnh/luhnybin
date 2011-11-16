import Control.Monad (forever)
import Data.Char (isDigit, digitToInt)

digitSum n
    | n >= 20   = undefined
    | n >= 10   = 1 + (n - 10)
    | otherwise = n

luhnChecksum xs = sum
    where (sum,_) = foldr luhn (0,0) xs
          luhn x (s,n) = if even n
                         then ((x + s) `mod` 10, n + 1)
                         else (((digitSum $ 2 * x) + s) `mod` 10, n + 1)

readLine line = if and [all isDigit line,
                        lengthBetween 14 16 line]
                then Just $ map digitToInt line
                else Nothing
    where lengthBetween lo hi x
              | length x >= lo && length x <= hi = True
              | otherwise                        = False

redactLine :: IO ()
redactLine = do
  line <- getLine
  putStrLn $ case readLine line of
               Just digits -> if (luhnChecksum digits) == 0
                              then replicate (length line) 'X'
                              else line
               Nothing     -> line

endProgram :: IOError -> IO ()
endProgram _ = putStrLn ""

main :: IO ()
main = do
  forever redactLine `catch` endProgram
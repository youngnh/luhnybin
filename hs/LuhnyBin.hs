import Control.Monad (forever, msum)
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

takeCC 0 cs = Just ([],0)
takeCC _ [] = Nothing
takeCC n (c:cs)
    | isDigit c = case takeCC (n - 1) cs of
                    Just (xs, end) -> Just (x:xs, end+1)
                    Nothing -> Nothing
    | c == ' ' || c == '-' = case takeCC n cs of
                               Just (xs, end) -> Just (xs, end+1)
                               Nothing -> Nothing
    | otherwise = Nothing
    where x = digitToInt c

ccNum input = msum [checkCC 16, checkCC 15, checkCC 14]
    where checkCC n = case takeCC n input of
                        Just (digits, end) -> if luhnChecksum digits == 0
                                              then Just end
                                              else Nothing
                        Nothing            -> Nothing

ccWindow [] = Nothing
ccWindow input = case ccEnd input of
                   Just end -> Just (0,end)
                   Nothing  -> case ccWindow (tail input) of
                                 Just (s,e) -> Just (s+1,e+1)
                                 Nothing    -> Nothing
    where ccEnd s = case ccNum s of
                      Just end -> case ccEnd (tail s) of
                                    Just end' -> Just $ max end (end' + 1)
                                    Nothing   -> Just end
                      Nothing  -> Nothing

between xs start end = take (end - start) $ drop start xs

redact line = case ccWindow line of
                Just (start, end) -> prefix ++ redacted ++ (redact suffix)
                    where prefix = take start line
                          suffix = drop end line
                          middle = take (end - start) $ drop start line
                          redact' c = if isDigit c
                                      then 'X'
                                      else c
                          redacted = map redact' middle
                Nothing -> line

redactLine :: IO ()
redactLine = do
  line <- getLine
  putStrLn $ redact line

endProgram :: IOError -> IO ()
endProgram _ = putStrLn ""

main :: IO ()
main = do
  forever redactLine `catch` endProgram
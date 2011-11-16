import Control.Monad (forever)

main :: IO ()
main = do
  forever $ getLine >>= putStrLn
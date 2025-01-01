module App where

import Args (Args, parseArgs)
import Options.Applicative (handleParseResult)
import System.Environment (getArgs)

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO ()
program' _ = do
  let santa = "🎅"
      reindeer = "🦌"
      elf = "🧝"
  putStrLn $ "Santa: " ++ santa
  putStrLn $ "Reindeer: " ++ reindeer
  putStrLn $ "Elf: " ++ elf

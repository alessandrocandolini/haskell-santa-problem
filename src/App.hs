module App where

import Args (Args (..), Command (..), ElfsRoomSize (..), NumberOfElfs (..), NumberOfReinders (..), NumberOfThreads (..), SimulationArgs (..), parseArgs)
import Options.Applicative (handleParseResult)
import System.Environment (getArgs)

santa, reindeer, elf :: String
santa = "🎅"
reindeer = "🦌"
elf = "🧝"

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO ()
program' (Args _ Doctor) = doctor
program' (Args _ (Simulate args)) = simulate args

simulate :: SimulationArgs -> IO ()
simulate (SimulationArgs (NumberOfThreads n) (NumberOfElfs e) (NumberOfReinders r) (ElfsRoomSize g)) = do
  putStrLn $ "Threads: " ++ show n
  putStrLn $ "Elf room size: " ++ show g
  putStrLn $ "Santa: " ++ santa
  putStrLn $ "Reindeers [" ++ show r ++ "]: " ++ concat (replicate r reindeer)
  putStrLn $ "Elfs [" ++ show e ++ "]: " ++ concat (replicate e elf)

doctor :: IO ()
doctor = putStrLn "welcome to Santa simulator"

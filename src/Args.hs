{-# LANGUAGE DerivingVia #-}

module Args where

import Data.Maybe (fromMaybe)
import GHC.Base (when)
import Options.Applicative (
  Mod,
  Parser,
  ParserInfo,
  ParserResult,
  ReadM,
  auto,
  command,
  disambiguate,
  execParserPure,
  flag,
  fullDesc,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  short,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  value,
 )
import Options.Applicative.Builder (OptionFields)

data Args = Args GlobalFlags Command deriving (Eq, Show)

newtype NumberOfThreads = NumberOfThreads Int deriving (Eq, Show, Read, Ord, Num) via Int
newtype NumberOfElfs = NumberOfElfs Int deriving (Eq, Show, Read, Ord, Num) via Int
newtype NumberOfReinders = NumberOfReinders Int deriving (Eq, Show, Read, Ord, Num) via Int
newtype ElfsRoomSize = ElfsRoomSize Int deriving (Eq, Show, Read, Ord, Num) via Int

data SimulationArgs = SimulationArgs NumberOfThreads NumberOfElfs NumberOfReinders ElfsRoomSize deriving (Eq, Show)

data Command
  = Doctor
  | Simulate SimulationArgs
  deriving (Eq, Show)

data Verbosity = Verbose | NotVerbose deriving (Eq, Show)

newtype GlobalFlags = GlobalFlags Verbosity deriving (Eq, Show)

argsParser :: Parser Args
argsParser = Args <$> globalFlagsParser <*> commands

range :: (Ord a, Show a) => a -> a -> a -> ReadM a
range from to n = do
  when invalid (fail message)
  pure n
 where
  invalid = n < from || n >= to
  message = "value " ++ show n ++ " out of range! Must be " ++ show from ++ " <= " ++ show n ++ " < " ++ show to

optionRange :: (Read a, Ord a, Show a) => a -> a -> Mod OptionFields a -> Parser a
optionRange from to = option (auto >>= range from to)

helpWithRange :: (Show a) => String -> a -> a -> Mod f a
helpWithRange message from to = help (unwords [message, extra])
 where
  extra = "Value must be in range [" ++ show from ++ ", " ++ show to ++ "[."

numberOfThreadParser :: Parser NumberOfThreads
numberOfThreadParser =
  NumberOfThreads
    <$> optionRange
      1
      100
      ( long "threads"
          <> short 'n'
          <> value 10
          <> helpWithRange "number of threads" 1 100
          <> metavar "INT"
          <> showDefault
      )

numberOfElfsParser :: Parser NumberOfElfs
numberOfElfsParser =
  NumberOfElfs
    <$> optionRange
      1
      1000
      ( long "elfs"
          <> short 'e'
          <> value 20
          <> helpWithRange "number of elfs" 1 1000
          <> metavar "INT"
          <> showDefault
      )

numberOfReindersParser :: Parser NumberOfReinders
numberOfReindersParser =
  NumberOfReinders
    <$> optionRange
      1
      20
      ( long "reinders"
          <> short 'r'
          <> value 9
          <> helpWithRange "number of reinders" 1 20
          <> metavar "INT"
          <> showDefault
      )

elfsRoomSizeParser :: Parser ElfsRoomSize
elfsRoomSizeParser =
  ElfsRoomSize
    <$> optionRange
      1
      20
      ( long "room"
          <> short 'g'
          <> value 3
          <> helpWithRange "elf room size" 1 20
          <> metavar "INT"
          <> showDefault
      )

simulationArgsParser :: Parser SimulationArgs
simulationArgsParser =
  SimulationArgs
    <$> numberOfThreadParser
    <*> numberOfElfsParser
    <*> numberOfReindersParser
    <*> elfsRoomSizeParser

simulateParser :: Parser Command
simulateParser = Simulate <$> simulationArgsParser

verbosity :: Parser Verbosity
verbosity =
  flag
    NotVerbose
    Verbose
    (long "verbose" <> short 'v' <> help "Enable verbose output")

globalFlagsParser :: Parser GlobalFlags
globalFlagsParser = GlobalFlags <$> verbosity

commands :: Parser Command
commands =
  hsubparser
    ( command "doctor" (info (pure Doctor) (progDesc "check the CLI is working"))
        <> command "simulate" (info simulateParser (progDesc "run the simulation"))
    )

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) (fullDesc <> progDesc s)

parseArgs :: [String] -> ParserResult Args
parseArgs = execParserPure preferences parserInfo
 where
  parserInfo = withInfo argsParser "Santa Clause Concurrency Problem Simulator"
  preferences = prefs (disambiguate <> showHelpOnEmpty <> showHelpOnError)
